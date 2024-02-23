{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Peter Vreman

    Stabs Line Info Retriever

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This unit should not be compiled in objfpc mode, since this would make it
  dependent on objpas unit.
}
unit CustomLineInfo;
interface

{$S-}
{$Q-}

function GetLineInfo(addr:ptruint;var func,source:shortstring;var line:longint) : boolean;
function StabBackTraceStr(addr:Pointer):shortstring;

implementation

uses
  exeinfo,strings;

const
  N_Function    = $24;
  N_TextLine    = $44;
  N_DataLine    = $46;
  N_BssLine     = $48;
  N_SourceFile  = $64;
  N_IncludeFile = $84;

  maxstabs = 40; { size of the stabs buffer }

var
  { GDB after 4.18 uses offset to function begin
    in text section but OS/2 version still uses 4.16 PM }
  StabsFunctionRelative: boolean;

type
  pstab=^tstab;
  tstab=packed record
    strpos  : longint;
    ntype   : byte;
    nother  : byte;
    ndesc   : word;
    nvalue  : dword;
  end;

{ We use static variable so almost no stack is required, and is thus
  more safe when an error has occured in the program }
var
  e          : TExeFile;
  stabcnt,              { amount of stabs }
  stablen,
  stabofs,              { absolute stab section offset in executable }
  stabstrlen,
  stabstrofs : longint; { absolute stabstr section offset in executable }
  dirlength  : longint; { length of the dirctory part of the source file }
  stabs      : array[0..maxstabs-1] of tstab;  { buffer }
  funcstab,             { stab with current function info }
  linestab,             { stab with current line info }
  dirstab,              { stab with current directory info }
  filestab   : tstab;   { stab with current file info }
  filename: {$ifdef FPC_FULLVERSION >= 30301}ansistring{$else}shortstring{$endif};
  dbgfn : string;


var
  Crc32Tbl : array[0..255] of cardinal;

procedure MakeCRC32Tbl;
var
  crc : cardinal;
  i,n : integer;
begin
  for i:=0 to 255 do
   begin
     crc:=i;
     for n:=1 to 8 do
      if (crc and 1)<>0 then
       crc:=(crc shr 1) xor cardinal($edb88320)
      else
       crc:=crc shr 1;
     Crc32Tbl[i]:=crc;
   end;
end;

Function UpdateCrc32(InitCrc:cardinal;const InBuf;InLen:LongInt):cardinal;
var
  i : LongInt;
  p : pchar;
begin

  if Crc32Tbl[1]=0 then
   MakeCrc32Tbl;
  p:=@InBuf;
  Result:=not InitCrc;
  for i:=1 to InLen do
   begin
     UpdateCrc32:=Crc32Tbl[byte(Result) xor byte(p^)] xor (Result shr 8);
     inc(p);
   end;
  Result:=not Result;
end;

function CheckDbgFile(var e:TExeFile;const fn:string;dbgcrc:cardinal):boolean;
var
  c      : cardinal;
  ofm    : word;
  g      : file;
begin
  CheckDbgFile:=false;
  assign(g,fn);
  {$I-}
   ofm:=filemode;
   filemode:=$40;
   reset(g,1);
   filemode:=ofm;
  {$I+}
  if ioresult<>0 then
   exit;
  { We reuse the buffer from e here to prevent too much stack allocation }
  c:=0;
  repeat
    blockread(g,e.buf,e.bufsize,e.bufcnt);
    c:=UpdateCrc32(c,e.buf,e.bufcnt);
  until e.bufcnt<e.bufsize;
  close(g);
  CheckDbgFile:=(dbgcrc=c);
end;

function ReadDebugLink(var e:TExeFile;var dbgfn:string):boolean;
var
  dbglink : array[0..512] of char;
  i,
  dbglinklen,
  dbglinkofs : longint;
  dbgcrc     : cardinal;
begin
  ReadDebugLink:=false;
  dbglinkofs := 0;
  dbglinklen := 0;
  dbgcrc := 0;
  if not FindExeSection(e,'.gnu_debuglink',dbglinkofs,dbglinklen) then
    exit;
  if dbglinklen>sizeof(dbglink)-1 then
    exit;
  fillchar(dbglink,sizeof(dbglink),0);
  seek(e.f,dbglinkofs);
  blockread(e.f,dbglink,dbglinklen);
  dbgfn:=strpas(dbglink);
  if length(dbgfn)=0 then
    exit;
  i:=align(length(dbgfn)+1,4);
  if (i+4)>dbglinklen then
    exit;
  move(dbglink[i],dbgcrc,4);
  { current dir }
  if CheckDbgFile(e,dbgfn,dbgcrc) then
    begin
      ReadDebugLink:=true;
      exit;
    end;
  { executable dir }
  i:=length(e.filename);
  while (i>0) and not(e.filename[i] in AllowDirectorySeparators) do
    dec(i);
  if i>0 then
    begin
      dbgfn:=copy(e.filename,1,i)+dbgfn;
      if CheckDbgFile(e,dbgfn,dbgcrc) then
        begin
          ReadDebugLink:=true;
          exit;
        end;
    end;
end;

function OpenStabs(addr : pointer) : boolean;
var
  baseaddr : pointer;
begin
  OpenStabs:=false;
  baseaddr := nil;
  GetModuleByAddr(addr,baseaddr,filename);
{$ifdef DEBUG_LINEINFO}
  writeln(stderr,filename,' Baseaddr: ',hexstr(ptruint(baseaddr),sizeof(baseaddr)*2));
{$endif DEBUG_LINEINFO}

  if not OpenExeFile(e,filename) then
    exit;
  if ReadDebugLink(e,dbgfn) then
    begin
      CloseExeFile(e);
      if not OpenExeFile(e,dbgfn) then
        exit;
    end;
  if ptruint(BaseAddr) < e.processaddress then Exit;

  e.processaddress := ptruint(baseaddr) - e.processaddress;
  StabsFunctionRelative := E.FunctionRelative;
  if FindExeSection(e,'.stab',stabofs,stablen) and
     FindExeSection(e,'.stabstr',stabstrofs,stabstrlen) then
    begin
      stabcnt:=stablen div sizeof(tstab);
      OpenStabs:=true;
    end
  else
    begin
      CloseExeFile(e);
      exit;
    end;
end;

procedure CloseStabs;
begin
  CloseExeFile(e);
end;

function GetLineInfo(addr:ptruint;var func,source:shortstring;var line:longint) : boolean;
var
  res,
  stabsleft,
  stabscnt,i : longint;
  found : boolean;
  lastfunc : tstab;
begin
  GetLineInfo:=false;
{$ifdef DEBUG_LINEINFO}
  writeln(stderr,'GetLineInfo called');
{$endif DEBUG_LINEINFO}
  fillchar(func,high(func)+1,0);
  fillchar(source,high(source)+1,0);
  line:=0;
  if not e.isopen then
   begin
     if not OpenStabs(pointer(addr)) then
      exit;
   end;

  { correct the value to the correct address in the file }
  { processaddress is set in OpenStabs                   }
  addr := dword(addr - e.processaddress);

{$ifdef DEBUG_LINEINFO}
  writeln(stderr,'Addr: ',hexstr(addr,sizeof(addr)*2));
{$endif DEBUG_LINEINFO}

  fillchar(funcstab,sizeof(tstab),0);
  fillchar(filestab,sizeof(tstab),0);
  fillchar(dirstab,sizeof(tstab),0);
  fillchar(linestab,sizeof(tstab),0);
  fillchar(lastfunc,sizeof(tstab),0);
  found:=false;
  seek(e.f,stabofs);
  stabsleft:=stabcnt;
  repeat
    if stabsleft>maxstabs then
     stabscnt:=maxstabs
    else
     stabscnt:=stabsleft;
    blockread(e.f,stabs,stabscnt*sizeof(tstab),res);
    stabscnt:=res div sizeof(tstab);
    for i:=0 to stabscnt-1 do
     begin
       case stabs[i].ntype of
         N_BssLine,
         N_DataLine,
         N_TextLine :
           begin
             if (stabs[i].ntype=N_TextLine) and StabsFunctionRelative then
               inc(stabs[i].nvalue,lastfunc.nvalue);
             if (stabs[i].nvalue<=addr) and
                (stabs[i].nvalue>linestab.nvalue) then
              begin
                { if it's equal we can stop and take the last info }
                if stabs[i].nvalue=addr then
                 found:=true
                else
                 linestab:=stabs[i];
              end;
           end;
         N_Function :
           begin
             lastfunc:=stabs[i];
             if (stabs[i].nvalue<=addr) and
                (stabs[i].nvalue>funcstab.nvalue) then
              begin
                funcstab:=stabs[i];
                fillchar(linestab,sizeof(tstab),0);
              end;
           end;
         N_SourceFile,
         N_IncludeFile :
           begin
             if (stabs[i].nvalue<=addr) and
                (stabs[i].nvalue>=filestab.nvalue) then
              begin
                { if same value and type then the first one
                  contained the directory PM }
                if (stabs[i].nvalue=filestab.nvalue) and
                   (stabs[i].ntype=filestab.ntype) then
                  dirstab:=filestab
                else
                  fillchar(dirstab,sizeof(tstab),0);
                filestab:=stabs[i];
                fillchar(linestab,sizeof(tstab),0);
                { if new file then func is not valid anymore PM }
                if stabs[i].ntype=N_SourceFile then
                  begin
                    fillchar(funcstab,sizeof(tstab),0);
                    fillchar(lastfunc,sizeof(tstab),0);
                  end;
              end;
           end;
       end;
     end;
    dec(stabsleft,stabscnt);
  until found or (stabsleft=0);

{ get the line,source,function info }
  line:=linestab.ndesc;
  if dirstab.ntype<>0 then
   begin
     seek(e.f,stabstrofs+dirstab.strpos);
     blockread(e.f,source[1],high(source)-1,res);
     dirlength:=strlen(@source[1]);
     source[0]:=chr(dirlength);
   end
  else
   dirlength:=0;
  if filestab.ntype<>0 then
   begin
     seek(e.f,stabstrofs+filestab.strpos);
     blockread(e.f,source[dirlength+1],high(source)-(dirlength+1),res);
     source[0]:=chr(strlen(@source[1]));
   end;
  if funcstab.ntype<>0 then
   begin
     seek(e.f,stabstrofs+funcstab.strpos);
     blockread(e.f,func[1],high(func)-1,res);
     func[0]:=chr(strlen(@func[1]));
     i:=pos(':',func);
     if i>0 then
      Delete(func,i,255);
   end;
//  if e.isopen then
//    CloseStabs;
  GetLineInfo:=true;
end;

function StabBackTraceStr(addr:Pointer):shortstring;
var
  func,
  source : shortstring;
  hs     : string[32];
  line   : longint;
  Store  : TBackTraceStrFunc;
  Success : boolean;
begin
{$ifdef DEBUG_LINEINFO}
  writeln(stderr,'StabBackTraceStr called');
{$endif DEBUG_LINEINFO}
  { reset to prevent infinite recursion if problems inside the code PM }
  Success:=false;
  Store:=BackTraceStrFunc;
  BackTraceStrFunc:=@SysBackTraceStr;
  Success:=GetLineInfo(ptruint(addr),func,source,line);
{ create string }
{$ifdef netware}
  { we need addr relative to code start on netware }
  dec(addr,ptruint(system.NWGetCodeStart));
  StabBackTraceStr:='  CodeStart + $'+HexStr(ptruint(addr),sizeof(ptruint)*2);
{$else}
  StabBackTraceStr:='  $'+HexStr(ptruint(addr),sizeof(ptruint)*2);
{$endif}
  if func<>'' then
    Result := Result +'  '+func;
  if source<>'' then
   begin
     if func<>'' then
      Result := Result + ', ';
     if line<>0 then
      begin
        str(line,hs);
        Result := Result + ' line ' + hs;
      end;
     Result := Result + ' of ' + source;
   end;
  if Success then
    BackTraceStrFunc:=Store;
end;

initialization
  BackTraceStrFunc := @StabBackTraceStr;

finalization
  if e.isopen then
   CloseStabs;
end.
