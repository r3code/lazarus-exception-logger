# lazarus-exception-logger
Extended version of ExceptionLogger (https://github.com/beNative/lazarus).
At the moment of unhanled exception in the application shows basic application 
information and exception stack trace report. 

Also the error report can be saved to a file. New error report record will be 
added to the end of the existing report file.

**Example of app info**:

    Class: Exception
    Message: Simple exception
    Application: Demo
    Version: Demo
    Time: 29.11.2017 13:58:57
    Process ID: 6412
    Thread ID: 6240
    
**Example of error report file**

    Date/time          : 2017-11-29 16:23:45.911
    Process ID         : 12048
    Thread ID          : 9076
    Executable         : IncodeUsage.exe
    App. title         : IncodeUsage
    Version            : 1.1.0.0      
    Compiled date      : 2017/11/29 at 17:03:30
    Build target       : i386 - Win32
    LCL version        : LCL 1.6.4.0
    Widget set         : Win32/Win64 widget set
    Exception class    : EMyException
    Exception message  : Woo!
    01: 0042471D BUTTON1CLICK in utestincode.pas(35)
    02: 004F9558 CLICK in ./include/control.inc(2736)
    03: 0050E32D CLICK in ./include/buttoncontrol.inc(54)
    04: 0050E94D CLICK in ./include/buttons.inc(169)
    05: 0050E25B WMDEFAULTCLICKED in ./include/buttoncontrol.inc(20)
    06: 0040CFC8  in (754)
    07: 004EF660 WNDPROC in ./include/wincontrol.inc(5384)
    08: 005384C9 DELIVERMESSAGE in lclmessageglue.pas(112)
    09: 004CC54F DOWINDOWPROC in ./win32/win32callback.inc(2441)
    10: 004CCBDB WINDOWPROC in ./win32/win32callback.inc(2604)
    11: 0053D907 CUSTOMFORMWNDPROC in ./win32/win32wsforms.pp(382)
    12: 76CBC4B7  in (0)
    13: 76CBC5B7  in (0)
    14: 76CB5264  in (0)
    15: 76CB5552  in (0)
    16: 738445A1  in (0)
    17: 73844603  in (0)

    
**Example of exception stack trace dialog**:

see doc/images/error_report.jpg
![Exception Report Example](https://raw.githubusercontent.com/r3code/lazarus-exception-logger/master/doc/images/error_report.jpg)

## Install 

* Download and install lpk file as Lazarus package.
* Or copy source file to your project to use it standalone, include the files in a project or set the source path, init it directly in your code.

## Usage

### Common

Set *Project Options - Debugging* 
*Checks and assertions*
 
* I/O (-Ci)
* Range (-Cr)
* Overflow(-Co)
* Stack (-Ct)
* other by your needs.

*Generate debugging info...*: Yes

*Type of debug info*: Stabs (-gs)

*Display line numbers...*: Yes

### Standalone usage

See demo/IncodeUsage.lpr

### As Component

Put component TExceptionLogger (tab Samples) to your form and set properties you want.

### TExceptionLogger Properties 

**LogFileName** - filepath to write report to.

**MaxCallStackDepth** - maximum count of lines listed in the report, default 20.

## Demo 

See *demo* folder.



