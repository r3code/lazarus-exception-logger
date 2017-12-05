# lazarus-exception-logger

Extended version of ExceptionLogger (https://github.com/beNative/lazarus).
At the moment of unhanled exception in the application shows basic application 
information and exception stack trace report. 

Also the error report can be saved to a file. New error report record will be 
added to the end of the existing report file.

Repository: https://github.com/r3code/lazarus-exception-logger

**Example of app info**:

    Class: Exception
    Message: Simple exception
    Application: Demo
    Version: Demo
    Time: 29.11.2017 13:58:57
    Process ID: 6412
    Thread ID: 6240
    
**Example of error report file**

    Date/time          : 2017-12-01 17:53:48.110
    Operating system   : Windows 7 SP1 i386
    user name          : Дмитрий
    program up time    : 0days 0hours 0min 2sec 356ms
    free disk space    : 296 GB
    Process ID         : 15860
    Thread ID          : 16152
    Executable         : IncodeUsage.exe
    App. title         : IncodeUsage Exaple of ExceptionLogger
    Version            : 1.1.0.0
    Compiled date      : 2017/12/01 at 17:52:03
    Build target       : i386 - Win32
    LCL version        : LCL 1.6.4.0
    Widget set         : Win32/Win64 widget set
    Exception class    : EMyException
    Exception message  : Woo!
    code revision      : 43594

    01: 00426B2D   TFRMTESTINCODE.BUTTON1CLICK in utestincode.pas (62)
    02: 004FCB98   TCONTROL.CLICK in ./include/control.inc (2736)
    03: 0051194D   TBUTTONCONTROL.CLICK in ./include/buttoncontrol.inc (54)
    04: 00511F6D   TCUSTOMBUTTON.CLICK in ./include/buttons.inc (169)
    05: 0051187B   TBUTTONCONTROL.WMDEFAULTCLICKED in ./include/buttoncontrol.inc (20)
    06: 0040D188     (754)
    07: 004F2CA0   TWINCONTROL.WNDPROC in ./include/wincontrol.inc (5384)
    08: 0053BAE9   DELIVERMESSAGE in lclmessageglue.pas (112)
    09: 004CFB8F   TWINDOWPROCHELPER.DOWINDOWPROC in ./win32/win32callback.inc (2441)
    10: 004D021B   WINDOWPROC in ./win32/win32callback.inc (2604)
    11: 00540F27   CUSTOMFORMWNDPROC in ./win32/win32wsforms.pp (382)
    12: 76CBC4B7     
    13: 76CBC5B7     
    14: 76CB5264     
    15: 76CB5552     
    16: 738445A1     
    17: 73844603 

    
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



