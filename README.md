# lazarus-exception-logger
Extended version of ExceptionLogger (https://github.com/beNative/lazarus).
At the moment of unhanled exception in the application shows basic application information and exception stack trace report. Also the error report can be saved to a file.

**Example of app info**:

    Class: Exception
    Message: Simple exception
    Application: Demo
    Version: Demo
    Time: 29.11.2017 13:58:57
    Process ID: 6412
    Thread ID: 6240
    
**Example of error report file**

    Date/time          : 2017-11-29 15:41:22.904
    Process ID         : 7056
    Thread ID          : 12004
    Executable         : IncodeUsage.exe
    Application title  : IncodeUsage
    Version            : 1.1.0.0
    Exception class    : EMyException
    Exception message  : Woo!
    1: 004247AD in BUTTON1CLICK utestincode.pas(35)
    2: 004F9668 in CLICK ./include/control.inc(2736)
    3: 0050E43D in CLICK ./include/buttoncontrol.inc(54)
    4: 0050EA5D in CLICK ./include/buttons.inc(169)
    5: 0050E36B in WMDEFAULTCLICKED ./include/buttoncontrol.inc(20)
    6: 0040D058 in  (754)
    7: 004EF770 in WNDPROC ./include/wincontrol.inc(5384)
    8: 005385D9 in DELIVERMESSAGE lclmessageglue.pas(112)
    9: 004CC65F in DOWINDOWPROC ./win32/win32callback.inc(2441)
    10: 004CCCEB in WINDOWPROC ./win32/win32callback.inc(2604)
    11: 0053DA17 in CUSTOMFORMWNDPROC ./win32/win32wsforms.pp(382)
    12: 76CBC4B7 in  (0)
    13: 76CBC5B7 in  (0)
    14: 76CB5264 in  (0)
    15: 76CB5552 in  (0)
    16: 738445A1 in  (0)
    17: 73844603 in  (0)

    
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



