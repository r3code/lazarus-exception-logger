# lazarus-exception-logger
Extended version of ExceptionLogger (https://github.com/beNative/lazarus).
At the moment of unhanled exception in the application shows basic application information and exception stack trace report.

**Example of app info**:

    Class: Exception
    Message: Simple exception
    Application: Demo
    Version: Demo
    Time: 29.11.2017 13:58:57
    Process ID: 6412
    Thread ID: 6240
    
**Example of exception stack trace**:

see doc/images/error_report.jpg

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

**LogFileName** - filename where to write report to.
**MaxCallStackDepth** - maximum count of lines listed in the report, default 20.

## Demo 

See *demo* folder.



