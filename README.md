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

Download and install lpk file as Lazarus package.

## Usage

Put component TExceptionLogger (tab Samples) to your form and set properties you want.

### Properties 

**LogFileName** - filename where to write report to.
**MaxCallStackDepth** - maximum count of lines listed in the report, default 20.

## Demo 

See *demo* folder.



