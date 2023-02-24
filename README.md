# eepAssembler

A simple assembler for EEP1 CPU as taught at Imperial College 2023

*This code is written by Tom Clarke with no guarantee as to its correctness: please e-mail me if you find errors*

Use issues on this repo for feature requests

The F# source is in `./src/Program.fs`

## To run the assembler on any system (Windows/Macos/Linux)

Before you start:

* Download and unzip the latest release source code or fork and clone this repo.
* Install [.NET 64 bit SDK 7](https://dotnet.microsoft.com/en-us/download). 

To run the assembler on any system (see nicer option via Windows Powershell below):

* start a command line terminal running in this directory (the one containing the downloaded README file).
* `dotnet run dir`
   * replace `dir` by the directory you want to watch
* `dotnet run` will watch `.`
    * The downloaded `.` directory has a file `assem.txt` which will be correctly assembled into `assem.ram` with a success message as below.
    
```
C:\GitHub\eepAssembler>dotnet run
Watching '.'
Successful assembly of '.\assem.txt'
7 lines written to '.\assem.ram'   
```

### On Windows systems only via Powershell

Double-click `chooser.bat` from this directory to use file selection GUI - the whole directory of file selected will be watched.

## Features

* The assembler will watch a directory and turn any `.txt` file of EEP1 assembly language into a `.ram` file of machine code suitable for use by Issie.
* If assembly errors exist they will be printed out
* Files that change will get re-assembled, so you can edit a file and save it with auto-assembly on save.
* Lines can be labelled (see `assembler.txt`) and labels used in jump or memory instructions as Imm8 operands.



## To develop

On windows:
* Install *Visual Studio 2022* with F# desktop
* (if needed) install [.NET SDK](https://dotnet.microsoft.com/en-us/download/visual-studio-sdks) 
* load `./eepassem.sln`

See [HLP setup](https://intranet.ee.ic.ac.uk/t.clarke/hlp/install-notes.html) for more details of different dev environments.


