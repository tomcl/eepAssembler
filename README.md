# eepAssembler

A simple assembler for EEP1 and EEP2 CPU as taught at Imperial College 2022

*This code is written by Tom Clarke with no guarantee as to its correctness: please e-mail me if you find errors*

Use issues on this repo for feature requests

The F# source is in `./src/Program.fs`

## To run the assembler

Before you start:

* Download or fork and clone this repo.
* Install [.NET SDK 6](https://dotnet.microsoft.com/en-us/download/visual-studio-sdks) or if you don't want to develop the code [.NET Runtime 6](https://dotnet.microsoft.com/en-us/download/dotnet/6.0) on your systemon your system

To run the assembler:

* `dotnet run dir`
   * replace `dir` by the directory you want to watch
* `dotnet run` will watch `.`

### From windows via powershell

Run `chooser.bat` to use file selection GUI - directory of file selected will be watched.

## Features

* The assembler will watch a directory and turn any `.txt` file into a `.ram` file suitable for use by Issie, or print out errors.
* Files that change will get re-assembled, so you can edit a file and save it with auto-assembly on save.
* Lines can be labelled (see `assembler.txt`) and labels used in jump or memory instructions as Imm8 operands.



## To develop

On windows:
* Install *Visual Studio 2022* with F# desktop
* (if needed) install [.NET SDK](https://dotnet.microsoft.com/en-us/download/visual-studio-sdks) 
* load `./eepassem.sln`

See [HLP setup](https://intranet.ee.ic.ac.uk/t.clarke/hlp/install-notes.html) for more details of different dev environments.


