<h1 align="center">
  <br>
    <img src="https://github.com/brunopacheco1/coboldoc/blob/master/icon.png?raw=true" alt="logo" width="200">
  <br>
  Documentation generator for COBOL
  <br>
  <br>
</h1>

<h4 align="center">A Command-line tool to generate documentation for COBOL.</h4>
<p align="center">
<span><a href="https://sonarcloud.io/dashboard?id=brunopacheco1_coboldoc"><img src="https://sonarcloud.io/api/project_badges/measure?project=brunopacheco1_coboldoc&metric=alert_status" alt="Quality Gate Status" /></a></span>
<span><img src="https://github.com/brunopacheco1/coboldoc/workflows/nodejs-ci/badge.svg" alt="NPM downloads" /></span>
<span><a href="https://npmjs.com/package/coboldoc" title="View this project on NPM"><img src="https://img.shields.io/npm/dm/coboldoc.svg" alt="NPM downloads" /></a></span>
</p>

#### Features
- Generate documentation in Markdown;
- Generate documentation in HTML;
- Support for in-line HTML content;
- Support for in-line Markdown content;
- Support for Tag and XML annotations;
- Support for Free Format and Microfocus;

#### Requirements
- NodeJS 8+

#### Installation
```
$ npm install -g coboldoc
```

#### Usage

```
$ coboldoc 
                 _               _       _                
   ___    ___   | |__     ___   | |   __| |   ___     ___ 
  / __|  / _ \  | '_ \   / _ \  | |  / _` |  / _ \   / __|
 | (__  | (_) | | |_) | | (_) | | | | (_| | | (_) | | (__ 
  \___|  \___/  |_.__/   \___/  |_|  \__,_|  \___/   \___|
                                                          
Usage: coboldoc <command> [options]

Options:
  -o, --output <output directory>        The output directory (default: "/home/casa/dev/git/coboldoc")
  -f, --format <fileFormat>              Suported output format: md, html (default: "md")
  -d, --dialect <dialect>                Suported dialects: free, microfocus (default: "free")
  -a, --annotation <comment annotation>  Suported comment annotations: tag, xml (default: "tag")
  -v, --version                          output the version number
  -h, --help                             display help for command

Commands:
  generate <files...>                    generate the documentation
  help [command]                         display help for command
```

##### Usage sample

```
$ coboldoc generate resources/string.cbl resources/keccak.cbl resources/freedialectsample.cbl -o ./doc/
                 _               _       _
   ___    ___   | |__     ___   | |   __| |   ___     ___
  / __|  / _ \  | '_ \   / _ \  | |  / _` |  / _ \   / __|
 | (__  | (_) | | |_) | | (_) | | | | (_| | | (_) | | (__
  \___|  \___/  |_.__/   \___/  |_|  \__,_|  \___/   \___|

Output directory: ./doc/
Selected format: md
Generating string.cbl documentation... DONE
Generating keccak.cbl documentation... DONE
Generating freedialectsample.cbl documentation... DONE
Generating README.md... DONE
```

#### Supported dialects
COBOLDoc suports free format and Microfocus comment dialects. The diff is on how the comment blocks are detected. If it is a free format code, the comment block pattern is `*>*(*)`, and if it is Microfocus code, it will look for `*>>*(*)`.

#### Supported annotations
COBOLDoc suports tag or XML annotations.

##### Tag annotations
```
*>**
*>  Short sample.
*>  @author Bruno Pacheco (https://brunopacheco1.github.io/)
*>  @license LGPL-3.0
*>**
```

##### XML annotations
```
*>>**
*>>  <summary>Short sample.</summary>
*>>  <author>Bruno Pacheco (https://brunopacheco1.github.io/)</author>
*>>  <license>LGPL-3.0</license>
*>**
```

#### Comment Blocks
COBOLDoc supports the following comment blocks in order to build the documentation.

In essence, COBOLDoc has been tested against COBOL source files written in free-format only, but there shouldn't be restrictions on other COBOL coding formats, if you follow the standards of this tool. During the scanning phase, COBOLDoc ignores leading blank spaces of each line of code.

##### File Comment Block
Anything that describes the file as a whole, will scanned in this kind of comment block, starting and ending with `*>**` and all internal line starting with `*> `.

File comment block can be anywhere in the source file, but ideally it should be in the beginning of it.

```
*>**
*>  Short sample.
*>  @author Bruno Pacheco (https://brunopacheco1.github.io/)
*>  @license LGPL-3.0
*>**
```

##### Module/Function Comment Block
Anything that describes the next line module or function, will scanned in this kind of comment block, starting and ending with `*>*` and all internal line starting with `*> `.

Module/Function comment blocks have to preceed a `FUNCTION-ID` or `PROGRAM-ID` definition.

```
...

*>*
*> The description of this module.
*>*
IDENTIFICATION DIVISION.
PROGRAM-ID. anymodule.

...

*>*
*> The description of this function
*> @param  {PIC 9} myarg The argument of this function
*> @return {PIC 9}       The return of this function
*>*
IDENTIFICATION DIVISION.
FUNCTION-ID. anyfunction.

...
```

#### Supported tags

##### @author \<author>
Defines the author(s) of the source file. This tag will be scanned in a File Comment Block.

Input:
```
*>**
*> @author Frodo, Sam, Pippin, Merry, Aragorn,
*> Legolas, Gandalf, Gimli, Boromir
*>**
```

Output in MD:

```
author: Frodo, Sam, Pippin, Merry, Aragorn, Legolas, Gandalf, Gimli, Boromir
```

##### @license \<license>
Defines the license of the source file. This tag will be scanned in a File Comment Block.

Input:
```
*>**
*>  @license LGPL-3.0
*>
*>  This library is free software; you can redistribute it and/or
*>  modify it under the terms of the GNU Lesser General Public
*>  License as published by the Free Software Foundation; either
*>  version 3.0 of the License, or (at your option) any later version.
*>  
*>  This library is distributed in the hope that it will be useful,
*>  but WITHOUT ANY WARRANTY; without even the implied warranty of
*>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*>  Lesser General Public License for more details.
*>  
*>  You should have received a copy of the GNU Lesser General Public
*>  License along with this library.
*>**
```

Output in MD:

```
license: LGPL-3.0 This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the License, or (at your option) any later version. This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You should have received a copy of the GNU Lesser General Public License along with this library.
```

##### @param {type} \<param name>
Defines the parameter or argument of function. This tag will be scanned in a Module/Function Comment Block.
The type is not mandatory and accepts anything.

Input:
```
*>*
*> @param {Any Type}   l-myparameter1  A description of l-myparameter1
*> @param {Any Type 2} l-myparameter2  A description of l-myparameter2
*>*
```

Output in MD:

```
> *{Any Type}* **l-myparameter1** A description of l-myparameter1
> *{Any Type 2}* **l-myparameter2** A description of l-myparameter2
```

##### @return {type}
Defines the return of function. This tag will be scanned in a Module/Function Comment Block.
The type is not mandatory and accepts anything.

Input:
```
*>*
*> @return {Any Type}  A description of this function return.
*>*
```

Output in MD:

```
> *{Any Type}* A description of this function return.
```

##### @summary \<summary>
If you need to add a short text into the Module or Function Summary tables, you can make use of this tag. When it is missing, the general description will be added into Summary table.

Input:
```
*>*
*> The description of anyfunction1 perhaps is quite too long to be added into the summary table.
*> @summary Short summary of anyfunction1.
*>*
IDENTIFICATION DIVISION.
FUNCTION-ID. anyfunction1.

...

*>*
*> The description of anyfunction2 is not that long.
*>*
IDENTIFICATION DIVISION.
FUNCTION-ID. anyfunction2.
```

Output in MD:

```
| Name | Description |
| ----------- | ----------- | 
| [anyfunction1](#anyfunction1) | Short summary of anyfunction1. | 
| [anyfunction2](#anyfunction2) | The description of anyfunction2 is not that long. | 
```

#### Support for HTML in-line content
COBOLDoc support HTML content. If you need any special formating, it should work correctly if it respects HTML syntax.

Input:
```
*>*
*>  The KECCAK module, that uses the Keccak-f[1600] permutation.<br>
*>
*>  Date-Written: 2016-05-17<br>
*>  Fields in LINKAGE SECTION:<br>
*> <ul>
*>    <li>LNK-KECCAK-RATE: The value of the rate r. The rate must be
*>      a multiple of 8 bits in this implementation.</li>           
*>    <li>LNK-KECCAK-CAPACITY: The value of the capacity c. 
*>      The rate and capacity must have r+c=1600.</li>        
*>    <li>LNK-KECCAK-INPUT: The input message.   </li>         
*>    <li>LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided
*>      in the input message.</li> 
*>    <li>LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically
*>      appended to the end of the input message, as in domain 
*>      separation.</li> 
*>    <li>LNK-KECCAK-OUTPUT: The buffer where to store the output.   </li>        
*>    <li>LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.</li>
*> </ul>
*>*
```

Output in MD:

```
The KECCAK module, that uses the Keccak-f[1600] permutation.<br> Date-Written: 2016-05-17<br> Fields in LINKAGE SECTION:<br> <ul> <li>LNK-KECCAK-RATE: The value of the rate r. The rate must be a multiple of 8 bits in this implementation.</li> <li>LNK-KECCAK-CAPACITY: The value of the capacity c. The rate and capacity must have r+c=1600.</li> <li>LNK-KECCAK-INPUT: The input message. </li> <li>LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided in the input message.</li> <li>LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically appended to the end of the input message, as in domain separation.</li> <li>LNK-KECCAK-OUTPUT: The buffer where to store the output. </li> <li>LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.</li> </ul>
```

#### Support for Markdown in-line content
COBOLDoc support Markdown content. If you need any special formating, it should work correctly if it respects Markdown syntax.

Input:
```
*>*
*>  The KECCAK module, that uses the Keccak-f[1600] permutation.  
*>  Date-Written: 2016-05-17  
*>  Fields in LINKAGE SECTION:  
*>  - LNK-KECCAK-RATE: The value of the rate r. The rate must be
*>      a multiple of 8 bits in this implementation.
*>  - LNK-KECCAK-CAPACITY: The value of the capacity c.
*>      The rate and capacity must have r+c=1600.
*>  - LNK-KECCAK-INPUT: The input message.
*>  - LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided
*>      in the input message.
*>  - LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically
*>      appended to the end of the input message, as in domain
*>      separation.
*>  - LNK-KECCAK-OUTPUT: The buffer where to store the output.
*>  - LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.
*>*
```

Output in MD:

```
The KECCAK module, that uses the Keccak-f[1600] permutation.  
Date-Written: 2016-05-17  
Fields in LINKAGE SECTION:  
- LNK-KECCAK-RATE: The value of the rate r. The rate must be
a multiple of 8 bits in this implementation.
- LNK-KECCAK-CAPACITY: The value of the capacity c.
The rate and capacity must have r+c=1600.
- LNK-KECCAK-INPUT: The input message.
- LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided
in the input message.
- LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically
appended to the end of the input message, as in domain
separation.
- LNK-KECCAK-OUTPUT: The buffer where to store the output.
- LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.
```

#### Roadmap
- List of paragraphs in the module/function details view;
- Scan and display changelog (if present);
- Comply with MSDN Comment Style (on XML annotations);
- Support other structures in COBOL (entries, classes, interfaces...);

Your contribution is always welcome!