# keccak.cbl

author: Laszlo Erdos - https://www.facebook.com/wortfee


License: LGPL-3.0
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3.0 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library.

## Program Summary

| Name | Description |
| ----------- | ----------- | 
| [KECCAK](#KECCAK) | The KECCAK program, that uses the Keccak-f[1600] permutation.<br> Date-Written: 2016-05-17<br> Fields in LINKAGE SECTION:<br> <ul> <li>LNK-KECCAK-RATE: The value of the rate r. The rate must be a multiple of 8 bits in this implementation.</li>         <br><li>LNK-KECCAK-CAPACITY: The value of the capacity c.  The rate and capacity must have r+c=1600.</li>      <br><li>LNK-KECCAK-INPUT: The input message.   </li>       <br><li>LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided in the input message.</li>  <li>LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically appended to the end of the input message, as in domain  separation.</li>  <li>LNK-KECCAK-OUTPUT: The buffer where to store the output.   </li>      <br><li>LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.</li> </ul> | 
| [STATE-PERMUTE](#STATE-PERMUTE) | Program that computes the Keccak-f[1600] permutation on the given state. | 
| [READ-LANE](#READ-LANE) | Program to load a 64-bit value from STATE. | 
| [WRITE-LANE](#WRITE-LANE) | Program to write a 64-bit value in STATE. | 
| [XOR-LANE](#XOR-LANE) | Program to xor and write a 64-bit value in STATE. | 
| [ROL-LANE](#ROL-LANE) | Program to rotate a 64-bit value. | 
| [LFSR86540](#LFSR86540) | Program that computes the linear feedback shift register (LFSR) used to define the round constants (see [Keccak Reference, Section 1.2]). | 

## Program Details

### KECCAK

*CALL "KECCAK".*

The KECCAK program, that uses the Keccak-f[1600] permutation.<br>
Date-Written: 2016-05-17<br>
Fields in LINKAGE SECTION:<br>
<ul>
<li>LNK-KECCAK-RATE: The value of the rate r. The rate must be
a multiple of 8 bits in this implementation.</li>           
<li>LNK-KECCAK-CAPACITY: The value of the capacity c. 
The rate and capacity must have r+c=1600.</li>        
<li>LNK-KECCAK-INPUT: The input message.   </li>         
<li>LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided
in the input message.</li> 
<li>LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically
appended to the end of the input message, as in domain 
separation.</li> 
<li>LNK-KECCAK-OUTPUT: The buffer where to store the output.   </li>        
<li>LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.</li>
</ul>


### STATE-PERMUTE

*CALL "STATE-PERMUTE".*

Program that computes the Keccak-f[1600] permutation on the given state.


### READ-LANE

*CALL "READ-LANE".*

Program to load a 64-bit value from STATE.


### WRITE-LANE

*CALL "WRITE-LANE".*

Program to write a 64-bit value in STATE.


### XOR-LANE

*CALL "XOR-LANE".*

Program to xor and write a 64-bit value in STATE.


### ROL-LANE

*CALL "ROL-LANE".*

Program to rotate a 64-bit value.


### LFSR86540

*CALL "LFSR86540".*

Program that computes the linear feedback shift register (LFSR) used to
define the round constants (see [Keccak Reference, Section 1.2]).

