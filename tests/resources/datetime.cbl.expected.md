# datetime.cbl

Core library: datetime

author: Olegs Kunicins


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

## Function Summary

| Name | Description |
| ----------- | ----------- | 
| [datetime-format](#datetime-format) | Format the given or current timestamp, replacing the tokens, such as<br>YY    Year                                      18<br>YYYY  Year                                      2018<br>M     Month of the year (1-12)                  7<br>MM    Month of the year (01-12)                 07<br>MMM   Month of the year textual                 Jul<br>D     Day of the month (1-31)                   9<br>DD    Day of the month (01-31)                  09<br>DDD   Day of the year (01-366)                  07<br>WW    Week of the year (01-53)                  05<br>U     Weekday (1-7)                             2<br>EEE   Weekday textual      	                   Tue<br>h     Hour of the day (0-23)                    5<br>hh    Hour of the day (00-23)                   05<br>m     Minute of the hour (0-59)                 9<br>mm    Minute of the hour (00-59)                09<br>s     Second of the minute (0-59)               4<br>ss    Second of the minute (00-59)              04<br>z     Timezone                                  GMT-08:00<br>x     Timezone ISO 8601                         -08:00<br> | 

## Function Details

### datetime-format

*datetime-format(l-format, l-timestamp)*

Format the given or current timestamp, replacing the tokens, such as  
YY    Year                                      18  
YYYY  Year                                      2018  
M     Month of the year (1-12)                  7  
MM    Month of the year (01-12)                 07  
MMM   Month of the year textual                 Jul  
D     Day of the month (1-31)                   9  
DD    Day of the month (01-31)                  09  
DDD   Day of the year (01-366)                  07  
WW    Week of the year (01-53)                  05  
U     Weekday (1-7)                             2  
EEE   Weekday textual      	                   Tue  
h     Hour of the day (0-23)                    5  
hh    Hour of the day (00-23)                   05  
m     Minute of the hour (0-59)                 9  
mm    Minute of the hour (00-59)                09  
s     Second of the minute (0-59)               4  
ss    Second of the minute (00-59)              04  
z     Timezone                                  GMT-08:00  
x     Timezone ISO 8601                         -08:00  


#### Parameters

> **l-format** 32-char long string 
> **l-timestamp** 21-char long current-date or ZERO 

#### Returns

> Formatted timestamp trailing by spaces, 32-char long
