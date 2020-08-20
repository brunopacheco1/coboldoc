import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { FreeCommentsExtractor, FreeCommentsExtractorImpl } from '../../src/services/free-comments-extractor';
import { TagCommentsParser, TagCommentsParserImpl } from '../../src/services/tag-comments-parser';
import { Documentation } from '../../src/model/documentation';

describe('datetime.cbl to documentation', () => {
    let commentsExtractor: FreeCommentsExtractor;
    let commentsParser: TagCommentsParser;

    beforeEach(() => {
        commentsExtractor = new FreeCommentsExtractorImpl();
        commentsParser = new TagCommentsParserImpl();
    });

    it('should extract', async () => {
        const preDoc = commentsExtractor.extract('./tests/resources/datetime.cbl');
        const actual: Documentation = commentsParser.parse(preDoc);
        const expected: Documentation = {
            fileName: 'datetime.cbl',
            fileDescription: 'Core library: datetime',
            author: 'Olegs Kunicins\n',
            license: 'LGPL-3.0\n\nThis library is free software; you can redistribute it and/or\nmodify it under the terms of the GNU Lesser General Public\nLicense as published by the Free Software Foundation; either\nversion 3.0 of the License, or (at your option) any later version.\n\nThis library is distributed in the hope that it will be useful,\nbut WITHOUT ANY WARRANTY; without even the implied warranty of\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\nLesser General Public License for more details.\n\nYou should have received a copy of the GNU Lesser General Public\nLicense along with this library.',
            functions: [
                {
                    description: 'Format the given or current timestamp, replacing the tokens, such as  \nYY    Year                                      18  \nYYYY  Year                                      2018  \nM     Month of the year (1-12)                  7  \nMM    Month of the year (01-12)                 07  \nMMM   Month of the year textual                 Jul  \nD     Day of the month (1-31)                   9  \nDD    Day of the month (01-31)                  09  \nDDD   Day of the year (01-366)                  07  \nWW    Week of the year (01-53)                  05  \nU     Weekday (1-7)                             2  \nEEE   Weekday textual      \t                   Tue  \nh     Hour of the day (0-23)                    5  \nhh    Hour of the day (00-23)                   05  \nm     Minute of the hour (0-59)                 9  \nmm    Minute of the hour (00-59)                09  \ns     Second of the minute (0-59)               4  \nss    Second of the minute (00-59)              04  \nz     Timezone                                  GMT-08:00  \nx     Timezone ISO 8601                         -08:00  \n',
                    summary: 'Format the given or current timestamp, replacing the tokens, such as  \nYY    Year                                      18  \nYYYY  Year                                      2018  \nM     Month of the year (1-12)                  7  \nMM    Month of the year (01-12)                 07  \nMMM   Month of the year textual                 Jul  \nD     Day of the month (1-31)                   9  \nDD    Day of the month (01-31)                  09  \nDDD   Day of the year (01-366)                  07  \nWW    Week of the year (01-53)                  05  \nU     Weekday (1-7)                             2  \nEEE   Weekday textual      \t                   Tue  \nh     Hour of the day (0-23)                    5  \nhh    Hour of the day (00-23)                   05  \nm     Minute of the hour (0-59)                 9  \nmm    Minute of the hour (00-59)                09  \ns     Second of the minute (0-59)               4  \nss    Second of the minute (00-59)              04  \nz     Timezone                                  GMT-08:00  \nx     Timezone ISO 8601                         -08:00  \n',
                    line: 48,
                    name: 'datetime-format',
                    paragraphs: [],
                    params: [
                        {
                            name: 'l-format',
                            description: '32-char long string',
                            type: undefined,
                        },
                        {
                            name: 'l-timestamp',
                            description: '21-char long current-date or ZERO',
                            type: undefined,
                        }
                    ],
                    return: {
                        description: 'Formatted timestamp trailing by spaces, 32-char long',
                        type: undefined,
                    }
                }
            ],
            modules: []
        };
        expect(actual).to.deep.equal(expected);
    });
});
