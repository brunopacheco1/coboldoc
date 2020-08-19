import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { FreeCommentsExtractor, FreeCommentsExtractorImpl } from '../../src/services/free-comments-extractor';
import { TagCommentsParser, TagCommentsParserImpl } from '../../src/services/tag-comments-parser';
import { Documentation } from '../../src/model/documentation';

describe('keccak.cbl to documentation', () => {
    let commentsExtractor: FreeCommentsExtractor;
    let commentsParser: TagCommentsParser;

    beforeEach(() => {
        commentsExtractor = new FreeCommentsExtractorImpl();
        commentsParser = new TagCommentsParserImpl();
    });

    it('should extract', async () => {
        const preDoc = commentsExtractor.extract('./tests/resources/keccak.cbl');
        const actual: Documentation = commentsParser.parse(preDoc);
        const expected: Documentation = {
            fileName: 'keccak.cbl',
            fileDescription: '',
            author: 'Laszlo Erdos - https://www.facebook.com/wortfee',
            license: 'LGPL-3.0\nThis library is free software; you can redistribute it and/or\nmodify it under the terms of the GNU Lesser General Public\nLicense as published by the Free Software Foundation; either\nversion 3.0 of the License, or (at your option) any later version.\nThis library is distributed in the hope that it will be useful,\nbut WITHOUT ANY WARRANTY; without even the implied warranty of\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\nLesser General Public License for more details.\nYou should have received a copy of the GNU Lesser General Public\nLicense along with this library.',
            modules: [{
                name: 'KECCAK',
                line: 41,
                description: 'The KECCAK module, that uses the Keccak-f[1600] permutation.<br>\nDate-Written: 2016-05-17<br>\nFields in LINKAGE SECTION:<br>\n<ul>\n<li>LNK-KECCAK-RATE: The value of the rate r. The rate must be\na multiple of 8 bits in this implementation.</li>           \n<li>LNK-KECCAK-CAPACITY: The value of the capacity c. \nThe rate and capacity must have r+c=1600.</li>        \n<li>LNK-KECCAK-INPUT: The input message.   </li>         \n<li>LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided\nin the input message.</li> \n<li>LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically\nappended to the end of the input message, as in domain \nseparation.</li> \n<li>LNK-KECCAK-OUTPUT: The buffer where to store the output.   </li>        \n<li>LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.</li>\n</ul>',
                summary: 'The KECCAK module, that uses the Keccak-f[1600] permutation.<br>\nDate-Written: 2016-05-17<br>\nFields in LINKAGE SECTION:<br>\n<ul>\n<li>LNK-KECCAK-RATE: The value of the rate r. The rate must be\na multiple of 8 bits in this implementation.</li>           \n<li>LNK-KECCAK-CAPACITY: The value of the capacity c. \nThe rate and capacity must have r+c=1600.</li>        \n<li>LNK-KECCAK-INPUT: The input message.   </li>         \n<li>LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided\nin the input message.</li> \n<li>LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically\nappended to the end of the input message, as in domain \nseparation.</li> \n<li>LNK-KECCAK-OUTPUT: The buffer where to store the output.   </li>        \n<li>LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.</li>\n</ul>',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                name: 'STATE-PERMUTE',
                line: 198,
                description: 'Module that computes the Keccak-f[1600] permutation on the given state.',
                summary: 'Module that computes the Keccak-f[1600] permutation on the given state.',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                name: 'READ-LANE',
                line: 605,
                description: 'Module to load a 64-bit value from STATE.',
                summary: 'Module to load a 64-bit value from STATE.',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                name: 'WRITE-LANE',
                line: 642,
                description: 'Module to write a 64-bit value in STATE.',
                summary: 'Module to write a 64-bit value in STATE.',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                name: 'XOR-LANE',
                line: 679,
                description: 'Module to xor and write a 64-bit value in STATE.',
                summary: 'Module to xor and write a 64-bit value in STATE.',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                name: 'ROL-LANE',
                line: 725,
                description: 'Module to rotate a 64-bit value.',
                summary: 'Module to rotate a 64-bit value.',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                name: 'LFSR86540',
                line: 784,
                description: 'Module that computes the linear feedback shift register (LFSR) used to\ndefine the round constants (see [Keccak Reference, Section 1.2]).',
                summary: 'Module that computes the linear feedback shift register (LFSR) used to\ndefine the round constants (see [Keccak Reference, Section 1.2]).',
                paragraphs: [],
                return: undefined,
                params: []
            }],
            functions: []
        };
        expect(actual).to.deep.equal(expected);
    });
});
