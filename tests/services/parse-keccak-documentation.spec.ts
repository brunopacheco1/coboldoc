import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { TemplateEngine, TemplateEngineImpl } from '../../src/services/template-engine';
import { Documentation } from '../../src/model/documentation';
import { Format } from '../../src/model/format';
import * as fs from 'fs';

describe('Parsing keccak.cbl to doc file', () => {
    const doc: Documentation = {
        fileName: 'keccak.cbl',
        fileDescription: '',
        author: 'Laszlo Erdos - https://www.facebook.com/wortfee',
        license: 'LGPL-3.0\nThis library is free software; you can redistribute it and/or\nmodify it under the terms of the GNU Lesser General Public\nLicense as published by the Free Software Foundation; either\nversion 3.0 of the License, or (at your option) any later version.\nThis library is distributed in the hope that it will be useful,\nbut WITHOUT ANY WARRANTY; without even the implied warranty of\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\nLesser General Public License for more details.\nYou should have received a copy of the GNU Lesser General Public\nLicense along with this library.',
        modules: [{
            name: 'KECCAK',
            line: 41,
            description: 'The KECCAK module, that uses the Keccak-f[1600] permutation.<br>\nDate-Written: 2016-05-17<br>\nFields in LINKAGE SECTION:<br>\n<ul>\n<li>LNK-KECCAK-RATE: The value of the rate r. The rate must be\na multiple of 8 bits in this implementation.</li>           \n<li>LNK-KECCAK-CAPACITY: The value of the capacity c. \nThe rate and capacity must have r+c=1600.</li>        \n<li>LNK-KECCAK-INPUT: The input message.   </li>         \n<li>LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided\nin the input message.</li> \n<li>LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically\nappended to the end of the input message, as in domain \nseparation.</li> \n<li>LNK-KECCAK-OUTPUT: The buffer where to store the output.   </li>        \n<li>LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.</li>\n</ul>',
            summary: 'The KECCAK module, that uses the Keccak-f[1600] permutation.<br> Date-Written: 2016-05-17<br> Fields in LINKAGE SECTION:<br> <ul> <li>LNK-KECCAK-RATE: The value of the rate r. The rate must be a multiple of 8 bits in this implementation.</li><br><br><br><br><br><br><li>LNK-KECCAK-CAPACITY: The value of the capacity c.<br>The rate and capacity must have r+c=1600.</li><br><br><br><br> <li>LNK-KECCAK-INPUT: The input message.<br> </li><br><br><br><br><br><li>LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided in the input message.</li><br><li>LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically appended to the end of the input message, as in domain<br>separation.</li><br><li>LNK-KECCAK-OUTPUT: The buffer where to store the output.<br> </li><br><br><br><br> <li>LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.</li> </ul>',
            paragraphs: []
        },
        {
            name: 'STATE-PERMUTE',
            line: 198,
            description: 'Module that computes the Keccak-f[1600] permutation on the given state.',
            summary: 'Module that computes the Keccak-f[1600] permutation on the given state.',
            paragraphs: []
        },
        {
            name: 'READ-LANE',
            line: 605,
            description: 'Module to load a 64-bit value from STATE.',
            summary: 'Module to load a 64-bit value from STATE.',
            paragraphs: []
        },
        {
            name: 'WRITE-LANE',
            line: 642,
            description: 'Module to write a 64-bit value in STATE.',
            summary: 'Module to write a 64-bit value in STATE.',
            paragraphs: []
        },
        {
            name: 'XOR-LANE',
            line: 679,
            description: 'Module to xor and write a 64-bit value in STATE.',
            summary: 'Module to xor and write a 64-bit value in STATE.',
            paragraphs: []
        },
        {
            name: 'ROL-LANE',
            line: 725,
            description: 'Module to rotate a 64-bit value.',
            summary: 'Module to rotate a 64-bit value.',
            paragraphs: []
        },
        {
            name: 'LFSR86540',
            line: 784,
            description: 'Module that computes the linear feedback shift register (LFSR) used to\ndefine the round constants (see [Keccak Reference, Section 1.2]).',
            summary: 'Module that computes the linear feedback shift register (LFSR) used to define the round constants (see [Keccak Reference, Section 1.2]).',
            paragraphs: []
        }],
        functions: []
    };

    let service: TemplateEngine;

    beforeEach(() => {
        service = new TemplateEngineImpl();
    });

    it('should parse to md', async () => {
        const actual = service.parseDocumentation(Format.MD, doc);
        const expected = fs.readFileSync('./tests/resources/keccak.cbl.expected.md', 'utf8');
        expect(actual).to.deep.equal(expected);
    });

    it('should parse to html', async () => {
        const actual = service.parseDocumentation(Format.HTML, doc);
        const expected = fs.readFileSync('./tests/resources/keccak.cbl.expected.html', 'utf8');
        expect(actual).to.deep.equal(expected);
    });
});
