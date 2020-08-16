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
        license: 'LGPL-3.0 This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the License, or (at your option) any later version. This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You should have received a copy of the GNU Lesser General Public License along with this library.',
        modules: [{
            name: 'KECCAK',
            line: 39,
            description: 'The KECCAK module, that uses the Keccak-f[1600] permutation. Date-Written: 2016-05-17 Fields in LINKAGE SECTION: - LNK-KECCAK-RATE: The value of the rate r. The rate must be a multiple of 8 bits in this implementation. - LNK-KECCAK-CAPACITY: The value of the capacity c. The rate and capacity must have r+c=1600. - LNK-KECCAK-INPUT: The input message. - LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided in the input message. - LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically appended to the end of the input message, as in domain separation. - LNK-KECCAK-OUTPUT: The buffer where to store the output. - LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.',
            paragraphs: []
        },
        {
            name: 'STATE-PERMUTE',
            line: 196,
            description: 'Module that computes the Keccak-f[1600] permutation on the given state.',
            paragraphs: []
        },
        {
            name: 'READ-LANE',
            line: 603,
            description: 'Module to load a 64-bit value from STATE.',
            paragraphs: []
        },
        {
            name: 'WRITE-LANE',
            line: 640,
            description: 'Module to write a 64-bit value in STATE.',
            paragraphs: []
        },
        {
            name: 'XOR-LANE',
            line: 677,
            description: 'Module to xor and write a 64-bit value in STATE.',
            paragraphs: []
        },
        {
            name: 'ROL-LANE',
            line: 723,
            description: 'Module to rotate a 64-bit value.',
            paragraphs: []
        },
        {
            name: 'LFSR86540',
            line: 782,
            description: 'Module that computes the linear feedback shift register (LFSR) used to define the round constants (see [Keccak Reference, Section 1.2]).',
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
