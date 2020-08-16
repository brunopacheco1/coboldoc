import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { TemplateEngine, TemplateEngineImpl } from '../../src/services/template-engine';
import { Documentation } from '../../src/model/documentation';
import { Format } from '../../src/model/format';
import * as fs from 'fs';

describe('Parsing string.cbl to doc file', () => {
    const doc: Documentation = {
        fileName: 'string.cbl',
        author: 'Olegs Kunicins',
        license: 'LGPL-3.0 This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the License, or (at your option) any later version. This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You should have received a copy of the GNU Lesser General Public License along with this library.',
        fileDescription: 'Core library: string',
        modules: [],
        functions: [{
            name: 'substr-pos',
            line: 32,
            description: 'Find the position of the first occurrence of a substring in a string. Case-sensitive.',
            params: [{
                name: 'l-haystack',
                description: 'String to search in',
                type: undefined
            }, {
                name: 'l-needle',
                description: 'String to search for',
                type: undefined
            }],
            return: {
                description: 'Position where the needle exists relative to the beginnning of l-haystack. Returns 0 if not found.',
                type: undefined
            }
        }, {
            name: 'substr-pos-case',
            line: 74,
            description: 'Find the position of the first occurrence of a substring in a string. Case-insensitive.',
            params: [{
                name: 'l-haystack',
                description: 'String to search in',
                type: undefined
            }, {
                name: 'l-needle',
                description: 'String to search for',
                type: undefined
            }],
            return: {
                description: 'Position where the needle exists relative to the beginnning of l-haystack. Returns 0 if not found.',
                type: undefined
            }
        }, {
            name: 'byte-to-hex',
            line: 97,
            description: 'Convert one byte into hexadecimal representation.',
            params: [{
                name: 'l-byte',
                description: 'Byte',
                type: undefined
            }],
            return: {
                description: '2 hexadecimal chars',
                type: undefined
            }
        }, {
            name: 'hex-to-byte',
            line: 123,
            description: 'Convert one byte into hexadecimal representation.',
            params: [{
                name: 'l-hex',
                description: '2 hexadecimal chars',
                type: undefined
            }],
            return: {
                description: 'Byte',
                type: undefined
            }
        }, {
            name: 'substr-count',
            line: 155,
            description: 'Count the number of substring occurrences. Case-sensitive.',
            params: [{
                name: 'l-haystack',
                description: 'String to search in',
                type: undefined
            }, {
                name: 'l-needle',
                description: 'String to search for',
                type: undefined
            }],
            return: {
                description: 'Number of occurrences',
                type: undefined
            }
        }, {
            name: 'substr-count-case',
            line: 193,
            description: 'Count the number of substring occurrences. Case-insensitive.',
            params: [{
                name: 'l-haystack',
                description: 'String to search in',
                type: undefined
            }, {
                name: 'l-needle',
                description: 'String to search for',
                type: undefined
            }],
            return: {
                description: 'Number of occurrences',
                type: undefined
            }
        }, {
            name: 'sha3-256',
            line: 216,
            description: 'Generate SHA3-256 message digest',
            params: [{
                name: 'l-buffer',
                description: 'Input bytes',
                type: undefined
            }],
            return: {
                description: '64 hexadecimal chars',
                type: undefined
            }
        }, {
            name: 'sha3-512',
            line: 254,
            description: 'Generate SHA3-512 message digest',
            params: [{
                name: 'l-buffer',
                description: 'Input bytes',
                type: undefined
            }],
            return: {
                description: '128 hexadecimal chars',
                type: undefined
            }
        }, {
            name: 'urlencoded-to-byte',
            line: 292,
            description: 'Convert urlencoded symbol into one byte.',
            params: [{
                name: 'l-symbol',
                description: 'Urlencoded symbol (3 bytes)',
                type: undefined
            }],
            return: {
                description: 'Byte',
                type: undefined
            }
        }, {
            name: 'byte-to-urlencoded',
            line: 318,
            description: 'Convert one byte into urlencoded symbol.',
            params: [{
                name: 'l-byte',
                description: 'Byte',
                type: undefined
            }],
            return: {
                description: 'Urlencoded symbol (3 bytes)',
                type: undefined
            }
        }, {
            name: 'csv-ecb-rates',
            line: 340,
            description: 'Convert ECB exchange rates in CSV format to the list of currency-rate pairs. https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html',
            params: [{
                name: 'l-byte',
                description: 'CSV string',
                type: undefined
            }],
            return: {
                description: 'Urlencoded symbol Pointer to the list of 64 [pic x(3), pic 9(7)V9(8)] elements',
                type: undefined
            }
        }]
    };

    let service: TemplateEngine;

    beforeEach(() => {
        service = new TemplateEngineImpl();
    });

    it('should parse to md', async () => {
        const actual = service.parseDocumentation(Format.MD, doc);
        const expected = fs.readFileSync('./tests/resources/string.cbl.expected.md', 'utf8');
        expect(actual).to.deep.equal(expected);
    });

    it('should parse to html', async () => {
        const actual = service.parseDocumentation(Format.HTML, doc);
        const expected = fs.readFileSync('./tests/resources/string.cbl.expected.html', 'utf8');
        expect(actual).to.deep.equal(expected);
    });
});
