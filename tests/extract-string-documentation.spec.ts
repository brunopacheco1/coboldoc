import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { CommentsExtractor, CommentsExtractorImpl } from '../src/services/comments-extractor';
import { TagCommentsParser, TagCommentsParserImpl } from '../src/services/tag-comments-parser';
import { Documentation } from '../src/model/documentation';
import { CommentStyle } from '../src/model/comment-style';

describe('string.cbl to documentation', () => {
    let commentsExtractor: CommentsExtractor;
    let commentsParser: TagCommentsParser;

    beforeEach(() => {
        commentsExtractor = new CommentsExtractorImpl();
        commentsParser = new TagCommentsParserImpl();
    });

    it('should extract', async () => {
        const preDoc = commentsExtractor.extract(CommentStyle.FREE, './tests/resources/string.cbl');
        const actual: Documentation = commentsParser.parse(preDoc);
        const expected: Documentation = {
            fileName: 'string.cbl',
            author: 'Olegs Kunicins\n',
            license: 'LGPL-3.0\nThis library is free software; you can redistribute it and/or\nmodify it under the terms of the GNU Lesser General Public\nLicense as published by the Free Software Foundation; either\nversion 3.0 of the License, or (at your option) any later version.\n\nThis library is distributed in the hope that it will be useful,\nbut WITHOUT ANY WARRANTY; without even the implied warranty of\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\nLesser General Public License for more details.\n\nYou should have received a copy of the GNU Lesser General Public\nLicense along with this library.',
            fileDescription: 'Core library: string',
            modules: [],
            functions: [{
                name: 'substr-pos',
                line: 32,
                description: 'Find the position of the first occurrence of a substring in a string.\nCase-sensitive.\n\n',
                summary: 'Find the position of the first occurrence of a substring in a string.\nCase-sensitive.\n\n',
                paragraphs: [],
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
                    description: 'Position where the needle exists relative to the beginnning\nof l-haystack. Returns 0 if not found.',
                    type: undefined
                }
            }, {
                name: 'substr-pos-case',
                line: 74,
                description: 'Find the position of the first occurrence of a substring in a string.\nCase-insensitive.\n\n',
                summary: 'Find the position of the first occurrence of a substring in a string.\nCase-insensitive.\n\n',
                paragraphs: [],
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
                    description: 'Position where the needle exists relative to the beginnning\nof l-haystack. Returns 0 if not found.',
                    type: undefined
                }
            }, {
                name: 'byte-to-hex',
                line: 97,
                description: 'Convert one byte into hexadecimal representation.\n\n',
                summary: 'Convert one byte into hexadecimal representation.\n\n',
                paragraphs: [],
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
                description: 'Convert one byte into hexadecimal representation.\n\n',
                summary: 'Convert one byte into hexadecimal representation.\n\n',
                paragraphs: [],
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
                description: 'Count the number of substring occurrences. Case-sensitive.\n\n',
                summary: 'Count the number of substring occurrences. Case-sensitive.\n\n',
                paragraphs: [],
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
                description: 'Count the number of substring occurrences. Case-insensitive.\n\n',
                summary: 'Count the number of substring occurrences. Case-insensitive.\n\n',
                paragraphs: [],
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
                description: 'Generate SHA3-256 message digest\n\n',
                summary: 'Generate SHA3-256 message digest\n\n',
                paragraphs: [],
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
                description: 'Generate SHA3-512 message digest\n\n',
                summary: 'Generate SHA3-512 message digest\n\n',
                paragraphs: [],
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
                description: 'Convert urlencoded symbol into one byte.\n\n',
                summary: 'Convert urlencoded symbol into one byte.\n\n',
                paragraphs: [],
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
                description: 'Convert one byte into urlencoded symbol.\n\n',
                summary: 'Convert one byte into urlencoded symbol.\n\n',
                paragraphs: [],
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
                description: 'Convert ECB exchange rates in CSV format to the list of currency-rate pairs.\nhttps://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html\n\n',
                summary: 'Convert ECB exchange rates in CSV format to the list of currency-rate pairs.\nhttps://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html\n\n',
                paragraphs: [],
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
        expect(actual).to.deep.equal(expected);
    });
});
