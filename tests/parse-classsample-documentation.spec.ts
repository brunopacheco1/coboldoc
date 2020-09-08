import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { TemplateEngine, TemplateEngineImpl } from '../src/services/template-engine';
import { Documentation } from '../src/model/documentation';
import { Format } from '../src/model/format';
import * as fs from 'fs';

describe('Parsing classsample.cbl to doc file', () => {
    const doc: Documentation = {
        fileName: 'classsample.cbl',
        author: undefined,
        fileDescription: undefined,
        license: undefined,
        functions: [],
        modules: [],
        classes: [{
            line: 10,
            name: 'SuccessException',
            example: undefined,
            methods: [{
                description: '\nSuccessException - constructor with no assert message\n',
                summary: '\nSuccessException - constructor with no assert message\n',
                example: undefined,
                line: 19,
                name: 'New',
                paragraphs: [],
                params: [],
                return: undefined,
            }, {
                description: '\nSuccessException - constructor with assert message\n',
                summary: '\nSuccessException - constructor with assert message\n',
                example: undefined,
                line: 27,
                name: 'New',
                paragraphs: [],
                params: [{
                    name: 'msg',
                    description: 'message',
                    type: undefined,
                }],
                return: undefined,
            }],
            properties: [],
            description: '\nSuccessException - Exception thrown if the test case ends with a sucess condition\n',
            summary: '\nSuccessException - Exception thrown if the test case ends with a sucess condition\n',
        }, {
            line: 35,
            name: 'AssertionFailedException',
            example: undefined,
            methods: [{
                description: '\nAssertionFailedException - constructor with no assert message\n',
                summary: '\nAssertionFailedException - constructor with no assert message\n',
                example: undefined,
                line: 48,
                name: 'New',
                paragraphs: [],
                params: [],
                return: undefined,
            }, {
                description: '\nAssertionFailedException - constructor with assert message\n',
                summary: '\nAssertionFailedException - constructor with assert message\n',
                example: undefined,
                line: 57,
                name: 'New',
                paragraphs: [],
                params: [{
                    name: 'msg',
                    description: 'message',
                    type: undefined,
                }],
                return: undefined,
            }],
            properties: [{
                line: 43,
                name: 'AssertMessage',
                description: '\nAssertMessage - Property for assert message\n',
                type: undefined,
            }],
            description: '\nAssertionFailedException - Thrown when an assertion failed.\n',
            summary: '\nAssertionFailedException - Thrown when an assertion failed.\n',
        }, {
            line: 65,
            name: 'InconclusiveException',
            example: undefined,
            methods: [{
                description: '\nInconclusiveException - constructor with assert message\n',
                summary: '\nInconclusiveException - constructor with assert message\n',
                example: undefined,
                line: 77,
                name: 'New',
                paragraphs: [],
                params: [{
                    name: 'msg',
                    description: 'message',
                    type: undefined,
                }],
                return: undefined,
            }, {
                description: '\nInconclusiveException - constructor with assert message\n',
                summary: '\nInconclusiveException - constructor with assert message\n',
                example: undefined,
                line: 85,
                name: 'New',
                paragraphs: [],
                params: [{
                    name: 'msg',
                    description: 'message',
                    type: undefined,
                }],
                return: undefined,
            }],
            properties: [{
                line: 70,
                name: 'AssertMessage',
                description: '\nAssertMessage - Property for assert message\n',
                type: undefined,
            }],
            description: '\nInconclusiveException - Thrown when an Inconclusive failure assert is used.\n',
            summary: '\nInconclusiveException - Thrown when an Inconclusive failure assert is used.\n',
        }, {
            line: 93,
            name: 'AssertErrorException',
            example: undefined,
            methods: [{
                description: '\nAssertErrorException - constructor with no assert message\n',
                summary: '\nAssertErrorException - constructor with no assert message\n',
                example: undefined,
                line: 111,
                name: 'New',
                paragraphs: [],
                params: [],
                return: undefined,
            }, {
                description: '\nAssertErrorException - constructor with assert message\n',
                summary: '\nAssertErrorException - constructor with assert message\n',
                example: undefined,
                line: 120,
                name: 'New',
                paragraphs: [],
                params: [{
                    name: 'msg',
                    description: 'message',
                    type: undefined,
                }],
                return: undefined,
            }, {
                description: '\nAssertErrorException - constructor with assert message\n',
                summary: '\nAssertErrorException - constructor with assert message\n',
                example: undefined,
                line: 129,
                name: 'New',
                paragraphs: [],
                params: [{
                    name: 'msg',
                    description: 'message',
                    type: undefined,
                }, {
                    name: 'cause',
                    description: 'inner exception',
                    type: undefined,
                }],
                return: undefined,
            }],
            properties: [{
                line: 101,
                name: 'AssertMessage',
                description: '\nAssertMessage - Property for assert message\n',
                type: undefined,
            }, {
                line: 106,
                name: 'CauseOfException',
                description: '\nAssertMessage - Property for assert message\n',
                type: undefined,
            }],
            description: '\nAssertErrorException - Thrown when an assertion failed.\n',
            summary: '\nAssertErrorException - Thrown when an assertion failed.\n',
        }],
        changeLogs: [],
    };

    let service: TemplateEngine;

    beforeEach(() => {
        service = new TemplateEngineImpl();
    });

    it('should parse to html', async () => {
        const actual = service.parseDocumentation(Format.HTML, doc);
        const expected = fs.readFileSync('./tests/resources/classsample.cbl.expected.html', 'utf8');
        expect(actual).to.deep.equal(expected);
    });
});
