import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { TemplateEngine, TemplateEngineImpl } from '../src/services/template-engine';
import { Documentation } from '../src/model/documentation';
import { Format } from '../src/model/format';
import * as fs from 'fs';

describe('Parsing microfocusdialectsample.cbl to doc file', () => {
    const doc: Documentation = {
        fileName: 'microfocusdialectsample.cbl',
        author: 'Bruno Pacheco (https://brunopacheco1.github.io/)',
        license: 'LGPL-3.0',
        fileDescription: 'Short sample.',
        modules: [{
            description: 'The <c>first</c> program.  \nTrying to see **what** happens to    huge text.',
            summary: 'Any remark',
            example: '\n<code>\nCALL \"first-program\" USING BY CONTENT WS-FIRSTMODULE.\n</code>\n',
            line: 19,
            name: 'first-program',
            paragraphs: [],
            return: undefined,
            params: []
        },
        {
            description: 'The second program',
            summary: 'The second program',
            example: undefined,
            line: 49,
            name: 'second-program',
            paragraphs: [],
            return: undefined,
            params: []
        },
        {
            description: 'The third program',
            summary: 'The third program',
            example: undefined,
            line: 82,
            name: 'third-program',
            paragraphs: [],
            return: undefined,
            params: []
        }],
        functions: [{
            name: 'firstprogramfunction',
            line: 117,
            description: 'first program function accepts <paramref name=\"first-arg\"/> as an arg.\n<seealso cref=\"secondprogramfunction\"/>\n',
            summary: 'first program function accepts <paramref name=\"first-arg\"/> as an arg.\n<seealso cref=\"secondprogramfunction\"/>\n',
            example: undefined,
            paragraphs: [],
            params: [{
                name: 'first-arg',
                description: 'First arg',
                type: 'PIC 9'
            }, {
                name: 'second-arg',
                description: 'Second arg',
                type: 'PIC 9'
            }],
            return: {
                description: 'First return',
                type: 'PIC 9'
            }
        }, {
            name: 'secondprogramfunction',
            line: 134,
            description: 'second program function',
            summary: 'second program function',
            example: undefined,
            paragraphs: [],
            params: [{
                name: 'secondarg',
                description: 'Second arg',
                type: 'PIC 9'
            }],
            return: {
                description: 'Second return',
                type: 'PIC 9'
            }
        }, {
            name: 'thirdprogramfunction',
            line: 151,
            description: 'third program function',
            summary: 'third program function',
            example: undefined,
            paragraphs: [],
            params: [{
                name: 'thirdarg',
                description: 'Third arg',
                type: 'PIC 9'
            }],
            return: {
                description: 'Third return',
                type: 'PIC 9'
            }
        }]
    };

    let service: TemplateEngine;

    beforeEach(() => {
        service = new TemplateEngineImpl();
    });

    it('should parse to msdn', async () => {
        const actual = service.parseDocumentation(Format.MSDN, doc);
        const expected = fs.readFileSync('./tests/resources/microfocusdialectsample.cbl.expected.xml', 'utf8');
        expect(actual).to.deep.equal(expected);
    });
});
