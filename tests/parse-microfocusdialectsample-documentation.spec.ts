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
            description: 'The <c>first</c> module.  \nTrying to see **what** happens to    huge text.',
            summary: 'Any remark',
            example: '\n<code>\nCALL \"first-module\" USING BY CONTENT WS-FIRSTMODULE.\n</code>\n',
            line: 19,
            name: 'first-module',
            paragraphs: [],
            return: undefined,
            params: []
        },
        {
            description: 'The second module',
            summary: 'The second module',
            example: undefined,
            line: 49,
            name: 'second-module',
            paragraphs: [],
            return: undefined,
            params: []
        },
        {
            description: 'The third module',
            summary: 'The third module',
            example: undefined,
            line: 82,
            name: 'third-module',
            paragraphs: [],
            return: undefined,
            params: []
        }],
        functions: [{
            name: 'firstmodulefunction',
            line: 117,
            description: 'first module function accepts <paramref name=\"first-arg\"/> as an arg.\n<seealso cref=\"secondmodulefunction\"/>\n',
            summary: 'first module function accepts <paramref name=\"first-arg\"/> as an arg.\n<seealso cref=\"secondmodulefunction\"/>\n',
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
            name: 'secondmodulefunction',
            line: 134,
            description: 'second module function',
            summary: 'second module function',
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
            name: 'thirdmodulefunction',
            line: 151,
            description: 'third module function',
            summary: 'third module function',
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
