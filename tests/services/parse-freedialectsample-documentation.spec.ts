import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { TemplateEngine, TemplateEngineImpl } from '../../src/services/template-engine';
import { Documentation } from '../../src/model/documentation';
import { Format } from '../../src/model/format';
import * as fs from 'fs';

describe('Parsing freedialectsample.cbl to doc file', () => {
    const doc: Documentation = {
        fileName: 'freedialectsample.cbl',
        author: 'Bruno Pacheco (https://brunopacheco1.github.io/)',
        license: 'LGPL-3.0',
        fileDescription: 'Short sample.',
        modules: [{
            description: 'The first module. Trying to see what happens to huge text.',
            summary: "the first module summary.",
            line: 14,
            name: 'first-module',
            paragraphs: []
        },
        {
            description: 'The second module',
            summary: undefined,
            line: 44,
            name: 'second-module',
            paragraphs: []
        },
        {
            description: 'The third module',
            summary: undefined,
            line: 77,
            name: 'third-module',
            paragraphs: []
        }],
        functions: [{
            name: 'firstmodulefunction',
            line: 110,
            description: 'first module function',
            summary: "the first function summary.",
            params: [{
                name: 'first-arg',
                description: 'First arg',
                type: 'PIC 9'
            }],
            return: {
                description: 'First return',
                type: 'PIC 9'
            }
        }, {
            name: 'secondmodulefunction',
            line: 127,
            description: 'second module function',
            summary: undefined,
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
            line: 144,
            description: 'third module function',
            summary: undefined,
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

    it('should parse to md', async () => {
        const actual = service.parseDocumentation(Format.MD, doc);
        const expected = fs.readFileSync('./tests/resources/freedialectsample.cbl.expected.md', 'utf8');
        expect(actual).to.deep.equal(expected);
    });

    it('should parse to html', async () => {
        const actual = service.parseDocumentation(Format.HTML, doc);
        const expected = fs.readFileSync('./tests/resources/freedialectsample.cbl.expected.html', 'utf8');
        expect(actual).to.deep.equal(expected);
    });
});
