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
            description: 'The first module.  \nTrying to see **what** happens to    huge text.',
            summary: 'The first module.  \nTrying to see **what** happens to    huge text.',
            line: 13,
            name: 'first-module',
            paragraphs: [],
            return: undefined,
            params: []
        },
        {
            description: 'The second module',
            summary: 'The second module',
            line: 43,
            name: 'second-module',
            paragraphs: [],
            return: undefined,
            params: []
        },
        {
            description: 'The third module',
            summary: 'The third module',
            line: 76,
            name: 'third-module',
            paragraphs: [],
            return: undefined,
            params: []
        }],
        functions: [{
            name: 'firstmodulefunction',
            line: 109,
            description: 'first module function',
            summary: 'first module function',
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
            line: 126,
            description: 'second module function',
            summary: 'second module function',
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
            line: 143,
            description: 'third module function',
            summary: 'third module function',
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

    it('should parse to md', async () => {
        const actual = service.parseDocumentation(Format.MD, doc);
        const expected = fs.readFileSync('./tests/resources/microfocusdialectsample.cbl.expected.md', 'utf8');
        expect(actual).to.deep.equal(expected);
    });

    it('should parse to html', async () => {
        const actual = service.parseDocumentation(Format.HTML, doc);
        const expected = fs.readFileSync('./tests/resources/microfocusdialectsample.cbl.expected.html', 'utf8');
        expect(actual).to.deep.equal(expected);
    });
});
