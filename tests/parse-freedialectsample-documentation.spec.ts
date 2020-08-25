import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { TemplateEngine, TemplateEngineImpl } from '../src/services/template-engine';
import { Documentation } from '../src/model/documentation';
import { Format } from '../src/model/format';
import * as fs from 'fs';

describe('Parsing freedialectsample.cbl to doc file', () => {
    const doc: Documentation = {
        fileName: 'freedialectsample.cbl',
        author: 'Bruno Pacheco (https://brunopacheco1.github.io/)\n',
        license: 'LGPL-3.0',
        fileDescription: 'Short sample.',
        modules: [{
            description: 'The first module.  \nTrying to see **what** happens to    huge text.\n',
            summary: "the first module summary.",
            line: 14,
            name: 'first-module',
            paragraphs: [],
            return: undefined,
            params: []
        },
        {
            description: 'The second module',
            summary: 'The second module',
            line: 44,
            name: 'second-module',
            paragraphs: [],
            return: undefined,
            params: []
        },
        {
            description: 'The third module',
            summary: 'The third module',
            line: 77,
            name: 'third-module',
            paragraphs: [],
            return: undefined,
            params: []
        }],
        functions: [{
            name: 'firstmodulefunction',
            line: 110,
            description: 'first module function\n',
            summary: "the first function summary.\n",
            paragraphs: [],
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
            description: 'second module function\n',
            summary: 'second module function\n',
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
            line: 144,
            description: 'third module function\n',
            summary: 'third module function\n',
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
        }],
        changeLogs: [],
        classes: [],
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
