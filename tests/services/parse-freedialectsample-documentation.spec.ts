import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { TemplateEngine, TemplateEngineImpl } from '../../src/services/template-engine';
import { Documentation } from '../../src/model/documentation';
import { Dialect } from '../../src/model/dialect';
import { Format } from '../../src/model/format';
import * as fs from 'fs';

describe('freedialectsample.cbl to freedialectsample.cbl.md', () => {
    let service: TemplateEngine;

    beforeEach(() => {
        service = new TemplateEngineImpl();
    });

    it('should parse', async () => {
        const doc: Documentation = {
            dialect: Dialect.FREE,
            fileName: 'freedialectsample.cbl',
            author: 'Bruno Pacheco (https://brunopacheco1.github.io/)',
            license: 'LGPL-3.0',
            fileDescription: 'Short sample.',
            modules: [{
                description: 'The first module. Trying to see what happens to huge text.',
                line: 13,
                name: 'first-module',
                paragraphs: []
            },
            {
                description: 'The second module',
                line: 43,
                name: 'second-module',
                paragraphs: []
            },
            {
                description: 'The third module',
                line: 76,
                name: 'third-module',
                paragraphs: []
            }],
            functions: [{
                name: 'firstmodulefunction',
                line: 108,
                description: 'first module function',
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
                line: 125,
                description: 'second module function',
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
                line: 142,
                description: 'third module function',
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

        const actual = service.parse(Format.MD, doc);
        const expected = fs.readFileSync('./tests/resources/freedialectsample.cbl.expected.md', 'utf8');
        expect(actual.text).to.deep.equal(expected);
    });
});
