import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { FreeDialectExtractor, FreeDialectExtractorImpl } from '../../src/services/free-dialect-extractor';
import { Documentation } from '../../src/model/documentation';
import { Dialect } from '../../src/model/dialect';

describe('freedialectsample.cbl to documentation', () => {
    let service: FreeDialectExtractor;

    beforeEach(() => {
        service = new FreeDialectExtractorImpl();
    });

    it('should extract', async () => {
        const actual: Documentation = service.extract('./tests/resources/freedialectsample.cbl');
        const expected: Documentation = {
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
        expect(actual).to.deep.equal(expected);
    });
});
