import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { DocumentationExtractor, DocumentationExtractorImpl } from '../../src/services/documentation-extractor';
import { Documentation } from '../../src/model/documentation';

describe('freedialectsample.cbl to documentation', () => {
    let service: DocumentationExtractor;

    beforeEach(() => {
        service = new DocumentationExtractorImpl();
    });

    it('should extract', async () => {
        const actual: Documentation = service.extract('./tests/resources/freedialectsample.cbl');
        const expected: Documentation = {
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
        expect(actual).to.deep.equal(expected);
    });
});
