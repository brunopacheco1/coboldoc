import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { FreeDialectExtractor, FreeDialectExtractorImpl } from '../../src/services/free-dialect-extractor';
import { Documentation } from '../../src/model/documentation';
import { Dialect } from '../../src/model/dialect';

describe('FreeDialectExtractor', () => {
    let service: FreeDialectExtractor;

    beforeEach(() => {
        service = new FreeDialectExtractorImpl();
    });

    it('should extract documention from freedialectsample.cbl', async () => {
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
                paragraphs: [
                    { name: '0001-FIRSTMODULE-MAIN', line: 26 },
                    { name: '0002-FIRSTMODULE-NEWPARA', line: 30 }
                ]
            },
            {
                description: 'The second module',
                line: 43,
                name: 'second-module',
                paragraphs: [
                    { name: '0001-SECONDMODULE-MAIN', line: 58 },
                    { name: '0002-SECONDMODULE-NEWPARA', line: 62 }
                ]
            },
            {
                description: 'The third module',
                line: 76,
                name: 'third-module',
                paragraphs: [
                    { name: '0001-THIRDMODULE-MAIN', line: 91 },
                    { name: '0002-THIRDMODULE-NEWPARA', line: 95 }
                ]
            }],
            functions: [{
                name: 'firstmodulefunction',
                line: 108,
                description: 'first module function',
                params: [{
                    name: 'firstarg',
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
        expect(actual).to.be.equal(expected);
    });
});