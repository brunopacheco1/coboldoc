import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { CommentsExtractor, CommentsExtractorImpl } from '../../src/services/comments-extractor';
import { XmlCommentsParser, XmlCommentsParserImpl } from '../../src/services/xml-comments-parser';
import { Documentation } from '../../src/model/documentation';
import { Dialect } from '../../src/model/dialect';

describe('microfocusdialectsample.cbl to documentation', () => {
    let commentsExtractor: CommentsExtractor;
    let commentsParser: XmlCommentsParser;

    beforeEach(() => {
        commentsExtractor = new CommentsExtractorImpl();
        commentsParser = new XmlCommentsParserImpl();
    });

    it('should extract', async () => {
        const preDoc = commentsExtractor.extract(Dialect.MICROFOCUS, './tests/resources/microfocusdialectsample.cbl');
        const actual: Documentation = commentsParser.parse(preDoc);
        const expected: Documentation = {
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
        expect(actual).to.deep.equal(expected);
    });
});
