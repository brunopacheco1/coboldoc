import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { CommentsExtractor, CommentsExtractorImpl } from '../src/services/comments-extractor';
import { TagCommentsParser, TagCommentsParserImpl } from '../src/services/tag-comments-parser';
import { Documentation } from '../src/model/documentation';
import { CommentStyle } from '../src/model/comment-style';

describe('freedialectsample.cbl to documentation', () => {
    let commentsExtractor: CommentsExtractor;
    let commentsParser: TagCommentsParser;

    beforeEach(() => {
        commentsExtractor = new CommentsExtractorImpl();
        commentsParser = new TagCommentsParserImpl();
    });

    it('should extract', async () => {
        const preDoc = commentsExtractor.extract(CommentStyle.FREE, './tests/resources/freedialectsample.cbl');
        const actual: Documentation = commentsParser.parse(preDoc);
        const expected: Documentation = {
            fileName: 'freedialectsample.cbl',
            author: 'Bruno Pacheco (https://brunopacheco1.github.io/)\n',
            license: 'LGPL-3.0\n',
            fileDescription: 'Short sample.',
            modules: [{
                description: '\nThe first module.  \nTrying to see **what** happens to    huge text.\n',
                summary: "the first module summary.\n",
                line: 14,
                name: 'first-module',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                description: '\nThe second module\n',
                summary: '\nThe second module\n',
                line: 44,
                name: 'second-module',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                description: '\nThe third module\n',
                summary: '\nThe third module\n',
                line: 77,
                name: 'third-module',
                paragraphs: [],
                return: undefined,
                params: []
            }],
            functions: [{
                name: 'firstmodulefunction',
                line: 110,
                description: '\nfirst module function\n',
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
                description: '\nsecond module function\n',
                summary: '\nsecond module function\n',
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
                description: '\nthird module function\n',
                summary: '\nthird module function\n',
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
        expect(actual).to.deep.equal(expected);
    });
});
