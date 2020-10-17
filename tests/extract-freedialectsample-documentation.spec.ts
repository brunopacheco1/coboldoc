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
            license: 'LGPL-3.0',
            fileDescription: 'Short sample.',
            modules: [{
                description: 'The first program.  \nTrying to see **what** happens to    huge text.\n',
                summary: "the first program summary.",
                line: 14,
                name: 'first-program',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                description: 'The second program',
                summary: 'The second program',
                line: 44,
                name: 'second-program',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                description: 'The third program',
                summary: 'The third program',
                line: 77,
                name: 'third-program',
                paragraphs: [],
                return: undefined,
                params: []
            }],
            functions: [{
                name: 'firstprogramfunction',
                line: 110,
                description: 'first program function\n',
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
                name: 'secondprogramfunction',
                line: 127,
                description: 'second program function\n',
                summary: 'second program function\n',
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
                name: 'thirdprogramfunction',
                line: 144,
                description: 'third program function\n',
                summary: 'third program function\n',
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
