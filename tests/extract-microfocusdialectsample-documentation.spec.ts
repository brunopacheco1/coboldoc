import 'reflect-metadata';
import 'mocha';
import { expect } from 'chai';
import { CommentsExtractor, CommentsExtractorImpl } from '../src/services/comments-extractor';
import { MsdnCommentsParser, MsdnCommentsParserImpl } from '../src/services/msdn-comments-parser';
import { Documentation } from '../src/model/documentation';
import { CommentStyle } from '../src/model/comment-style';

describe('microfocusdialectsample.cbl to documentation', () => {
    let commentsExtractor: CommentsExtractor;
    let commentsParser: MsdnCommentsParser;

    beforeEach(() => {
        commentsExtractor = new CommentsExtractorImpl();
        commentsParser = new MsdnCommentsParserImpl();
    });

    it('should extract', async () => {
        const preDoc = commentsExtractor.extract(CommentStyle.MICROFOCUS, './tests/resources/microfocusdialectsample.cbl');
        const actual: Documentation = commentsParser.parse(preDoc);
        const expected: Documentation = {
            fileName: 'microfocusdialectsample.cbl',
            author: 'Bruno Pacheco (https://brunopacheco1.github.io/)',
            license: 'LGPL-3.0',
            fileDescription: 'Short sample.',
            modules: [{
                description: 'The <c>first</c> program.  \nTrying to see **what** happens to    huge text.',
                summary: 'Any remark',
                example: '\n<code>\nCALL \"first-program\" USING BY CONTENT WS-FIRSTMODULE.\n</code>\n',
                line: 19,
                name: 'first-program',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                description: 'The second program',
                summary: 'The second program',
                example: undefined,
                line: 49,
                name: 'second-program',
                paragraphs: [],
                return: undefined,
                params: []
            },
            {
                description: 'The third program',
                summary: 'The third program',
                example: undefined,
                line: 82,
                name: 'third-program',
                paragraphs: [],
                return: undefined,
                params: []
            }],
            functions: [{
                name: 'firstprogramfunction',
                line: 117,
                description: 'first program function accepts <paramref name=\"first-arg\"/> as an arg.\n<seealso cref=\"secondprogramfunction\"/>\n',
                summary: 'first program function accepts <paramref name=\"first-arg\"/> as an arg.\n<seealso cref=\"secondprogramfunction\"/>\n',
                example: undefined,
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
                name: 'secondprogramfunction',
                line: 134,
                description: 'second program function',
                summary: 'second program function',
                example: undefined,
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
                line: 151,
                description: 'third program function',
                summary: 'third program function',
                example: undefined,
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
