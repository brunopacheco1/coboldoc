import { injectable } from 'inversify';
import { PreDocumentation, PreCobolObject, PreCobolClass } from '../model/documentation';
import * as fs from 'fs';
import * as path from 'path';
import { CommentStyle, CommentsRegex } from '../model/comment-style';

export interface CommentsExtractor {
    extract(dialect: CommentStyle, filePath: string): PreDocumentation;
}

@injectable()
export class CommentsExtractorImpl implements CommentsExtractor {

    public extract(commentStyle: CommentStyle, filePath: string): PreDocumentation {
        const commentsRegex = CommentsRegex.from(commentStyle);
        const data = fs.readFileSync(filePath, 'utf8');
        const lines = data.split(/\r?\n/);

        let comments: string[] = [];
        const fileComments: string[] = [];
        let isFileComments = false;
        let isComments = false;
        const functions: PreCobolObject[] = [];
        const modules: PreCobolObject[] = [];
        const classes: PreCobolClass[] = [];
        let lineCounter = 1;
        lines.forEach((line) => {
            if (commentsRegex.contentRegex.test(line) && (isFileComments || isComments)) {
                if (isFileComments) {
                    fileComments.push(line);
                }
                if (isComments) {
                    comments.push(line);
                }
            } else if (commentsRegex.fileCommentsRegex.test(line)) {
                isFileComments = !isFileComments;
                isComments = false;
                fileComments.push(line);
            } else if (commentsRegex.documentationRegex.test(line)) {
                isComments = !isComments;
                isFileComments = false;
                comments.push(line);
            } else if (/^\s*function-id\.{0,1}.+/i.test(line)) {
                functions.push({
                    name: this._cleanName(line),
                    line: lineCounter,
                    comments: this._cleanComments(comments, commentsRegex),
                });
                comments = [];
            } else if (/^\s*program-id\.{0,1}.+/i.test(line)) {
                modules.push({
                    name: this._cleanName(line),
                    line: lineCounter,
                    comments: this._cleanComments(comments, commentsRegex),
                });
                comments = [];
            } else if (/^\s*class-id\.{0,1}.+/i.test(line)) {
                classes.push({
                    name: this._cleanName(line),
                    line: lineCounter,
                    comments: this._cleanComments(comments, commentsRegex),
                    methods: [],
                    properties: [],
                });
                comments = [];
            } else if (/^\s*\d+.+/i.test(line)) {
                const clazz = classes[classes.length - 1];
                if (!!clazz) {
                    clazz.properties.push({
                        name: this._cleanName(line),
                        line: lineCounter,
                        comments: this._cleanComments(comments, commentsRegex),
                    });
                    comments = [];
                }
            } else if (/^\s*method-id\.{0,1}.+/i.test(line)) {
                const clazz = classes[classes.length - 1];
                if (!!clazz) {
                    clazz.methods.push({
                        name: this._cleanName(line),
                        line: lineCounter,
                        comments: this._cleanComments(comments, commentsRegex),
                    });
                }
                comments = [];
            }
            lineCounter++;
        });

        return {
            fileName: path.basename(filePath),
            fileComments: this._cleanComments(fileComments, commentsRegex),
            functions: functions,
            modules: modules,
            classes: classes,
        };
    }

    private _cleanName(line: string): string {
        return line.trim().split(' ')[1].replace(/\./, '');
    }

    private _cleanComments(comments: string[], commentsRegex: CommentsRegex): string {
        return comments
            .map(comment => comment.replace(commentsRegex.cleanRegex, ''))
            .join('\n');
    }
}
