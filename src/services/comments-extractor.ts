import { injectable } from 'inversify';
import { PreDocumentation, PreCobolFunction, PreCobolClass } from '../model/documentation';
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
        const functions: PreCobolFunction[] = [];
        const modules: PreCobolFunction[] = [];
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
            } else if (commentsRegex.documentationRegex.test(line)) {
                isComments = !isComments;
                isFileComments = false;
            } else if (/^\s*function-id\..+/i.test(line)) {
                functions.push({
                    name: line.split(".")[1].trim(),
                    line: lineCounter,
                    comments: this._cleanComments(comments, commentsRegex),
                });
                comments = [];
            } else if (/^\s*program-id\..+/i.test(line)) {
                modules.push({
                    name: line.split(".")[1].trim(),
                    line: lineCounter,
                    comments: this._cleanComments(comments, commentsRegex),
                });
                comments = [];
            } else if (/^\s*class-id.+/i.test(line)) {
                classes.push({
                    name: line.split('.')[1].trim().split(' ')[0].trim(),
                    line: lineCounter,
                    comments: this._cleanComments(comments, commentsRegex),
                    methods: [],
                });
                comments = [];
            } else if (/^\s*method-id.+/i.test(line)) {
                const clazz = classes[classes.length - 1];
                clazz.methods.push({
                    name: line.split('.')[1].trim().split(' ')[0].trim(),
                    line: lineCounter,
                    comments: this._cleanComments(comments, commentsRegex),
                });
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

    private _cleanComments(comments: string[], commentsRegex: CommentsRegex): string {
        return comments
            .map(comment => comment.replace(commentsRegex.cleanRegex, ''))
            .join('\n');
    }
}
