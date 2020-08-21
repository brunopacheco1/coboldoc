import { injectable } from 'inversify';
import { PreDocumentation, PreModuleOrFunction } from '../model/documentation';
import * as fs from 'fs';
import * as path from 'path';
import { Dialect, CommentsRegex } from '../model/dialect';

export interface CommentsExtractor {
    extract(dialect: Dialect, filePath: string): PreDocumentation;
}

@injectable()
export class CommentsExtractorImpl implements CommentsExtractor {

    public extract(dialect: Dialect, filePath: string): PreDocumentation {
        const commentsRegex = CommentsRegex.from(dialect);
        const data = fs.readFileSync(filePath, 'utf8');
        const lines = data.split(/\r?\n/);

        let comments: string[] = [];
        const fileComments: string[] = [];
        let isFileComments = false;
        let isComments = false;
        const functions: PreModuleOrFunction[] = [];
        const modules: PreModuleOrFunction[] = [];
        let lineCounter = 1;
        lines.forEach((line) => {
            if (commentsRegex.contentRegex.test(line)) {
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
            }
            lineCounter++;
        });

        return {
            fileName: path.basename(filePath),
            fileComments: this._cleanComments(fileComments, commentsRegex),
            functions: functions,
            modules: modules,
        };
    }

    private _cleanComments(comments: string[], commentsRegex: CommentsRegex): string {
        return comments
            .map(comment => comment.replace(commentsRegex.cleanRegex, ''))
            .join('\n');
    }
}
