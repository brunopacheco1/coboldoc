import { injectable } from 'inversify';
import { PreDocumentation, PreModuleOrFunction } from '../model/documentation';
import * as fs from 'fs';
import * as path from 'path';
import { CommentsExtractor } from './comments-extractor';

export interface FreeCommentsExtractor extends CommentsExtractor {
}

@injectable()
export class FreeCommentsExtractorImpl implements FreeCommentsExtractor {

    public extract(filePath: string): PreDocumentation {
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
            if (/^\s*\*\>[^*].*/.test(line)) {
                if (isFileComments) {
                    fileComments.push(line);
                }
                if (isComments) {
                    comments.push(line);
                }
            } else if (/^\s*\*\>\*\*.*/.test(line)) {
                isFileComments = !isFileComments;
                isComments = false;
            } else if (/^\s*\*\>\*.*/.test(line)) {
                isComments = !isComments;
                isFileComments = false;
            } else if (/^\s*function-id\..+/i.test(line)) {
                functions.push({
                    name: line.split(".")[1].trim(),
                    line: lineCounter,
                    comments: this._cleanComments(comments),
                });
                comments = [];
            } else if (/^\s*program-id\..+/i.test(line)) {
                modules.push({
                    name: line.split(".")[1].trim(),
                    line: lineCounter,
                    comments: this._cleanComments(comments),
                });
                comments = [];
            }
            lineCounter++;
        });

        return {
            fileName: path.basename(filePath),
            fileComments: this._cleanComments(fileComments),
            functions: functions,
            modules: modules,
        };
    }

    private _cleanComments(comments: string[]): string {
        return comments
            .map(comment => comment.replace(/^\s*\*>\**\s*/, ''))
            .join('\n');
    }
}
