import { injectable } from 'inversify';
import { Documentation, CobolFunction, CobolModule, Parameter, Return } from '../model/documentation';
import * as fs from 'fs';
import { DocumentationExtractor } from './documentation-extractor';
import * as path from 'path';
import { Dialect } from '../model/dialect';

export interface FreeDialectExtractor extends DocumentationExtractor {
}

@injectable()
export class FreeDialectExtractorImpl implements FreeDialectExtractor {

    constructor() { }

    public extract(filePath: string): Documentation {
        const data = fs.readFileSync(filePath, 'utf8');
        const lines = data.split(/\r?\n/);

        let comments: string[] = [];
        let fileComments: string[] = [];
        let isFileComments = false;
        let isComments = false;
        let functions: CobolFunction[] = [];
        let modules: CobolModule[] = [];
        let lineCounter = 1;
        lines.forEach((line) => {
            if (/^\*\>[^*].*/.test(line)) {
                if (isFileComments) {
                    fileComments.push(line);
                }
                if (isComments) {
                    comments.push(line);
                }
            } else if (/^\*\>\*\*.*/.test(line)) {
                fileComments.push(line);
                isFileComments = !isFileComments;
                isComments = false;
            } else if (/^\*\>\*.*/.test(line)) {
                comments.push(line);
                isComments = !isComments;
                isFileComments = false;
            } else if (/^\s*function-id\..+/i.test(line)) {
                const functionName = line.split(".")[1].trim();
                let description: string | undefined = undefined;
                let params: Parameter[] = [];
                let returnObj: Return | undefined = undefined;
                [description, params, returnObj] = this._extractFunctionDetails(comments);
                const cobolFunction: CobolFunction = {
                    description: description,
                    line: lineCounter,
                    name: functionName,
                    params: params,
                    return: returnObj
                };
                functions.push(cobolFunction);
                comments = [];
            } else if (/^\s*program-id\..+/i.test(line)) {
                const moduleName = line.split(".")[1].trim();
                let description: string | undefined = undefined;
                [description] = this._extractModuleDetails(comments);
                const cobolModule: CobolModule = {
                    description: description,
                    line: lineCounter,
                    name: moduleName,
                };
                modules.push(cobolModule);
                comments = [];
            }
            lineCounter++;
        });

        let fileDescription: string = '';
        let author: string | undefined;
        let license: string | undefined;
        [fileDescription, author, license] = this._extractFileDetails(fileComments);

        return {
            dialect: Dialect.FREE,
            fileName: path.basename(filePath),
            fileDescription: fileDescription,
            author: author,
            license: license,
            functions: functions,
            modules: modules,
        };
    }

    private _extractFunctionDetails(comments: string[]): [string, Parameter[], Return | undefined] {
        const params: Parameter[] = [];
        const returnObj: Return | undefined = undefined;
        let rawDescription = '';
        comments.forEach(line => {
            if (/@param/.test(line)) {

            } else if (/@return/.test(line)) {

            } else {
                rawDescription += '\n' + line;
            }
        });

        return [this._extractDescription(rawDescription, /\*\>\*/g), params, returnObj];
    }

    private _extractModuleDetails(comments: string[]): [string] {
        return [this._extractDescription(comments.join('\n'), /\*\>\*/g)];
    }

    private _extractFileDetails(comments: string[]): [string, string | undefined, string | undefined] {
        let author: string | undefined = undefined;
        let license: string | undefined = undefined;
        let rawDescription = '';
        comments.forEach(line => {
            if (/@license/.test(line)) {
                license = line.split('@license')[1].trim();
            } else if (/@author/.test(line)) {
                author = line.split('@author')[1].trim();
            } else {
                rawDescription += '\n' + line;
            }
        });

        return [this._extractDescription(rawDescription, /\*\>\*\*/g), author, license];
    }

    private _extractDescription(rawText: string, commentDelimiterRegex: RegExp): string {
        return rawText
            .replace(commentDelimiterRegex, '')
            .replace(/\*\>/g, '')
            .replace(/\n/g, ' ')
            .replace(/\s\s+/g, ' ')
            .trim();
    }
}
