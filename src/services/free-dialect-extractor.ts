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
                    return: returnObj,
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
                    paragraphs: [],
                };
                modules.push(cobolModule);
                comments = [];
            }
            lineCounter++;
        });

        let fileDescription: string | undefined;
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

    private _extractFunctionDetails(comments: string[]): [string | undefined, Parameter[], Return | undefined] {
        let rawText = this._cleanComments(comments);
        let pieces = rawText.split('@');
        const params: Parameter[] = [];
        let returnObj: Return | undefined = undefined;
        let description: string | undefined = undefined;
        pieces.forEach((piece, index) => {
            if (index === 0) {
                description = piece.trim();
            } else if (/^param/.test(piece)) {
                piece = piece.substring(piece.indexOf(' ') + 1);
                let paramType: string | undefined = undefined;
                let match = piece.match(/\{(.+)\}/);
                if (!!match) {
                    paramType = match[1];
                    piece = piece.replace(/\{(.+)\}/, '').trim();
                }
                const paramName = piece.substring(0, piece.indexOf(' ')).trim();
                const paramDescription = piece.substring(piece.indexOf(' ') + 1).trim();
                params.push({
                    name: paramName,
                    description: paramDescription,
                    type: paramType,
                });
            } else if (/^return/.test(piece)) {
                let returnType: string | undefined = undefined;
                let match = piece.match(/\{(.+)\}/);
                if (!!match) {
                    returnType = match[1];
                    piece = piece.replace(/\{(.+)\}/, '').trim();
                }
                const returnDescription = piece.substring(piece.indexOf(' ') + 1).trim();
                returnObj = {
                    description: returnDescription,
                    type: returnType,
                };
            }
        });

        return [description, params, returnObj];
    }

    private _extractModuleDetails(comments: string[]): [string] {
        return [this._cleanComments(comments)];
    }

    private _extractFileDetails(comments: string[]): [string | undefined, string | undefined, string | undefined] {
        let rawText = this._cleanComments(comments);
        let pieces = rawText.split('@');
        let author: string | undefined = undefined;
        let license: string | undefined = undefined;
        let description: string | undefined = undefined;
        pieces.forEach((piece, index) => {
            if (index === 0) {
                description = piece.trim();
            } else if (/^license/.test(piece)) {
                license = piece.substring(piece.indexOf(' ')).trim();
            } else if (/^author/.test(piece)) {
                author = piece.substring(piece.indexOf(' ')).trim();
            }
        });

        return [description, author, license];
    }

    private _cleanComments(comments: string[]): string {
        return comments.join('\n')
            .replace(/\*\>\**/g, '')
            .replace(/\n/g, ' ')
            .replace(/\s\s+/g, ' ')
            .trim();
    }
}
