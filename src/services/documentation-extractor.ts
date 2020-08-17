import { injectable } from 'inversify';
import { Documentation, CobolFunction, CobolModule, Parameter, Return } from '../model/documentation';
import * as fs from 'fs';
import * as path from 'path';

export interface DocumentationExtractor {
    extract(filePath: string): Documentation;
}

@injectable()
export class DocumentationExtractorImpl implements DocumentationExtractor {

    constructor() { }

    public extract(filePath: string): Documentation {
        const data = fs.readFileSync(filePath, 'utf8');
        const lines = data.split(/\r?\n/);

        let comments: string[] = [];
        const fileComments: string[] = [];
        let isFileComments = false;
        let isComments = false;
        const functions: CobolFunction[] = [];
        const modules: CobolModule[] = [];
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
                fileComments.push(line);
                isFileComments = !isFileComments;
                isComments = false;
            } else if (/^\s*\*\>\*.*/.test(line)) {
                comments.push(line);
                isComments = !isComments;
                isFileComments = false;
            } else if (/^\s*function-id\..+/i.test(line)) {
                const functionName = line.split(".")[1].trim();
                let description: string | undefined = undefined;
                let summary: string | undefined = undefined;
                let params: Parameter[] = [];
                let returnObj: Return | undefined = undefined;
                [description, summary, params, returnObj] = this._extractFunctionDetails(comments);
                const cobolFunction: CobolFunction = {
                    description: description,
                    summary: summary,
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
                let summary: string | undefined = undefined;
                [description, summary] = this._extractModuleDetails(comments);
                const cobolModule: CobolModule = {
                    description: description,
                    summary: summary,
                    line: lineCounter,
                    name: moduleName,
                    paragraphs: [],
                };
                modules.push(cobolModule);
                comments = [];
            }
            lineCounter++;
        });

        const [fileDescription, author, license] = this._extractFileDetails(fileComments);

        return {
            fileName: path.basename(filePath),
            fileDescription: fileDescription,
            author: author,
            license: license,
            functions: functions,
            modules: modules,
        };
    }

    private _extractFunctionDetails(comments: string[]): [string | undefined, string | undefined, Parameter[], Return | undefined] {
        const rawText = this._cleanComments(comments);
        const pieces = rawText.split('@');
        const params: Parameter[] = [];
        let returnObj: Return | undefined = undefined;
        let description: string | undefined = undefined;
        let summary: string | undefined = undefined;
        pieces.forEach((piece, index) => {
            if (index === 0) {
                description = piece.trim();
            } else if (/^summary/.test(piece)) {
                summary = piece.substring(piece.indexOf(' ')).trim();
            } else if (/^param/.test(piece)) {
                piece = piece.substring(piece.indexOf(' ') + 1);
                let paramType: string | undefined = undefined;
                const match = piece.match(/\{(.+)\}/);
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
                const match = piece.match(/\{(.+)\}/);
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

        return [description, summary, params, returnObj];
    }

    private _extractModuleDetails(comments: string[]): [string | undefined, string | undefined] {
        const rawText = this._cleanComments(comments);
        const pieces = rawText.split('@');
        let description: string | undefined = undefined;
        let summary: string | undefined = undefined;
        pieces.forEach((piece, index) => {
            if (index === 0) {
                description = piece.trim();
            } else if (/^summary/.test(piece)) {
                summary = piece.substring(piece.indexOf(' ')).trim();
            }
        });

        return [description, summary];
    }

    private _extractFileDetails(comments: string[]): [string | undefined, string | undefined, string | undefined] {
        const rawText = this._cleanComments(comments);
        const pieces = rawText.split('@');
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
            .replace(/\*\>\**\s*/g, '')
            .replace(/\r/g, ' ')
            .replace(/\n/g, ' ')
            .replace(/\s\s+/g, ' ')
            .trim();
    }
}
