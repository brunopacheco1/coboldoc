import { injectable } from 'inversify';
import { Documentation, ModuleOrFunction, Parameter, Return } from '../model/documentation';
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
        const functions: ModuleOrFunction[] = [];
        const modules: ModuleOrFunction[] = [];
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
                functions.push(this._extractModuleOrFunction(line, lineCounter, comments));
                comments = [];
            } else if (/^\s*program-id\..+/i.test(line)) {
                modules.push(this._extractModuleOrFunction(line, lineCounter, comments));
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

    private _extractModuleOrFunction(line: string, lineNumber: number, comments: string[]): ModuleOrFunction {
        const name = line.split(".")[1].trim();
        const rawText = this._cleanComments(comments);
        const pieces = rawText.split('@');
        const params: Parameter[] = [];
        let returnObj: Return | undefined = undefined;
        let description: string | undefined = undefined;
        let summary: string | undefined = undefined;
        pieces.forEach((piece, index) => {
            if (index === 0) {
                description = piece;
                summary = piece.replace(/\s\s/g, '<br>').replace(/\n/g, ' ').trim();
            } else if (/^summary/.test(piece)) {
                summary = piece.substring(piece.indexOf(' ')).replace(/\s\s/g, '<br>').replace(/\n/g, ' ').trim();
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

        return {
            description: description,
            line: lineNumber,
            name: name,
            paragraphs: [],
            params: params,
            return: returnObj,
            summary: summary
        };
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
        return comments
            .map(comment => comment.replace(/^\s*\*>\**\s*/, ''))
            .filter(comment => comment.length !== 0)
            .join('\n');
    }
}
