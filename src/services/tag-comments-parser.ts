import { injectable } from 'inversify';
import { Documentation, ModuleOrFunction, Parameter, Return, PreDocumentation, PreModuleOrFunction } from '../model/documentation';
import { CommentsParser } from './comments-parser';

export interface TagCommentsParser extends CommentsParser {
}

@injectable()
export class TagCommentsParserImpl implements TagCommentsParser {

    constructor() { }

    public parse(preDocumentation: PreDocumentation): Documentation {
        const [fileDescription, author, license] = this._extractFileDetails(preDocumentation.fileComments);
        const modules = preDocumentation.modules.map(
            preModule => this._extractModuleOrFunction(preModule)
        );
        const functions = preDocumentation.functions.map(
            preFunction => this._extractModuleOrFunction(preFunction)
        );

        return {
            fileName: preDocumentation.fileName,
            fileDescription: fileDescription,
            author: author,
            license: license,
            functions: functions,
            modules: modules,
        };
    }

    private _extractModuleOrFunction(preModuleOrFunction: PreModuleOrFunction): ModuleOrFunction {
        const pieces = preModuleOrFunction.comments.split('@');
        const params: Parameter[] = [];
        let returnObj: Return | undefined = undefined;
        let description: string | undefined = undefined;
        let summary: string | undefined = undefined;
        pieces.forEach((piece, index) => {
            if (index === 0) {
                description = piece;
                summary = piece;
            } else if (/^summary/.test(piece)) {
                summary = piece.substring(piece.indexOf(' ') + 1);
            } else if (/^param/.test(piece)) {
                piece = piece.substring(piece.indexOf(' ') + 1);
                let paramType: string | undefined = undefined;
                const match = piece.match(/\{(.+)\}/);
                if (!!match) {
                    paramType = match[1];
                    piece = piece.replace(/\{(.+)\}/, '').trim();
                }
                const paramName = piece.substring(0, piece.indexOf(' '));
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
            line: preModuleOrFunction.line,
            name: preModuleOrFunction.name,
            paragraphs: [],
            params: params,
            return: returnObj,
            summary: summary
        };
    }

    private _extractFileDetails(comments: string): [string | undefined, string | undefined, string | undefined] {
        const pieces = comments.split('@');
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
}
