import { injectable } from 'inversify';
import { Documentation, PreDocumentation, PreModuleOrFunction, ModuleOrFunction, Parameter, Return } from '../model/documentation';
import { CommentsParser } from './comments-parser';
import * as xml2js from 'xml2js';

export interface MsdnCommentsParser extends CommentsParser {
}

@injectable()
export class MsdnCommentsParserImpl implements MsdnCommentsParser {

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
        let parsed: any = {};
        xml2js.parseString(`<root>${preModuleOrFunction.comments}</root>`, {
            async: false,
            ignoreAttrs: false,
        }, (err, result) => {
            if (!!err) {
                throw err;
            }
            parsed = result.root;
        });

        const params: Parameter[] = [];
        if (!!parsed.param) {
            for (const param of parsed.param) {
                params.push({
                    name: !!param['$'] ? param['$']['name'] : undefined,
                    description: param['_'],
                    type: !!param['$'] ? param['$']['type'] : undefined,
                });
            }
        }

        let returnObj: Return | undefined = undefined;

        if (!!parsed.returns && parsed.returns.length > 0) {
            returnObj = {
                description: parsed.returns[0]['_'],
                type: !!parsed.returns[0]['$'] ? parsed.returns[0]['$']['type'] : undefined,
            };
        }

        const summary = this._extractTag(preModuleOrFunction.comments, 'summary');
        const remarks = this._extractTag(preModuleOrFunction.comments, 'remarks');
        const example = this._extractTag(preModuleOrFunction.comments, 'example');

        return {
            description: summary,
            summary: remarks || summary,
            example: example,
            line: preModuleOrFunction.line,
            name: preModuleOrFunction.name,
            paragraphs: [],
            params: params,
            return: returnObj,
        };
    }

    private _extractTag(comment: string, tag: string): string | undefined {
        if(comment.includes(`<${tag}>`) && comment.includes(`</${tag}>`)) {
            return comment.split(`<${tag}>`)[1].split(`</${tag}>`)[0];
        }
        return undefined;
    }

    private _retrieveValue(field: any): string {
        return !!field && field.length > 0 ? field[0] : undefined;
    }

    private _extractFileDetails(comments: string): [string | undefined, string | undefined, string | undefined] {
        let parsed: any = {};
        xml2js.parseString(`<root>${comments}</root>`, {
            async: false,
            ignoreAttrs: false,
        }, (err, result) => {
            if (!!err) {
                throw err;
            }
            parsed = result.root;
        });

        const summary = !!parsed.summary && parsed.summary.length > 0 ? parsed.summary[0] : undefined;
        const author = !!parsed.author && parsed.author.length > 0 ? parsed.author[0] : undefined;
        const license = !!parsed.license && parsed.license.length > 0 ? parsed.license[0] : undefined;
        return [summary, author, license];
    }
}