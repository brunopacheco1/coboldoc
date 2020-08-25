import { injectable } from 'inversify';
import { CobolFunction, Parameter, Return, PreCobolClass, CobolClass, PreCobolObject, CobolProperty, PreCobolProperty } from '../model/documentation';
import { CommentsParser, BaseCommentsParser } from './comments-parser';
import * as xml2js from 'xml2js';

export interface MsdnCommentsParser extends CommentsParser {
}

@injectable()
export class MsdnCommentsParserImpl extends BaseCommentsParser implements MsdnCommentsParser {

    constructor() {
        super();
    }

    protected _extractFunction(preCobolFunction: PreCobolObject): CobolFunction {
        let parsed: any = {};
        xml2js.parseString(`<root>${preCobolFunction.comments}</root>`, {
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

        const summary = this._extractTag(preCobolFunction.comments, 'summary');
        const remarks = this._extractTag(preCobolFunction.comments, 'remarks');
        const example = this._extractTag(preCobolFunction.comments, 'example');

        return {
            description: summary,
            summary: remarks || summary,
            example: example,
            line: preCobolFunction.line,
            name: preCobolFunction.name,
            paragraphs: [],
            params: params,
            return: returnObj,
        };
    }

    private _extractTag(comment: string, tag: string): string | undefined {
        if (comment.includes(`<${tag}>`) && comment.includes(`</${tag}>`)) {
            return comment.split(`<${tag}>`)[1].split(`</${tag}>`)[0];
        }
        return undefined;
    }

    protected _extractFileDetails(comments: string): [string | undefined, string | undefined, string | undefined] {
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

    protected _extractClass(preClass: PreCobolClass): CobolClass {
        const summary = this._extractTag(preClass.comments, 'summary');
        const remarks = this._extractTag(preClass.comments, 'remarks');
        const example = this._extractTag(preClass.comments, 'example');

        return {
            line: preClass.line,
            name: preClass.name,
            methods: [],
            properties: [],
            description: summary,
            summary: remarks || summary,
            example: example,
        };
    }

    protected _extractProperty(preProperty: PreCobolProperty): CobolProperty {
        const summary = this._extractTag(preProperty.comments, 'summary');
        const value = this._extractTag(preProperty.comments, 'value');

        return {
            line: preProperty.line,
            name: preProperty.name,
            description: summary || value,
            type: preProperty.type,
        };
    }
}
