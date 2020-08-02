import { injectable } from 'inversify';
import { Documentation } from '../model/documentation';
import { Parser } from './parser';

export interface HtmlParser extends Parser {
}

@injectable()
export class HtmlParserImpl implements HtmlParser {

    constructor() { }

    public parse(documentation: Documentation): string {
        throw new Error('Not implemented yet');
    }
}
