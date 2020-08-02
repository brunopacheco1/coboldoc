import { injectable } from 'inversify';
import { Documentation } from '../model/documentation';
import { Parser } from './parser';

export interface MdParser extends Parser {
}

@injectable()
export class MdParserImpl implements MdParser {

    constructor() { }

    public parse(documentation: Documentation): string {
        throw new Error('Not implemented yet');
    }
}
