import { injectable } from 'inversify';
import { Documentation, PreDocumentation } from '../model/documentation';
import { CommentsParser } from './comments-parser';

export interface XmlCommentsParser extends CommentsParser {
}

@injectable()
export class XmlCommentsParserImpl implements XmlCommentsParser {

    constructor() { }

    public parse(preDocumentation: PreDocumentation): Documentation {
        throw new Error('Not implemented yet.');
    }
}
