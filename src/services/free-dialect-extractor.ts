import { injectable } from 'inversify';
import { Documentation } from '../model/documentation';
import * as fs from 'fs';
import { DocumentationExtractor } from './documentation-extractor';

export interface FreeDialectExtractor extends DocumentationExtractor {
}

@injectable()
export class FreeDialectExtractorImpl implements FreeDialectExtractor {

    constructor() { }

    public extract(filePath: string): Documentation {
        const data = fs.readFileSync(filePath, 'utf8');
        console.log(data);
        throw new Error('Not implemented yet');
    }
}
