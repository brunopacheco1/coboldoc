import { injectable } from 'inversify';
import { Documentation } from '../model/documentation';
import * as fs from 'fs';

export interface DocumentationExtractor {
    extract(filePath: string): Documentation;
}

@injectable()
export class DocumentationExtractorImpl implements DocumentationExtractor {

    constructor() { }

    public extract(filePath: string): Documentation {
        const data = fs.readFileSync(filePath, 'utf8');
        console.log(data);
        throw new Error('Not implemented yet');
    }
}
