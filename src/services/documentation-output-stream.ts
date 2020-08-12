import { injectable } from 'inversify';
import { ParsedDocumentation } from '../model/documentation';
import * as fs from 'fs';
import * as path from 'path';

export interface DocumentationOutputStream {
    write(outputDirectory: string, parsedDocumentation: ParsedDocumentation): void;
}

@injectable()
export class DocumentationOutputStreamImpl implements DocumentationOutputStream {

    constructor() { }

    public write(outputDirectory: string, parsedDocumentation: ParsedDocumentation): void {
        const outputDirectoryPath = path.join(outputDirectory, parsedDocumentation.fileName);
        fs.writeFileSync(outputDirectoryPath, parsedDocumentation.text);
    }
}
