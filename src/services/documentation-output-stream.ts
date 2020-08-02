import { injectable } from 'inversify';

export interface DocumentationOutputStream {
    write(outputDirectory: string, fileName: string, documentationText: string): void;
}

@injectable()
export class DocumentationOutputStreamImpl implements DocumentationOutputStream {

    constructor() { }

    public write(outputDirectory: string, fileName: string, documentationText: string): void {
        throw new Error('Not implemented yet');
    }
}
