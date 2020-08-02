import { injectable } from 'inversify';

export interface DocumentationOutputStream {
    write(documentationText: string): void;
}

@injectable()
export class DocumentationOutputStreamImpl implements DocumentationOutputStream {

    constructor() { }

    public write(documentationText: string): void {
        throw new Error('Not implemented yet');
    }
}
