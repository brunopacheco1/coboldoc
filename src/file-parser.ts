import { injectable } from 'inversify';
import * as fs from 'fs';
import * as path from 'path';

export interface FileParser {
    parse(files: string[]): void;
}

@injectable()
export class FileParserImpl implements FileParser {

    constructor() { }

    public parse(files: string[]): void {
        files.forEach(file => {
            this._parseFile(file);
        })
    }

    private _parseFile(file: string): void {
        let filePath = file;
        if (!path.isAbsolute(file)) {
            filePath = path.join(process.cwd(), file);
        }
        const data = fs.readFileSync(filePath, 'utf8');
        console.log(data);
    }
}
