import { injectable } from 'inversify';
import * as fs from 'fs';
import * as path from 'path';

export interface FileOutputStream {
    write(outputDirectory: string, fileName: string, content: string): void;
}

@injectable()
export class FileOutputStreamImpl implements FileOutputStream {

    constructor() { }

    public write(outputDirectory: string, fileName: string, content: string): void {
        const outputDirectoryPath = path.join(outputDirectory, fileName);
        if (!fs.existsSync(outputDirectory)) {
            fs.mkdirSync(outputDirectory);
        }
        fs.writeFileSync(outputDirectoryPath, content);
    }
}
