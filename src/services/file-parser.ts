import { injectable, inject } from 'inversify';
import * as path from 'path';
import { DocumentationExtractor } from './documentation-extractor';
import { TYPES } from '../types';
import { Format } from '../model/format';
import { TemplateEngine } from './template-engine';
import { DocumentationOutputStream } from './documentation-output-stream';
import kleur from 'kleur';

export interface FileParser {
    parse(files: string[], outputDirectory: string, format: Format): void;
}

@injectable()
export class FileParserImpl implements FileParser {

    constructor(
        @inject(TYPES.DocumentationExtractor) private _documentationExtractor: DocumentationExtractor,
        @inject(TYPES.TemplateEngine) private _templateEngine: TemplateEngine,
        @inject(TYPES.DocumentationOutputStream) private _outputStream: DocumentationOutputStream,
    ) { }

    public parse(files: string[], outputDirectory: string, format: Format): void {
        console.log(`Output directory: ${outputDirectory}`);
        console.log(`Selected format: ${format}`);
        files.forEach(file => {
            process.stdout.write(`Generating ${file} documentation... `);
            this._parseFile(file, outputDirectory, format);
            console.log(kleur.green('DONE'));
        });
    }

    private _parseFile(file: string, outputDirectory: string, format: Format): void {
        const filePath = this._defineFilePath(file);
        const documentation = this._documentationExtractor.extract(filePath);
        const parsedDocumentation = this._templateEngine.parse(format, documentation);
        this._outputStream.write(outputDirectory, parsedDocumentation);
    }

    private _defineFilePath(file: string): string {
        let filePath = file;
        if (!path.isAbsolute(file)) {
            filePath = path.join(process.cwd(), file);
        }
        return filePath;
    }
}
