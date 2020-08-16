import { injectable, inject } from 'inversify';
import * as path from 'path';
import { DocumentationExtractor } from './documentation-extractor';
import { TYPES } from '../types';
import { Format, TableOfContents } from '../model/format';
import { TemplateEngine } from './template-engine';
import { FileOutputStream } from './file-output-stream';
import kleur from 'kleur';

export interface FileParser {
    parse(files: string[], outputDirectory: string, format: Format): void;
}

@injectable()
export class FileParserImpl implements FileParser {

    constructor(
        @inject(TYPES.DocumentationExtractor) private readonly _documentationExtractor: DocumentationExtractor,
        @inject(TYPES.TemplateEngine) private readonly _templateEngine: TemplateEngine,
        @inject(TYPES.FileOutputStream) private readonly _outputStream: FileOutputStream,
    ) { }

    public parse(files: string[], outputDirectory: string, format: Format): void {
        console.log(`Output directory: ${outputDirectory}`);
        console.log(`Selected format: ${format}`);
        const exportedFiles: string[] = [];
        files.forEach(file => {
            this._parseFile(file, outputDirectory, format, exportedFiles);
        });
        const tableOfContents = TableOfContents.from(format);
        process.stdout.write(`Generating ${tableOfContents}... `);
        const outputContent = this._templateEngine.parseTableOfContents(format, exportedFiles);
        this._outputStream.write(outputDirectory, tableOfContents, outputContent);
        console.log(kleur.green('DONE'));
    }

    private _parseFile(file: string, outputDirectory: string, format: Format, exportedFiles: string[]): void {
        const filePath = this._defineFilePath(file);
        const baseName = path.basename(filePath);
        const exportedFileName = `${baseName}.${format}`;
        process.stdout.write(`Generating ${baseName} documentation... `);
        const documentation = this._documentationExtractor.extract(filePath);
        const outputContent = this._templateEngine.parseDocumentation(format, documentation);
        this._outputStream.write(outputDirectory, exportedFileName, outputContent);
        console.log(kleur.green('DONE'));
        exportedFiles.push(exportedFileName);
    }

    private _defineFilePath(file: string): string {
        let filePath = file;
        if (!path.isAbsolute(file)) {
            filePath = path.join(process.cwd(), file);
        }
        return filePath;
    }
}
