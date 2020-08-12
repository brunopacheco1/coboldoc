import { injectable, inject } from 'inversify';
import * as path from 'path';
import { DocumentationExtractor } from './documentation-extractor';
import { TYPES } from '../types';
import { Format } from '../model/format';
import { TemplateEngine } from './template-engine';
import { DocumentationOutputStream } from './documentation-output-stream';
import { Dialect } from '../model/dialect';
import { FreeDialectExtractor } from './free-dialect-extractor';

export interface FileParser {
    parse(files: string[], dialect: Dialect, outputDirectory: string, format: Format): void;
}

@injectable()
export class FileParserImpl implements FileParser {

    constructor(
        @inject(TYPES.FreeDialectExtractor) private _freeDialectExtractor: FreeDialectExtractor,
        @inject(TYPES.TemplateEngine) private _templateEngine: TemplateEngine,
        @inject(TYPES.DocumentationOutputStream) private _outputStream: DocumentationOutputStream,
    ) { }

    public parse(files: string[], dialect: Dialect, outputDirectory: string, format: Format): void {
        const extractor = this._getExtractor(dialect);
        files.forEach(file => {
            this._parseFile(extractor, file, outputDirectory, format);
        });
    }

    private _getExtractor(dialect: Dialect): DocumentationExtractor {
        switch (dialect) {
            case Dialect.FREE:
                return this._freeDialectExtractor;
            default:
                throw new Error(`Not supported dialect: ${dialect}`);
        }
    }

    private _parseFile(extractor: DocumentationExtractor, file: string, outputDirectory: string, format: Format): void {
        const filePath = this._defineFilePath(file);
        const documentation = extractor.extract(filePath);
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
