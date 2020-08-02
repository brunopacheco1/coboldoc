import { injectable, inject } from 'inversify';
import * as path from 'path';
import { DocumentationExtractor } from './documentation-extractor';
import { TYPES } from '../types';
import { Format } from '../model/format';
import { MdParser } from './md-parser';
import { HtmlParser } from './html-parser';
import { Parser } from './parser';
import { DocumentationOutputStream } from './documentation-output-stream';

export interface FileParser {
    parse(files: string[], outputDirectory: string, format: Format): void;
}

@injectable()
export class FileParserImpl implements FileParser {

    constructor(
        @inject(TYPES.DocumentationExtractor) private _documentationExtractor: DocumentationExtractor,
        @inject(TYPES.MdParser) private _mdParser: MdParser,
        @inject(TYPES.HtmlParser) private _htmlParser: HtmlParser,
        @inject(TYPES.DocumentationOutputStream) private _outputStream: DocumentationOutputStream,
    ) { }

    public parse(files: string[], outputDirectory: string, format: Format): void {
        const parser = this._getParser(format);
        files.forEach(file => {
            this._parseFile(file, outputDirectory, parser);
        });
    }

    private _getParser(format: Format): Parser {
        switch (format) {
            case Format.HTML:
                return this._htmlParser;
            case Format.MD:
                return this._mdParser;
            default:
                throw new Error(`Not supported format: ${format}`);
        }
    }

    private _parseFile(file: string, outputDirectory: string, parser: Parser): void {
        const filePath = this._defineFilePath(file);
        const documentation = this._documentationExtractor.extract(filePath);
        const parsedDocumentation = parser.parse(documentation);
        this._outputStream.write(parsedDocumentation);
    }

    private _defineFilePath(file: string): string {
        let filePath = file;
        if (!path.isAbsolute(file)) {
            filePath = path.join(process.cwd(), file);
        }
        return filePath;
    }
}
