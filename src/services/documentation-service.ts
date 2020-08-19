import { injectable, inject } from 'inversify';
import * as path from 'path';
import { TYPES } from '../types';
import { Format, TableOfContents } from '../model/format';
import { TemplateEngine } from './template-engine';
import { FileOutputStream } from './file-output-stream';
import kleur from 'kleur';
import { FreeCommentsExtractor } from './free-comments-extractor';
import { TagCommentsParser } from './tag-comments-parser';
import { CodeFormat } from '../model/code-format';
import { CommentsExtractor } from './comments-extractor';
import { MicrofocusCommentsExtractor } from './microfocus-comments-extractor';
import { XmlCommentsParser } from './xml-comments-parser';
import { CommentType } from '../model/comment-type';
import { CommentsParser } from './comments-parser';

export interface DocumentationService {
    parse(files: string[], outputDirectory: string, format: Format, codeFormat: CodeFormat, commentType: CommentType): void;
}

@injectable()
export class DocumentationServiceImpl implements DocumentationService {

    constructor(
        @inject(TYPES.FreeCommentsExtractor) private readonly _freeCommentsExtractor: FreeCommentsExtractor,
        @inject(TYPES.MicrofocusCommentsExtractor) private readonly _microfocusCommentsExtractor: MicrofocusCommentsExtractor,
        @inject(TYPES.TagCommentsParser) private readonly _tagCommentsParser: TagCommentsParser,
        @inject(TYPES.XmlCommentsParser) private readonly _xmlCommentsParser: XmlCommentsParser,
        @inject(TYPES.TemplateEngine) private readonly _templateEngine: TemplateEngine,
        @inject(TYPES.FileOutputStream) private readonly _outputStream: FileOutputStream,
    ) { }

    public parse(files: string[], outputDirectory: string, format: Format, codeFormat: CodeFormat, commentType: CommentType): void {
        console.log(`Output directory: ${outputDirectory}`);
        console.log(`Selected format: ${format}`);
        const commentsExtractor = this._getCommentsExtractor(codeFormat);
        const commentsParser = this._getCommentsParser(commentType);
        const exportedFiles: string[] = [];
        files.forEach(file => {
            this._parseFile(file, outputDirectory, format, commentsExtractor, commentsParser, exportedFiles);
        });
        const tableOfContents = TableOfContents.from(format);
        process.stdout.write(`Generating ${tableOfContents}... `);
        const outputContent = this._templateEngine.parseTableOfContents(format, exportedFiles);
        this._outputStream.write(outputDirectory, tableOfContents, outputContent);
        console.log(kleur.green('DONE'));
    }

    private _parseFile(file: string, outputDirectory: string, format: Format, commentsExtractor: CommentsExtractor, commentsParser: CommentsParser, exportedFiles: string[]): void {
        const filePath = this._defineFilePath(file);
        const baseName = path.basename(filePath);
        const exportedFileName = `${baseName}.${format}`;
        process.stdout.write(`Generating ${baseName} documentation... `);
        const preDocumentation = commentsExtractor.extract(filePath);
        const documentation = commentsParser.parse(preDocumentation);
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

    private _getCommentsExtractor(codeFormat: CodeFormat): CommentsExtractor {
        switch (codeFormat) {
            case CodeFormat.FREE:
                return this._freeCommentsExtractor;
            case CodeFormat.MICROFOCUS:
                return this._microfocusCommentsExtractor;
            default:
                throw new Error(`Code format not supported: ${codeFormat}`);
        }
    }

    private _getCommentsParser(commentType: CommentType): CommentsParser {
        switch (commentType) {
            case CommentType.TAG:
                return this._tagCommentsParser;
            case CommentType.XML:
                return this._xmlCommentsParser;
            default:
                throw new Error(`Comment type not supported: ${commentType}`);
        }
    }
}
