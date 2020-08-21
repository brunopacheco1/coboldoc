import { injectable, inject } from 'inversify';
import * as path from 'path';
import { TYPES } from '../types';
import { Format, TableOfContents } from '../model/format';
import { TemplateEngine } from './template-engine';
import { FileOutputStream } from './file-output-stream';
import kleur from 'kleur';
import { TagCommentsParser } from './tag-comments-parser';
import { Dialect } from '../model/dialect';
import { CommentsExtractor } from './comments-extractor';
import { XmlCommentsParser } from './xml-comments-parser';
import { CommentType } from '../model/comment-style';
import { CommentsParser } from './comments-parser';

export interface DocumentationService {
    parse(files: string[], outputDirectory: string, format: Format, dialect: Dialect, commentType: CommentType): void;
}

@injectable()
export class DocumentationServiceImpl implements DocumentationService {

    constructor(
        @inject(TYPES.CommentsExtractor) private readonly _commentsExtractor: CommentsExtractor,
        @inject(TYPES.TagCommentsParser) private readonly _tagCommentsParser: TagCommentsParser,
        @inject(TYPES.XmlCommentsParser) private readonly _xmlCommentsParser: XmlCommentsParser,
        @inject(TYPES.TemplateEngine) private readonly _templateEngine: TemplateEngine,
        @inject(TYPES.FileOutputStream) private readonly _outputStream: FileOutputStream,
    ) { }

    public parse(files: string[], outputDirectory: string, format: Format, dialect: Dialect, commentType: CommentType): void {
        console.log(`Output directory: ${outputDirectory}`);
        console.log(`Selected format: ${format}`);
        const commentsParser = this._getCommentsParser(commentType);
        const exportedFiles: string[] = [];
        files.forEach(file => {
            this._parseFile(file, outputDirectory, format, dialect, commentsParser, exportedFiles);
        });
        const tableOfContents = TableOfContents.from(format);
        process.stdout.write(`Generating ${tableOfContents}... `);
        const outputContent = this._templateEngine.parseTableOfContents(format, exportedFiles);
        this._outputStream.write(outputDirectory, tableOfContents, outputContent);
        console.log(kleur.green('DONE'));
    }

    private _parseFile(file: string, outputDirectory: string, format: Format, dialect: Dialect, commentsParser: CommentsParser, exportedFiles: string[]): void {
        const filePath = this._defineFilePath(file);
        const baseName = path.basename(filePath);
        const exportedFileName = `${baseName}.${format}`;
        process.stdout.write(`Generating ${baseName} documentation... `);
        const preDocumentation = this._commentsExtractor.extract(dialect, filePath);
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
