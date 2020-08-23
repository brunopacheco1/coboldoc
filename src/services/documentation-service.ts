import { injectable, inject } from 'inversify';
import * as path from 'path';
import { TYPES } from '../types';
import { Format, TableOfContents } from '../model/format';
import { TemplateEngine } from './template-engine';
import { FileOutputStream } from './file-output-stream';
import kleur from 'kleur';
import { TagCommentsParser } from './tag-comments-parser';
import { CommentStyle } from '../model/comment-style';
import { CommentsExtractor } from './comments-extractor';
import { MsdnCommentsParser as MsdnCommentsParser } from './msdn-comments-parser';
import { AnnotationType } from '../model/annotation-type';
import { CommentsParser } from './comments-parser';

export interface DocumentationService {
    parse(files: string[], outputDirectory: string, format: Format, dialect: CommentStyle, annotationType: AnnotationType): void;
}

@injectable()
export class DocumentationServiceImpl implements DocumentationService {

    constructor(
        @inject(TYPES.CommentsExtractor) private readonly _commentsExtractor: CommentsExtractor,
        @inject(TYPES.TagCommentsParser) private readonly _tagCommentsParser: TagCommentsParser,
        @inject(TYPES.MsdnCommentsParser) private readonly _msdnCommentsParser: MsdnCommentsParser,
        @inject(TYPES.TemplateEngine) private readonly _templateEngine: TemplateEngine,
        @inject(TYPES.FileOutputStream) private readonly _outputStream: FileOutputStream,
    ) { }

    public parse(files: string[], outputDirectory: string, format: Format, commentStyle: CommentStyle, annotationType: AnnotationType): void {
        console.log(`Output directory: ${outputDirectory}`);
        console.log(`Selected format: ${format}`);
        console.log(`Selected comment style: ${commentStyle}`);
        console.log(`Selected annotation type: ${annotationType}`);
        const commentsParser = this._getCommentsParser(annotationType);
        const exportedFiles: string[] = [];
        files.forEach(file => {
            this._parseFile(file, outputDirectory, format, commentStyle, commentsParser, exportedFiles);
        });
        const tableOfContents = TableOfContents.from(format);
        process.stdout.write(`Generating ${tableOfContents}... `);
        const outputContent = this._templateEngine.parseTableOfContents(format, exportedFiles);
        this._outputStream.write(outputDirectory, tableOfContents, outputContent);
        console.log(kleur.green('DONE'));
    }

    private _parseFile(file: string, outputDirectory: string, format: Format, commentStyle: CommentStyle, commentsParser: CommentsParser, exportedFiles: string[]): void {
        const filePath = this._defineFilePath(file);
        const baseName = path.basename(filePath);
        const exportedFileName = `${baseName}.${format}`;
        process.stdout.write(`Generating ${baseName} documentation... `);
        const preDocumentation = this._commentsExtractor.extract(commentStyle, filePath);
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

    private _getCommentsParser(annotationType: AnnotationType): CommentsParser {
        switch (annotationType) {
            case AnnotationType.TAG:
                return this._tagCommentsParser;
            case AnnotationType.MSDN:
                return this._msdnCommentsParser;
            default:
                throw new Error(`Annotation type not supported: ${annotationType}`);
        }
    }
}
