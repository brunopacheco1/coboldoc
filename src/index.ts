#!/usr/bin/env node
import 'reflect-metadata';
import { Container } from 'inversify';
import { Cli } from './controllers/cli';
import { TYPES } from './types';
import { TemplateEngine, TemplateEngineImpl } from './services/template-engine';
import { FileOutputStream, FileOutputStreamImpl } from './services/file-output-stream';
import { TagCommentsParser, TagCommentsParserImpl } from './services/tag-comments-parser';
import { MsdnCommentsParser, MsdnCommentsParserImpl } from './services/msdn-comments-parser';
import { DocumentationService, DocumentationServiceImpl } from './services/documentation-service';
import { CommentsExtractor, CommentsExtractorImpl } from './services/comments-extractor';

export function init(): Cli {
    const container: Container = new Container();

    container.bind<FileOutputStream>(TYPES.FileOutputStream).to(FileOutputStreamImpl).inSingletonScope();
    container.bind<TemplateEngine>(TYPES.TemplateEngine).to(TemplateEngineImpl).inSingletonScope();
    container.bind<CommentsExtractor>(TYPES.CommentsExtractor).to(CommentsExtractorImpl).inSingletonScope();
    container.bind<TagCommentsParser>(TYPES.TagCommentsParser).to(TagCommentsParserImpl).inSingletonScope();
    container.bind<MsdnCommentsParser>(TYPES.MsdnCommentsParser).to(MsdnCommentsParserImpl).inSingletonScope();
    container.bind<DocumentationService>(TYPES.DocumentationService).to(DocumentationServiceImpl).inSingletonScope();
    container.bind<Cli>(TYPES.Cli).to(Cli).inSingletonScope();

    return container.get<Cli>(TYPES.Cli);
}

const app: Cli = init();

app.main(process.argv);
