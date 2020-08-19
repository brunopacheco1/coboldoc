#!/usr/bin/env node
import 'reflect-metadata';
import { Container } from 'inversify';
import { Cli } from './controllers/cli';
import { TYPES } from './types';
import { TemplateEngine, TemplateEngineImpl } from './services/template-engine';
import { FileOutputStream, FileOutputStreamImpl } from './services/file-output-stream';
import { FreeCommentsExtractor, FreeCommentsExtractorImpl } from './services/free-comments-extractor';
import { TagCommentsParser, TagCommentsParserImpl } from './services/tag-comments-parser';
import { MicrofocusCommentsExtractor, MicrofocusCommentsExtractorImpl } from './services/microfocus-comments-extractor';
import { XmlCommentsParser, XmlCommentsParserImpl } from './services/xml-comments-parser';
import { DocumentationService, DocumentationServiceImpl } from './services/documentation-service';

export function init(): Cli {
    const container: Container = new Container();

    container.bind<FileOutputStream>(TYPES.FileOutputStream).to(FileOutputStreamImpl).inSingletonScope();
    container.bind<TemplateEngine>(TYPES.TemplateEngine).to(TemplateEngineImpl).inSingletonScope();
    container.bind<FreeCommentsExtractor>(TYPES.FreeCommentsExtractor).to(FreeCommentsExtractorImpl).inSingletonScope();
    container.bind<MicrofocusCommentsExtractor>(TYPES.MicrofocusCommentsExtractor).to(MicrofocusCommentsExtractorImpl).inSingletonScope();
    container.bind<TagCommentsParser>(TYPES.TagCommentsParser).to(TagCommentsParserImpl).inSingletonScope();
    container.bind<XmlCommentsParser>(TYPES.XmlCommentsParser).to(XmlCommentsParserImpl).inSingletonScope();
    container.bind<DocumentationService>(TYPES.DocumentationService).to(DocumentationServiceImpl).inSingletonScope();
    container.bind<Cli>(TYPES.Cli).to(Cli).inSingletonScope();

    return container.get<Cli>(TYPES.Cli);
}

const app: Cli = init();

app.main(process.argv);
