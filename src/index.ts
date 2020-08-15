#!/usr/bin/env node
import 'reflect-metadata';
import { Container } from 'inversify';
import { Cli } from './controllers/cli';
import { TYPES } from './types';
import { FileParser, FileParserImpl } from './services/file-parser';
import { TemplateEngine, TemplateEngineImpl } from './services/template-engine';
import { DocumentationOutputStream, DocumentationOutputStreamImpl } from './services/documentation-output-stream';
import { DocumentationExtractor, DocumentationExtractorImpl } from './services/documentation-extractor';

export function init(): Cli {
    const container: Container = new Container();

    container.bind<DocumentationOutputStream>(TYPES.DocumentationOutputStream).to(DocumentationOutputStreamImpl).inSingletonScope();
    container.bind<TemplateEngine>(TYPES.TemplateEngine).to(TemplateEngineImpl).inSingletonScope();
    container.bind<DocumentationExtractor>(TYPES.DocumentationExtractor).to(DocumentationExtractorImpl).inSingletonScope();
    container.bind<FileParser>(TYPES.FileParser).to(FileParserImpl).inSingletonScope();
    container.bind<Cli>(TYPES.Cli).to(Cli).inSingletonScope();

    return container.get<Cli>(TYPES.Cli);
}

const app: Cli = init();

app.main(process.argv);
