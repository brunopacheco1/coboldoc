import 'reflect-metadata';
import { Container } from 'inversify';
import { Cli } from './controllers/cli';
import { TYPES } from './types';
import { FileParser, FileParserImpl } from './services/file-parser';
import { MdParser, MdParserImpl } from './services/md-parser';
import { HtmlParser, HtmlParserImpl } from './services/html-parser';
import { DocumentationOutputStream, DocumentationOutputStreamImpl } from './services/documentation-output-stream';
import { FreeDialectExtractor, FreeDialectExtractorImpl } from './services/free-dialect-extractor';

export function init(): Cli {
    const container: Container = new Container();

    container.bind<DocumentationOutputStream>(TYPES.DocumentationOutputStream).to(DocumentationOutputStreamImpl).inSingletonScope();
    container.bind<MdParser>(TYPES.MdParser).to(MdParserImpl).inSingletonScope();
    container.bind<HtmlParser>(TYPES.HtmlParser).to(HtmlParserImpl).inSingletonScope();
    container.bind<FreeDialectExtractor>(TYPES.FreeDialectExtractor).to(FreeDialectExtractorImpl).inSingletonScope();
    container.bind<FileParser>(TYPES.FileParser).to(FileParserImpl).inSingletonScope();
    container.bind<Cli>(TYPES.Cli).to(Cli).inSingletonScope();

    return container.get<Cli>(TYPES.Cli);
}

const app: Cli = init();

app.main(process.argv);
