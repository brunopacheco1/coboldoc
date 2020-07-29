import 'reflect-metadata';
import { Container } from 'inversify';
import { Cli } from './cli';
import { TYPES } from './types';
import { FileParser, FileParserImpl } from './file-parser';

export function init(): Cli {
    const container: Container = new Container();

    container.bind<FileParser>(TYPES.FileParser).to(FileParserImpl).inSingletonScope();
    container.bind<Cli>(TYPES.Cli).to(Cli).inSingletonScope();

    return container.get<Cli>(TYPES.Cli);
}

const app: Cli = init();

app.main(process.argv);
