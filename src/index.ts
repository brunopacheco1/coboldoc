import 'reflect-metadata';
import { Container } from 'inversify';
import { Main } from './cli';
import { TYPES } from './types';

export function init(): Main {
    const container: Container = new Container();

    container.bind<Main>(TYPES.Main).to(Main).inSingletonScope();

    return container.get<Main>(TYPES.Main);
}

const app: Main = init();

app.main(process.argv);
