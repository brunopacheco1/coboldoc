import 'reflect-metadata';
import { Container } from 'inversify';
import { Main } from './cli';

export function init(): Main {
    const container: Container = new Container();

    container.bind<Main>('Main').to(Main).inSingletonScope();

    return container.get<Main>('Main');
}

const app: Main = init();

app.main(process.argv);
