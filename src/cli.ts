import { Command } from 'commander';
import { injectable } from 'inversify';
import kleur from 'kleur';
import figlet from 'figlet';
const pkg = require('../package.json');

@injectable()
export class Main {

    constructor() { }

    public main(argv: string[]): void {
        const command = new Command();

        console.log(
            kleur.red(
                figlet.textSync('coboldoc', { horizontalLayout: 'full' })
            )
        );

        command.version(pkg.version, '-v, --version')
            .usage('<command> [options]');

        command.command('generate <files>')
            .description('generate the documentation')
            .action((files) => {
                console.log(files);
            });

        command.parse(argv);

        if (argv.length <= 2) {
            command.help();
        }
    }
}
