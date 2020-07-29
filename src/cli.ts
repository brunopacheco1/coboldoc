import { Command } from 'commander';
import { injectable, inject } from 'inversify';
import kleur from 'kleur';
import figlet from 'figlet';
import { FileParser } from './file-parser';
import { TYPES } from './types';
const pkg = require('../package.json');

@injectable()
export class Cli {

    private _MINIMUM_ARG_SIZE = 2;

    constructor(@inject(TYPES.FileParser) private _fileParser: FileParser) { }

    public main(argv: string[]): void {
        const command = new Command();

        console.log(
            kleur.red(
                figlet.textSync('coboldoc', { horizontalLayout: 'full' }),
            ),
        );

        command.version(pkg.version, '-v, --version')
            .usage('<command> [options]');

        command.command('generate <files...>')
            .description('generate the documentation')
            .action((files) => {
                this._fileParser.parse(files);
            });

        command.parse(argv);

        if (argv.length <= this._MINIMUM_ARG_SIZE) {
            command.help();
        }
    }
}
