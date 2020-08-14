import { Command } from 'commander';
import { injectable, inject } from 'inversify';
import kleur from 'kleur';
import figlet from 'figlet';
import { FileParser } from '../services/file-parser';
import { TYPES } from '../types';
import { Format } from '../model/format';
const pkg = require('../../package.json');

@injectable()
export class Cli {

    private _MINIMUM_ARG_SIZE = 2;

    constructor(
        @inject(TYPES.FileParser) private _fileParser: FileParser) { }

    public main(argv: string[]): void {
        console.log(
            kleur.red(
                figlet.textSync('coboldoc', { horizontalLayout: 'full' }),
            ),
        );

        const command = new Command();

        command.option('-o, --output <outputDirectory>', 'The output directory', process.cwd())
            .option('-f, --format <fileFormat>', 'Suported output format: md, html', 'md')

        command.version(pkg.version, '-v, --version')
            .usage('<command> [options]');

        command.command('generate <files...>')
            .description('generate the documentation')
            .action((files) => {
                this._fileParser.parse(files, command.output, Format[command.format.toUpperCase() as keyof typeof Format]);
            });

        command.parse(argv);

        if (argv.length <= this._MINIMUM_ARG_SIZE) {
            command.help();
        }
    }
}
