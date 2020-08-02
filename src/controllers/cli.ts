import { Command } from 'commander';
import { injectable, inject } from 'inversify';
import kleur from 'kleur';
import figlet from 'figlet';
import { FileParser } from '../services/file-parser';
import { TYPES } from '../types';
const pkg = require('../package.json');

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

        command.option('-o, --output <outputDirectory>', 'the output directory, if it is empty or this option is missing, the working directory will be the output', './')
            .option('-f, --format <fileFormat>', 'Currently MD or HTML are possible output format (md is the default)', 'MD')
            .option('-v, --verbose', 'For extra logs to help on debugging');

        command.version(pkg.version, '-v, --version')
            .usage('<command> [options]');

        command.command('generate <files...>')
            .description('generate the documentation')
            .action((files) => {
                this._fileParser.parse(files, command.output, command.format);
            });

        command.parse(argv);

        if (argv.length <= this._MINIMUM_ARG_SIZE) {
            command.help();
        }
    }
}
