import { Command } from 'commander';
import { injectable, inject } from 'inversify';
import kleur from 'kleur';
import figlet from 'figlet';
import { FileParser } from '../services/file-parser';
import { TYPES } from '../types';
import { Format } from '../model/format';
import { Dialect } from '../model/dialect';
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

        command.option('-d, --dialect <dialect>', 'The suported dialects are FREE', 'FREE')
            .option('-o, --output <outputDirectory>', 'The output directory', './')
            .option('-f, --format <fileFormat>', 'The suported output format are MD or HTML', 'MD')
            .option('-V, --verbose', 'For extra logs to help on debugging');

        command.version(pkg.version, '-v, --version')
            .usage('<command> [options]');

        command.command('generate <files...>')
            .description('generate the documentation')
            .action((files) => {
                this._fileParser.parse(files, Dialect[command.dialect as keyof typeof Dialect], command.output, Format[command.format as keyof typeof Format]);
            });

        command.parse(argv);

        if (argv.length <= this._MINIMUM_ARG_SIZE) {
            command.help();
        }
    }
}
