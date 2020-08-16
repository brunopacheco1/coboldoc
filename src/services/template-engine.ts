import { injectable } from 'inversify';
import { Documentation } from '../model/documentation';
import * as ejs from 'ejs';
import * as fs from 'fs';
import * as path from 'path';
import { Format } from '../model/format';

export interface TemplateEngine {
    parseDocumentation(format: Format, documentation: Documentation): string;
    parseTableOfContents(format: Format, fileNames: string[]): string;
}

@injectable()
export class TemplateEngineImpl implements TemplateEngine {

    private readonly _templateFunctions: Map<string, ejs.TemplateFunction> = new Map();

    constructor() {
        Object.values(Format).forEach(format => {
            const templatePath = path.join(__dirname, `../resources/templates/${format}.ejs`);
            const template = fs.readFileSync(templatePath, 'utf8');
            this._templateFunctions.set(format, ejs.compile(template));

            const tableOfContentsTemplatePath = path.join(__dirname, `../resources/templates/table-of-contents.${format}.ejs`);
            const templtableOfContentsTemplate = fs.readFileSync(tableOfContentsTemplatePath, 'utf8');
            this._templateFunctions.set(`table-of-contents.${format}`, ejs.compile(templtableOfContentsTemplate));
        });
    }

    public parseDocumentation(format: Format, documentation: Documentation): string {
        const renderFunction = this._templateFunctions.get(format);
        if (!renderFunction) {
            throw new Error(`template not found: ${format}`);
        }
        return renderFunction(documentation);
    }

    public parseTableOfContents(format: Format, fileNames: string[]): string {
        const renderFunction = this._templateFunctions.get(`table-of-contents.${format}`);
        if (!renderFunction) {
            throw new Error(`template of table of contents not found: ${format}`);
        }
        return renderFunction({ fileNames });
    }
}
