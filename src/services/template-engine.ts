import { injectable } from 'inversify';
import { Documentation, ParsedDocumentation } from '../model/documentation';
import * as ejs from 'ejs';
import * as fs from 'fs';
import * as path from 'path';
import { Format } from '../model/format';

export interface TemplateEngine {
    parse(format: Format, documentation: Documentation): ParsedDocumentation;
}

@injectable()
export class TemplateEngineImpl implements TemplateEngine {

    private readonly _templateFunctions: Map<string, ejs.TemplateFunction> = new Map();

    constructor() {
        Object.values(Format).forEach(format => {
            const templatePath = path.join(__dirname, `../resources/templates/${format}.ejs`);
            
            const template = fs.readFileSync(templatePath, 'utf8');
            this._templateFunctions.set(format, ejs.compile(template));
        });
    }

    public parse(format: Format, documentation: Documentation): ParsedDocumentation {
        const renderFunction = this._templateFunctions.get(format);
        if (!renderFunction) {
            throw new Error(`template not found: ${format}`);
        }
        return {
            fileName: `${documentation.fileName}.${format}`,
            text: renderFunction(documentation),
        }
    }
}
