import { injectable } from 'inversify';
import { Documentation, CobolFunction, CobolModule } from '../model/documentation';
import * as fs from 'fs';
import { DocumentationExtractor } from './documentation-extractor';
import * as path from 'path';
import { Dialect } from '../model/dialect';

export interface FreeDialectExtractor extends DocumentationExtractor {
}

@injectable()
export class FreeDialectExtractorImpl implements FreeDialectExtractor {

    constructor() { }

    public extract(filePath: string): Documentation {
        const data = fs.readFileSync(filePath, 'utf8');
        const lines = data.split(/\r?\n/);

        let comments: string[] = [];
        let fileComments: string[] = [];
        let firstCobolDoc = true;
        let isCobolDoc = false;
        let functions: CobolFunction[] = [];
        let modules: CobolModule[] = [];
        let lineCounter = 1;
        lines.forEach((line) => {
            if (/\*\>\*+/.test(line) && !isCobolDoc) {
                isCobolDoc = true;
                comments.push(line);
            } else if (/\*\>[^\*]/.test(line) && isCobolDoc) {
                comments.push(line);
            } else if (/\*\>\*+/.test(line) && isCobolDoc) {
                isCobolDoc = false;
                comments.push(line);
                
                if (firstCobolDoc) {
                    firstCobolDoc = false;
                    fileComments = [...comments];
                    comments = [];
                }
            } else if (line.toLowerCase().startsWith("function-id.")) {
                const cobolFunction: CobolFunction = {
                    description: comments.join('\n'),
                    line: lineCounter,
                    name: line.substring(12).trim()
                };
                functions.push(cobolFunction);
                comments = [];
            } else if (line.toLowerCase().startsWith("program-id.")) {
                const cobolModule: CobolModule = {
                    description: comments.join('\n'),
                    line: lineCounter,
                    name: line.substring(11).trim()
                };
                modules.push(cobolModule);
                comments = [];
            }
            lineCounter++;
        });

        return {
            dialect: Dialect.FREE,
            fileDescription: fileComments.join('\n'),
            fileName: path.basename(filePath),
            functions: functions,
            modules: modules,
        };
    }
}
