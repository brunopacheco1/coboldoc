import { Documentation } from '../model/documentation';

export interface DocumentationExtractor {
    extract(filePath: string): Documentation;
}
