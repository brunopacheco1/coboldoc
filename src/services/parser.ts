import { Documentation } from '../model/documentation';

export interface Parser {
    parse(documentation: Documentation): string;
}
