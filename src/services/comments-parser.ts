import { PreDocumentation, Documentation } from '../model/documentation';

export interface CommentsParser {
    parse(preDocumentation: PreDocumentation): Documentation;
}
