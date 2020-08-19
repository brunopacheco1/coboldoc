import { PreDocumentation } from '../model/documentation';

export interface CommentsExtractor {
    extract(filePath: string): PreDocumentation;
}
