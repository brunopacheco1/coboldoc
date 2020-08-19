import { injectable } from 'inversify';
import { PreDocumentation } from '../model/documentation';
import { CommentsExtractor } from './comments-extractor';

export interface MicrofocusCommentsExtractor extends CommentsExtractor {
}

@injectable()
export class MicrofocusCommentsExtractorImpl implements MicrofocusCommentsExtractor {

    public extract(filePath: string): PreDocumentation {
        throw new Error('Not implemented yet.');
    }
}
