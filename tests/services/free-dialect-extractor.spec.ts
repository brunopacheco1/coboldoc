import 'reflect-metadata';
import 'mocha';
import { FreeDialectExtractor, FreeDialectExtractorImpl } from '../../src/services/free-dialect-extractor';

describe('FreeDialectExtractor', () => {
    let service: FreeDialectExtractor;

    beforeEach(() => {
        service = new FreeDialectExtractorImpl();
    });

    it('should extract documention from freedialectsample.cbl', async () => {
        const documentation = service.extract('./tests/resources/freedialectsample.cbl');
        console.log(documentation);
    });
});
