import { PreDocumentation, Documentation, ModuleOrFunction, PreModuleOrFunction } from '../model/documentation';

export interface CommentsParser {
    parse(preDocumentation: PreDocumentation): Documentation;
}

export abstract class BaseCommentsParser {

    public parse(preDocumentation: PreDocumentation): Documentation {
        const [fileDescription, author, license] = this._extractFileDetails(preDocumentation.fileComments);
        const modules = preDocumentation.modules.map(
            preModule => this._extractModuleOrFunction(preModule),
        );
        const functions = preDocumentation.functions.map(
            preFunction => this._extractModuleOrFunction(preFunction),
        );

        return {
            fileName: preDocumentation.fileName,
            fileDescription: fileDescription,
            author: author,
            license: license,
            functions: functions,
            modules: modules,
        };
    }

    protected abstract _extractFileDetails(comments: string): [string | undefined, string | undefined, string | undefined];
    protected abstract _extractModuleOrFunction(preModuleOrFunction: PreModuleOrFunction): ModuleOrFunction;
}
