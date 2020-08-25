import { PreDocumentation, Documentation, CobolFunction, PreCobolClass, CobolClass, PreCobolFunction } from '../model/documentation';

export interface CommentsParser {
    parse(preDocumentation: PreDocumentation): Documentation;
}

export abstract class BaseCommentsParser {

    public parse(preDocumentation: PreDocumentation): Documentation {
        const [fileDescription, author, license] = this._extractFileDetails(preDocumentation.fileComments);
        const modules = preDocumentation.modules.map(
            preModule => this._extractFunction(preModule),
        );
        const functions = preDocumentation.functions.map(
            preFunction => this._extractFunction(preFunction),
        );

        const classes = preDocumentation.classes.map(
            preClass => this._extractClass(preClass),
        )

        return {
            fileName: preDocumentation.fileName,
            fileDescription: fileDescription,
            author: author,
            license: license,
            functions: functions,
            modules: modules,
            classes: classes,
            changeLogs: [],
        };
    }

    protected abstract _extractFileDetails(comments: string): [string | undefined, string | undefined, string | undefined];
    protected abstract _extractFunction(preCobolFunction: PreCobolFunction): CobolFunction;
    protected abstract _extractClass(preClass: PreCobolClass): CobolClass;
}
