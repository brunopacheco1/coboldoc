import { PreDocumentation, Documentation, CobolFunction, PreCobolClass, CobolClass, PreCobolObject, CobolProperty, PreCobolProperty } from '../model/documentation';

export interface CommentsParser {
    parse(preDocumentation: PreDocumentation): Documentation;
}

export abstract class BaseCommentsParser {

    public parse(preDocumentation: PreDocumentation): Documentation {
        const [fileDescription, author, license] = this._extractFileDetails(preDocumentation.fileComments);
        const modules = preDocumentation.modules.map(preModule => this._extractFunction(preModule));
        const functions = preDocumentation.functions.map(preFunction => this._extractFunction(preFunction));

        const classes = preDocumentation.classes.map(
            preClass => {
                const methods = preClass.methods.map(
                    preMethod => this._extractFunction(preMethod)
                );
                const properties = preClass.properties.map(
                    preProperty => this._extractProperty(preProperty)
                );

                const clazz = this._extractClass(preClass);
                clazz.methods = methods;
                clazz.properties = properties;
                return clazz;
            }
        );

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
    protected abstract _extractFunction(preCobolFunction: PreCobolObject): CobolFunction;
    protected abstract _extractClass(preClass: PreCobolClass): CobolClass;
    protected abstract _extractProperty(preProperty: PreCobolProperty): CobolProperty;
}
