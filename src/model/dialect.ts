export enum Dialect {
    FREE = 'free',
    MICROFOCUS = 'microfocus',
}

export namespace CommentsRegex {
    export function from(dialect: Dialect): CommentsRegex {
        switch (dialect) {
            case Dialect.FREE:
                return {
                    fileCommentsRegex: /^\s*\*>\*\*.*/,
                    documentationRegex: /^\s*\*>\*.*/,
                    contentRegex: /^\s*\*>[^*].*/,
                    cleanRegex: /^\s*\*>\**\s*/,
                };
            case Dialect.MICROFOCUS:
                return {
                    fileCommentsRegex: /^\s*\*>>\*\*.*/,
                    documentationRegex: /^\s*\*>>\*.*/,
                    contentRegex: /^\s*\*>>[^*].*/,
                    cleanRegex: /^\s*\*>>\**\s*/,
                };
            default:
                throw new Error(`Not supported dialect: ${dialect}`);
        }
    }
}

export interface CommentsRegex {
    fileCommentsRegex: RegExp,
    documentationRegex: RegExp,
    contentRegex: RegExp,
    cleanRegex: RegExp,
}