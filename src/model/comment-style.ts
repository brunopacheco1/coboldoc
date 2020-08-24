export enum CommentStyle {
    FREE = 'free',
    MICROFOCUS = 'microfocus',
}

export namespace CommentsRegex {
    export function from(commentStyle: CommentStyle): CommentsRegex {
        switch (commentStyle) {
            case CommentStyle.FREE:
                return {
                    fileCommentsRegex: /^\s*\*>\*\*.*/,
                    documentationRegex: /^\s*\*>\*.*/,
                    contentRegex: /^\s*\*>[^*].*/,
                    cleanRegex: /^\s*\*>\**\s*/,
                };
            case CommentStyle.MICROFOCUS:
                return {
                    fileCommentsRegex: /^\s*\*>>>.*/,
                    documentationRegex: /^\s*\*>>.*/,
                    contentRegex: /^\s*\*>>[^>].*/,
                    cleanRegex: /^\s*\*>>\s*/,
                };
            default:
                throw new Error(`Not supported dialect: ${commentStyle}`);
        }
    }
}

export interface CommentsRegex {
    fileCommentsRegex: RegExp,
    documentationRegex: RegExp,
    contentRegex: RegExp,
    cleanRegex: RegExp,
}
