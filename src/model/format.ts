export enum Format {
    HTML = 'html',
    MD = 'md',
}

export enum TableOfContents {
    HTML = 'index.html',
    MD = 'README.md',
}

export namespace TableOfContents {
    export function from(format: Format): TableOfContents {
        switch (format) {
            case Format.HTML:
                return TableOfContents.HTML;
            case Format.MD:
                return TableOfContents.MD;
            default:
                throw new Error(`Not supported format: ${format}`);
        }
    }
}
