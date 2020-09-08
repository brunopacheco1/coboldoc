export enum Format {
    HTML = 'html',
    MD = 'md',
    MSDN = 'msdn',
}

export enum TableOfContents {
    HTML = 'index.html',
    MD = 'README.md',
}

export namespace TableOfContents {
    export function from(format: Format): TableOfContents | undefined {
        switch (format) {
            case Format.HTML:
                return TableOfContents.HTML;
            case Format.MD:
                return TableOfContents.MD;
            case Format.MSDN:
                return undefined;
            default:
                throw new Error(`Not supported format: ${format}`);
        }
    }
}

export enum OutputFormat {
    HTML = 'html',
    MD = 'md',
    MSDN = 'xml',
}

export namespace OutputFormat {
    export function from(format: Format): OutputFormat {
        switch (format) {
            case Format.HTML:
                return OutputFormat.HTML;
            case Format.MD:
                return OutputFormat.MD;
            case Format.MSDN:
                return OutputFormat.MSDN;
            default:
                throw new Error(`Not supported format: ${format}`);
        }
    }
}
