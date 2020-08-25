export interface Paragraph {
    name: string,
    line: number,
    description?: string,
    calledBy?: Paragraph[],
    calls?: Paragraph[]
}

export interface Parameter {
    name: string,
    description: string,
    type?: string,
}

export interface Return {
    description: string,
    type?: string,
}

export interface CobolFunction {
    name: string,
    line: number,
    paragraphs: Paragraph[],
    description?: string,
    example?: string,
    summary?: string,
    params: Parameter[],
    return?: Return,
}

export interface CobolClass {
    name: string,
    line: number,
    methods: CobolFunction[],
    description?: string,
    example?: string,
    summary?: string,
}

export interface ChangeLog {
    date: string,
    changedBy: string,
    description: string,
}

export interface Documentation {
    fileName: string,
    license?: string,
    author?: string,
    fileDescription?: string,
    functions: CobolFunction[],
    modules: CobolFunction[],
    classes: CobolClass[],
    changeLogs: ChangeLog[],
}

export interface PreCobolFunction {
    name: string,
    line: number,
    comments: string,
}

export interface PreCobolClass extends PreCobolFunction {
    methods: PreCobolFunction[],
}

export interface PreDocumentation {
    fileName: string,
    fileComments: string,
    functions: PreCobolFunction[],
    modules: PreCobolFunction[],
    classes: PreCobolClass[],
}
