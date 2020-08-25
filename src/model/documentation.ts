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

export interface CobolProperty {
    name: string,
    line: number,
    description?: string,
    type?: string,
}

export interface CobolClass {
    name: string,
    line: number,
    methods: CobolFunction[],
    properties: CobolProperty[],
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

export interface PreCobolObject {
    name: string,
    line: number,
    comments: string,
}

export interface PreCobolProperty extends PreCobolObject {
    type?: string,
}

export interface PreCobolClass extends PreCobolObject {
    properties: PreCobolProperty[],
    methods: PreCobolObject[],
}

export interface PreDocumentation {
    fileName: string,
    fileComments: string,
    functions: PreCobolObject[],
    modules: PreCobolObject[],
    classes: PreCobolClass[],
}
