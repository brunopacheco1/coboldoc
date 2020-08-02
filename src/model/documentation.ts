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
    description: string,
    line: number,
    params?: Parameter[],
    return?: Return,
}

export interface CobolModule {
    name: string,
    description: string,
    line: number,
}

export interface ChangeLog {
    date: string,
    changedBy: string,
    description: string,
}

export interface Documentation {
    fileName: string,
    dialect: string,
    license?: string,
    author?: string,
    fileDescription?: string,
    paragraphs?: Paragraph[],
    functions?: CobolFunction[],
    modules?: CobolModule[],
    changeLogs?: ChangeLog[],
}
