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

export interface ModuleOrFunction {
    name: string,
    line: number,
    paragraphs?: Paragraph[],
    description?: string,
    example?: string,
    summary?: string,
    params?: Parameter[],
    return?: Return,
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
    functions?: ModuleOrFunction[],
    modules?: ModuleOrFunction[],
    changeLogs?: ChangeLog[],
}

export interface PreModuleOrFunction {
    name: string,
    line: number,
    comments: string,
}

export interface PreDocumentation {
    fileName: string,
    fileComments: string,
    functions: PreModuleOrFunction[],
    modules: PreModuleOrFunction[],
}
