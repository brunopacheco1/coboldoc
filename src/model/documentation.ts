export interface Paragraph {
    name: string,
    description: string,
    line: number,
    calledBy: Paragraph[],
    calls: Paragraph[]
}

export interface Parameter {
    name: string,
    description: string,
    type: string,
}

export interface Return {
    description: string,
    type: string,
}

export interface CobolFunction {
    name: string,
    params: Parameter[],
    return: Return,
    line: number,
}

export interface Documentation {
    fileName: string,
    license: string,
    author: string,
    programId: string,
    dialect: string,
    programDescription: string,
    paragraphs: Paragraph[],
    functions: CobolFunction[],
}
