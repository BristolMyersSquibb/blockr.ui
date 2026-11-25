export interface Checked {
    name: string;
    text: string;
}
export interface Problem {
    file: string;
    line: number;
    message: string;
}
export declare function themeProblems(stylesheets: readonly Checked[], code: readonly Checked[], own: boolean): Problem[];
export declare function themeError(problems: readonly Problem[]): Error;
export declare function themeCheck(files: {
    stylesheets?: readonly string[];
    code?: readonly string[];
}): Promise<void>;
