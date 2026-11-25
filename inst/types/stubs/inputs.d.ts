export declare function find(el: ParentNode, selector: string): HTMLElement;
export declare function find<E extends Element>(el: ParentNode, selector: string, type: abstract new () => E): E;
export declare function texts(el: ParentNode, selector: string): string[];
export declare function type(input: HTMLInputElement | HTMLTextAreaElement, text: string): void;
export declare function fieldError(el: ParentNode): string | null;
export declare function numberSaid(text: string, { min, max, step }: {
    min?: number;
    max?: number;
    step?: number;
}): string | null;
export declare function press(input: HTMLElement, key: string, init?: KeyboardEventInit): void;
export declare function pressAndRelease(el: HTMLElement, key: string): void;
export declare function toggle(checkbox: HTMLInputElement): void;
export declare function choose(input: HTMLInputElement, file: File): void;
export declare function drop(target: HTMLElement, file: File): void;
export declare function picking(file: File | undefined): {
    accept: string;
    connected: boolean;
}[];
export declare function toggles(el: ParentNode, label: string): {
    readonly label: string;
    readonly picked: boolean;
}[];
export declare function toggleButton(el: ParentNode, group: string, label: string): HTMLButtonElement;
export declare function toggleGroup(el: ParentNode, label: string): HTMLElement;
