import type { ReactElement } from "react";
export declare function step(fn: () => void): Promise<void>;
export declare function draw(node: ReactElement): {
    el: HTMLElement;
    redraw: (next: ReactElement) => void;
    unmount: () => void;
};
