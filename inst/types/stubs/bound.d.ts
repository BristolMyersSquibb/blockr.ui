import type { Bound, Json } from "../inputs/index.ts";
export interface StubBound extends Bound {
    readonly committed: readonly (Json | undefined)[];
    readonly typed: readonly (Json | undefined)[];
    readonly flushed: readonly (Json | undefined)[];
    set(value: Json | undefined): void;
    refuse(error: string | null): void;
    readonly subscribe: (listener: () => void) => () => void;
}
export declare function stubBound(value?: Json): StubBound;
export declare function useStubBound(stub: StubBound): Bound;
