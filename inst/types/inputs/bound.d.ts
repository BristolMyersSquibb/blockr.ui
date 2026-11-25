export type Json = null | boolean | number | string | readonly Json[] | {
    readonly [key: string]: Json;
};
export interface Bound {
    readonly value: Json | undefined;
    readonly error: string | null;
    get(): Json | undefined;
    commit(value: Json): Promise<boolean>;
    unset(): Promise<boolean>;
    type(value: Json | undefined): void;
    flush(): void;
}
