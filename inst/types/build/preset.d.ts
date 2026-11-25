import type { Plugin, PluginBuild } from "esbuild";
export declare const VERSIONS: {
    readonly "react-aria-components": "1.22.1";
    readonly "react-aria": "3.53.1";
    readonly "react-stately": "3.51.0";
};
export declare function leaveReact(build: PluginBuild): void;
export declare function installedVersion(name: string, dir: string): string | undefined;
export declare const preset: {
    bundle: true;
    format: "esm";
    platform: "browser";
    target: string;
    jsx: "automatic";
    define: {
        "process.env.NODE_ENV": string;
    };
    plugins: Plugin[];
};
