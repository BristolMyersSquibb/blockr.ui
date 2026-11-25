// Where the overlays of blockr.ui stack: at the z-index that the theme variable
// --bk-z-index-overlay gives them
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#the-defaults).
// React Aria writes a z-index of its own on a popover and a tooltip, in their
// style, which blockr.ui's popovers, its tooltip and the select's list take in
// place of it.
// It lives with the overlays, since it reads React's types.

import type { CSSProperties } from "react";
import { composeRenderProps } from "react-aria-components";

export const OVERLAY_Z_INDEX = "var(--bk-z-index-overlay)";

// The style of an overlay of React Aria, which React Aria merges over its own,
// with the overlays' z-index in place of any that it holds.
export function overlayStyle<V>(
  style: CSSProperties | ((values: V) => CSSProperties | undefined) | undefined,
): (values: V) => CSSProperties | undefined {
  return composeRenderProps(style, (given: CSSProperties | undefined) => ({ ...given, zIndex: OVERLAY_Z_INDEX }));
}
