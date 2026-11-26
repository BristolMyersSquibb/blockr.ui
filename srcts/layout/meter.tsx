// The meter and the progress bar of blockr.ui: React Aria's Meter
// (https://react-aria.adobe.com/Meter) and ProgressBar
// (https://react-aria.adobe.com/ProgressBar), drawn with the theme variables, after the
// Meter and the ProgressBar of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Meter.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ProgressBar.tsx).
// Each takes the props of its part of React Aria, such as `value` between
// `minValue` and `maxValue`, 0 and 100 by default, and the starter kit's
// `label`.
// Each shows its label and its value, as text, over a bar that fills as far
// as the value goes.
// The meter shows how much of something is taken, such as storage: its bar is
// green below 70%, orange from 70%, and red from 80%, where its text turns
// red as well and shows a warning sign.
// The progress bar shows how far a task has come, in the accent colour.
// Where it is `isIndeterminate`, as where the task cannot tell, it shows no
// value, and a part of the bar slides across it, but not while the user
// prefers reduced motion.

import type { ReactElement } from "react";
import { composeRenderProps, Meter as AriaMeter, ProgressBar as AriaProgressBar } from "react-aria-components";
import type { MeterProps as AriaMeterProps, ProgressBarProps as AriaProgressBarProps } from "react-aria-components";
import { Icon } from "../icon/index.ts";
import { Label } from "../inputs/index.ts";
import { TriangleExclamationPoint } from "../vendor/index.ts";

export interface MeterProps extends AriaMeterProps {
  readonly label?: string;
}

export function Meter({ label, className, ...props }: MeterProps): ReactElement {
  return (
    <AriaMeter {...props} className={composeRenderProps(className, (given) => `${METER} ${given ?? ""}`)}>
      {({ percentage, valueText }) => (
        <>
          <div className="bk:flex bk:justify-between bk:gap-2">
            <Label>{label}</Label>
            <span className={`bk:text-sm ${percentage >= 80 ? "bk:text-destructive-text" : "bk:text-muted-foreground"}`}>
              {percentage >= 80 ? <Alert /> : null}
              {` ${valueText ?? ""}`}
            </span>
          </div>
          <div className={`${TRACK} bk:w-64`}>
            <div className={`${FILL} bk:left-0 ${fill(percentage)}`} style={{ width: `${String(percentage)}%` }} />
          </div>
        </>
      )}
    </AriaMeter>
  );
}

export interface ProgressBarProps extends AriaProgressBarProps {
  readonly label?: string;
}

export function ProgressBar({ label, className, ...props }: ProgressBarProps): ReactElement {
  return (
    <AriaProgressBar {...props} className={composeRenderProps(className, (given) => `${PROGRESS} ${given ?? ""}`)}>
      {({ percentage, valueText, isIndeterminate }) => (
        <>
          <div className="bk:flex bk:justify-between bk:gap-2">
            <Label>{label}</Label>
            <span className="bk:text-sm bk:text-muted-foreground">{valueText}</span>
          </div>
          <div className={`${TRACK} bk:overflow-hidden`}>
            <div
              className={`${FILL} bk:left-0 bk:bg-primary ${isIndeterminate ? "bk:motion-safe:animate-[bk-progress-slide_1s_ease-out_infinite]" : ""}`}
              style={{ width: `${String(isIndeterminate ? 40 : (percentage ?? 0))}%` }}
            />
          </div>
        </>
      )}
    </AriaProgressBar>
  );
}

// The colour of the meter's bar at the share given, in percent: that of a
// success below 70%, of a warning below 80%, and of an error from there.
function fill(percentage: number): string {
  if (percentage < 70) return "bk:bg-success";
  if (percentage < 80) return "bk:bg-warning";
  return "bk:bg-destructive";
}

// The warning sign of a meter from 80%, Lucide's TriangleExclamationPoint, as
// in the starter kit, which a screen reader says as "Alert".
function Alert(): ReactElement {
  return (
    <span className="bk:inline-flex bk:align-text-bottom">
      <Icon icon={TriangleExclamationPoint} size={16} label="Alert" />
    </span>
  );
}

const METER = "bk:flex bk:max-w-full bk:flex-col bk:gap-2 bk:font-sans";
const PROGRESS = "bk:flex bk:w-64 bk:max-w-full bk:flex-col bk:gap-2 bk:font-sans";
// The track keeps a transparent outline, which forced colours draw.
const TRACK =
  "bk:relative bk:h-2 bk:max-w-full bk:rounded-full bk:bg-border bk:outline bk:outline-1 bk:-outline-offset-1 bk:outline-transparent";
const FILL = "bk:absolute bk:top-0 bk:h-full bk:rounded-full bk:forced-colors:bg-[Highlight]";
