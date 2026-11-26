// The tags of blockr.ui: React Aria's TagGroup, TagList and Tag
// (https://react-aria.adobe.com/TagGroup), drawn with the theme variables, after the
// TagGroup and the Tag of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/TagGroup.tsx),
// drawn as the chips of the ComboBox's field (./combobox.tsx).
// They take what those take: the props of React Aria's parts, and the label,
// the description, the error message, the items, what the list says where it
// has none, and the colour that the starter kit adds.
// A group holds its tags, each with its text as its children, which the user
// picks where the group's selectionMode picks, and removes by its button
// where the group takes onRemove.
// A tag group takes no `bound`: a caller that picks or removes with it holds
// what it picks itself.

import { createContext, useContext } from "react";
import type { ReactElement, ReactNode } from "react";
import { Button, composeRenderProps, Tag as AriaTag, TagGroup as AriaTagGroup, TagList, Text } from "react-aria-components";
import type { TagGroupProps as AriaTagGroupProps, TagListProps, TagProps as AriaTagProps } from "react-aria-components";
import { styles } from "../styles/index.ts";
import { Icon } from "../icon/index.ts";
import { X } from "../vendor/index.ts";
import { Description, Label } from "./inputs.tsx";

// The colours of a tag: a quiet fill with the edge of an outline, "gray", or
// with an edge in the brand colour, "accent".
const colors = {
  gray: "bk:border-border bk:bg-muted",
  accent: "bk:border-primary bk:bg-muted",
} as const;

type Color = keyof typeof colors;

const ColorContext = createContext<Color>("gray");

export interface TagGroupProps<T> extends Omit<AriaTagGroupProps, "children">, Pick<TagListProps<T>, "items" | "children" | "renderEmptyState"> {
  // What the starter kit adds.
  readonly color?: Color;
  readonly label?: string;
  readonly description?: string;
  readonly errorMessage?: string;
}

export function TagGroup<T extends object>({
  label,
  description,
  errorMessage,
  items,
  children,
  renderEmptyState,
  color = "gray",
  className,
  ...props
}: TagGroupProps<T>): ReactElement {
  return (
    <AriaTagGroup {...props} className={`${styles.field} ${className ?? ""}`}>
      {label === undefined ? null : <Label>{label}</Label>}
      <ColorContext.Provider value={color}>
        <TagList items={items} renderEmptyState={renderEmptyState} className="bk:flex bk:min-w-0 bk:flex-wrap bk:gap-1">
          {children}
        </TagList>
      </ColorContext.Provider>
      {description === undefined ? null : <Description>{description}</Description>}
      {errorMessage === undefined ? null : (
        <Text slot="errorMessage" className={styles.alert}>
          {errorMessage}
        </Text>
      )}
    </AriaTagGroup>
  );
}

export interface TagProps extends AriaTagProps {
  // What the starter kit adds, the group's colour unless given.
  readonly color?: Color;
  readonly children?: ReactNode;
}

// A tag shows its text, and the button that removes it where its group takes
// onRemove.
// A picked tag takes the accent colour.
export function Tag({ children, color, className, ...props }: TagProps): ReactElement {
  const textValue = typeof children === "string" ? children : undefined;
  const groupColor = useContext(ColorContext);
  return (
    <AriaTag
      textValue={textValue}
      {...props}
      className={composeRenderProps(className, (given) => `${TAG} ${colors[color ?? groupColor]} ${given ?? ""}`)}
    >
      {composeRenderProps(children, (children, { allowsRemoving }) => (
        <>
          <span className="bk:truncate">{children}</span>
          {allowsRemoving ? (
            <Button slot="remove" className={REMOVE}>
              <Icon icon={X} size={10} />
            </Button>
          ) : null}
        </>
      ))}
    </AriaTag>
  );
}

// The classes of a tag and of its button, as of a chip of the ComboBox's
// field.
const TAG =
  "bk:inline-flex bk:max-w-full bk:min-w-0 bk:cursor-default bk:items-center bk:gap-0.5 bk:rounded-sm bk:border bk:px-1.5 bk:py-0 bk:font-sans bk:text-xs bk:leading-5 bk:outline-none bk:data-allows-removing:pr-0.5 bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-selected:border-primary bk:data-selected:bg-primary bk:data-selected:text-primary-foreground bk:data-disabled:opacity-50";
const REMOVE =
  "bk:flex bk:size-4 bk:shrink-0 bk:cursor-pointer bk:items-center bk:justify-center bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-inherit bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground";
