// srcts/stubs/dom.ts
for (const name of ["clientWidth", "clientHeight"]) {
  if (typeof HTMLElement === "undefined") break;
  const getter = Object.getOwnPropertyDescriptor(HTMLElement.prototype, name);
  if (getter === void 0) continue;
  Reflect.deleteProperty(HTMLElement.prototype, name);
  if (!Object.hasOwn(Element.prototype, name)) Object.defineProperty(Element.prototype, name, getter);
}
for (const name of ["min", "max", "step"]) {
  if (typeof HTMLInputElement === "undefined") break;
  const property = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, name);
  if (property?.set === void 0) continue;
  Object.defineProperty(HTMLInputElement.prototype, name, {
    ...property,
    set(value) {
      if (value === "") this.removeAttribute(name);
      else property.set?.call(this, value);
    }
  });
}
if (typeof document !== "undefined") {
  const view = document.defaultView;
  if (view !== null && !(document instanceof view.Document)) {
    const own = Object.getPrototypeOf(view.Document);
    Object.defineProperty(view.Document, Symbol.hasInstance, { value: (value) => value instanceof own });
  }
}

// srcts/stubs/bound.ts
import { useSyncExternalStore } from "react";
function stubBound(value) {
  let held = value;
  let error = null;
  let refusal = null;
  let pending = null;
  const committed = [];
  const typed = [];
  const flushed = [];
  const listeners = /* @__PURE__ */ new Set();
  const changed = () => {
    for (const listener of [...listeners]) listener();
  };
  const submit = (next) => {
    pending = null;
    if (refusal !== null) {
      error = refusal;
      changed();
      return Promise.resolve(false);
    }
    held = next;
    error = null;
    changed();
    return Promise.resolve(true);
  };
  return {
    get value() {
      return held;
    },
    get error() {
      return error;
    },
    committed,
    typed,
    flushed,
    get: () => pending === null ? held : pending.value,
    commit: (next) => {
      committed.push(next);
      return submit(next);
    },
    unset: () => {
      committed.push(void 0);
      return submit(void 0);
    },
    type: (next) => {
      typed.push(next);
      pending = { value: next };
    },
    flush: () => {
      if (pending === null) return;
      flushed.push(pending.value);
      void submit(pending.value);
    },
    set: (next) => {
      held = next;
      changed();
    },
    refuse: (next) => {
      refusal = next;
    },
    subscribe: (listener) => {
      listeners.add(listener);
      return () => listeners.delete(listener);
    }
  };
}
function useStubBound(stub) {
  return useSyncExternalStore(stub.subscribe, () => snapshotOf(stub));
}
var snapshots = /* @__PURE__ */ new WeakMap();
function snapshotOf(stub) {
  const last = snapshots.get(stub);
  if (last !== void 0 && Object.is(last.value, stub.value) && last.error === stub.error) return last;
  const next = {
    value: stub.value,
    error: stub.error,
    get: () => stub.get(),
    commit: (value) => stub.commit(value),
    unset: () => stub.unset(),
    type: (value) => {
      stub.type(value);
    },
    flush: () => {
      stub.flush();
    }
  };
  snapshots.set(stub, next);
  return next;
}

// srcts/stubs/inputs.ts
import { vi } from "vitest";
function find(el, selector, type2 = HTMLElement) {
  const found = el.querySelector(selector);
  if (!(found instanceof type2)) throw new Error(`Found no ${type2.name} ${selector}.`);
  return found;
}
function texts(el, selector) {
  return [...el.querySelectorAll(selector)].map((node) => node.textContent);
}
function type(input, text) {
  input.focus();
  const steps = text === "" ? [""] : Array.from({ length: text.length }, (_, i) => text.slice(0, i + 1));
  for (const step2 of steps) {
    setValue(input, step2);
    input.dispatchEvent(new Event("input", { bubbles: true }));
  }
}
function fieldError(el) {
  return el.querySelector('[slot="errorMessage"]')?.textContent ?? null;
}
function numberSaid(text, { min, max, step: step2 }) {
  const input = document.createElement("input");
  input.type = "number";
  if (min !== void 0) input.min = String(min);
  if (max !== void 0) input.max = String(max);
  if (step2 !== void 0) input.step = String(step2);
  input.value = text;
  return input.validationMessage === "" ? null : input.validationMessage;
}
function press(input, key, init = {}) {
  input.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true, ...init }));
}
function pressAndRelease(el, key) {
  el.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true, cancelable: true }));
  el.dispatchEvent(new KeyboardEvent("keyup", { key, bubbles: true, cancelable: true }));
}
function toggle(checkbox) {
  checkbox.click();
}
function choose(input, file) {
  Object.defineProperty(input, "files", { value: [file], configurable: true });
  input.dispatchEvent(new Event("change", { bubbles: true }));
}
function drop(target, file) {
  for (const name of ["dragenter", "dragover", "drop"]) {
    const transfer = new DataTransfer();
    transfer.items.add(file);
    const event = new DragEvent(name, { bubbles: true, cancelable: true, clientX: 1, clientY: 1 });
    Object.defineProperty(event, "dataTransfer", { value: transfer });
    target.dispatchEvent(event);
  }
}
function picking(file) {
  const clicked = [];
  vi.spyOn(HTMLInputElement.prototype, "click").mockImplementation(function() {
    clicked.push({ accept: this.accept, connected: this.isConnected });
    if (file === void 0) this.dispatchEvent(new Event("cancel"));
    else choose(this, file);
  });
  return clicked;
}
function setValue(element, value) {
  if (!Reflect.set(Object.getPrototypeOf(element), "value", value, element)) {
    throw new Error("The element has no value to set.");
  }
}
function toggles(el, label) {
  return [...toggleGroup(el, label).querySelectorAll("button")].map((button) => ({
    label: button.textContent.trim(),
    picked: button.getAttribute("aria-checked") === "true" || button.getAttribute("aria-pressed") === "true"
  }));
}
function toggleButton(el, group, label) {
  const found = [...toggleGroup(el, group).querySelectorAll("button")].find((button) => button.textContent.trim() === label);
  if (found === void 0) throw new Error(`Found no toggle button ${label} in ${group}.`);
  return found;
}
function toggleGroup(el, label) {
  const found = [...el.querySelectorAll('[role="radiogroup"], [role="toolbar"]')].find((group) => {
    const id = group.getAttribute("aria-labelledby") ?? "";
    const root = group.getRootNode();
    const named = id === "" ? null : root.querySelector(`[id="${CSS.escape(id)}"]`);
    return (named?.textContent.trim() ?? group.getAttribute("aria-label")) === label;
  });
  if (found === void 0) throw new Error(`Found no group of toggle buttons labelled ${label}.`);
  return found;
}

// srcts/stubs/react.ts
import { act } from "react";
import { createRoot } from "react-dom/client";
globalThis.IS_REACT_ACT_ENVIRONMENT = true;
async function step(fn) {
  await act(() => {
    fn();
    return Promise.resolve();
  });
}
function draw(node) {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  const render = (next) => {
    act(() => {
      root.render(next);
    });
  };
  render(node);
  return {
    el,
    redraw: render,
    unmount: () => {
      act(() => {
        root.unmount();
      });
      el.remove();
    }
  };
}

// srcts/stubs/overlays.ts
function openDialog() {
  return find(document, '[role="dialog"]');
}
function openMenu(label) {
  return find(document, `[role="menu"][aria-label="${label}"]`);
}
function menuItem(menu, label) {
  const found = [...menu.querySelectorAll('[role="menuitem"]')].find((item) => itemLabel(item) === label);
  if (found === void 0) throw new Error(`Found no menu item ${label}.`);
  return found;
}
function menuItems(menu) {
  return [...menu.querySelectorAll('[role="menuitem"]')].map(itemLabel);
}
function itemLabel(item) {
  const id = (item.getAttribute("aria-labelledby") ?? "").split(" ")[0] ?? "";
  return ((id === "" ? null : item.ownerDocument.getElementById(id)) ?? item).textContent.trim();
}
function itemDescribed(item) {
  return (item.getAttribute("aria-describedby") ?? "").split(" ").flatMap((id) => id === "" ? [] : [item.ownerDocument.getElementById(id)?.textContent.trim() ?? ""]).filter((text) => text !== "");
}
function menuGroup(menu, title) {
  const found = [...menu.querySelectorAll('[role="group"]')].find((group) => {
    const heading = group.ownerDocument.getElementById(group.getAttribute("aria-labelledby") ?? "");
    return heading?.textContent.trim() === title;
  });
  if (found === void 0) throw new Error(`Found no group ${title}.`);
  return found;
}
function inert(el) {
  for (let node = el; node !== null; node = node.parentElement) {
    if (node.hasAttribute("inert") || node instanceof HTMLElement && node.inert) return true;
  }
  return false;
}
function shownTip() {
  return document.querySelector('[role="tooltip"]')?.textContent ?? null;
}
async function tipOnFocus(el) {
  await step(() => {
    el.ownerDocument.dispatchEvent(new KeyboardEvent("keydown", { key: "Tab", bubbles: true }));
    el.focus();
  });
  return shownTip();
}
function pointTo(el) {
  el.dispatchEvent(new PointerEvent("pointermove", { bubbles: true, pointerType: "mouse" }));
  el.dispatchEvent(new PointerEvent("pointerover", { bubbles: true, pointerType: "mouse" }));
}
async function pointerPress(el) {
  const pointer = { bubbles: true, cancelable: true, composed: true, pointerId: 1, pointerType: "mouse", isPrimary: true, button: 0 };
  await step(() => {
    el.dispatchEvent(new PointerEvent("pointerdown", { ...pointer, buttons: 1 }));
    el.dispatchEvent(new PointerEvent("pointerup", { ...pointer, buttons: 0 }));
    el.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, composed: true, button: 0, detail: 1 }));
  });
}

// srcts/stubs/rows.ts
function rows(el) {
  return [...el.querySelectorAll('[role="row"][data-key]')];
}
function rowTexts(el) {
  return rows(el).map((row) => row.textContent);
}
function rowOf(el, text) {
  const found = rows(el).find((row) => row.textContent === text);
  if (found === void 0) throw new Error(`Found no row ${text}.`);
  return found;
}
function rowCheckbox(row) {
  return find(row, 'input[type="checkbox"]', HTMLInputElement);
}

// srcts/stubs/select.ts
async function listed(field, act2) {
  await act2(() => {
    field.click();
  });
  const rows2 = optionLabels(field);
  await act2(() => {
    press(focused(field), "Escape");
  });
  return rows2;
}
async function picked(field, act2, ...labels) {
  await act2(() => {
    field.click();
  });
  for (const label of labels) {
    await act2(() => {
      optionFromLabel(field, label).click();
    });
  }
  if (listFromField(field) !== null) {
    await act2(() => {
      press(focused(field), "Escape");
    });
  }
}
function focused(field) {
  const active = field.ownerDocument.activeElement;
  const list = listFromField(field);
  return active instanceof HTMLElement && (active === field || list?.contains(active) === true) ? active : field;
}
function selectButton(el, label) {
  const found = [...el.querySelectorAll('button[aria-haspopup="listbox"]')].find(
    (button) => (button.getAttribute("aria-labelledby") ?? "").split(" ").some((id) => byId(button, id)?.textContent.trim() === label)
  );
  if (!(found instanceof HTMLButtonElement)) throw new Error(`Found no select labelled ${label}.`);
  return found;
}
function selectShows(button) {
  const [id] = (button.getAttribute("aria-labelledby") ?? "").split(" ");
  return byId(button, id ?? "")?.textContent.trim() ?? "";
}
function labelled(el, label) {
  const found = [...el.querySelectorAll("label")].find((l) => l.textContent.trim() === label);
  const field = found === void 0 ? null : byId(found, found.htmlFor) ?? found.querySelector("input");
  if (!(field instanceof HTMLInputElement)) throw new Error(`Found no text field labelled ${label}.`);
  return field;
}
function listFromField(field) {
  const id = field.getAttribute("aria-controls");
  return id === null ? null : byId(field, id);
}
function optionLabels(field) {
  const list = listFromField(field);
  if (list === null || list.hasAttribute("data-empty")) return [];
  return [...list.querySelectorAll('[role="option"]')].map((option) => {
    const section = option.closest('[role="group"]');
    const heading = section === null ? null : byId(field, section.getAttribute("aria-labelledby") ?? "");
    const label = optionLabel(option);
    return heading === null ? label : `${heading.textContent}: ${label}`;
  });
}
function optionFromLabel(field, label) {
  const found = [...listFromField(field)?.querySelectorAll('[role="option"]') ?? []].find((option) => optionLabel(option) === label);
  if (!(found instanceof HTMLElement)) throw new Error(`Found no option ${label} of ${field.id}.`);
  return found;
}
function pickedLabels(field) {
  return [...listFromField(field)?.querySelectorAll('[role="option"][aria-selected="true"]') ?? []].map(optionLabel);
}
function listNote(field) {
  return listFromField(field)?.querySelector("p")?.textContent ?? null;
}
function markedLabel(field) {
  const id = field.getAttribute("aria-activedescendant");
  const option = id === null ? null : byId(field, id);
  return option === null ? null : optionLabel(option);
}
function chipLabels(field) {
  return chips(field).map((chip) => chip.textContent.trim());
}
function chipFromLabel(field, label) {
  const found = chips(field).find((chip) => chip.textContent.trim() === label);
  if (!(found instanceof HTMLElement)) throw new Error(`Found no chip ${label} of ${field.id}.`);
  return found;
}
async function removeChip(field, act2, label) {
  const button = chipFromLabel(field, label).querySelector("button");
  if (!(button instanceof HTMLElement)) throw new Error(`Found no button that removes the chip ${label} of ${field.id}.`);
  await act2(() => {
    button.click();
  });
}
function clearButton(field) {
  const found = field.closest('[role="group"]')?.querySelector(':scope > button[aria-label^="Clear"]');
  return found instanceof HTMLButtonElement ? found : null;
}
async function cleared(field, act2) {
  const button = clearButton(field);
  if (button === null) throw new Error(`Found no button that clears ${field.id}.`);
  await act2(() => {
    button.click();
  });
}
function chips(field) {
  return [...field.closest('[role="group"]')?.querySelectorAll('[role="row"], [role="listitem"]') ?? []];
}
function byId(node, id) {
  if (id === "") return null;
  const root = node.getRootNode();
  const found = root.getElementById?.(id) ?? root.querySelector?.(`[id="${CSS.escape(id)}"]`) ?? node.ownerDocument?.getElementById(id) ?? null;
  return found instanceof HTMLElement ? found : null;
}
function optionLabel(option) {
  const id = option.getAttribute("aria-labelledby");
  const label = id === null ? null : byId(option, id.split(" ")[0] ?? "");
  return (label ?? option).textContent.trim();
}
function notAvailable(el) {
  return [...el.querySelectorAll('[role="alert"]')].map((line) => line.textContent.trim()).filter((text) => text.startsWith("Not available"));
}
export {
  chipFromLabel,
  chipLabels,
  choose,
  clearButton,
  cleared,
  draw,
  drop,
  fieldError,
  find,
  inert,
  itemDescribed,
  labelled,
  listFromField,
  listNote,
  listed,
  markedLabel,
  menuGroup,
  menuItem,
  menuItems,
  notAvailable,
  numberSaid,
  openDialog,
  openMenu,
  optionFromLabel,
  optionLabels,
  picked,
  pickedLabels,
  picking,
  pointTo,
  pointerPress,
  press,
  pressAndRelease,
  removeChip,
  rowCheckbox,
  rowOf,
  rowTexts,
  rows,
  selectButton,
  selectShows,
  shownTip,
  step,
  stubBound,
  texts,
  tipOnFocus,
  toggle,
  toggleButton,
  toggleGroup,
  toggles,
  type,
  useStubBound
};
