// What makes happy-dom, the DOM that the tests of blockr.ui and of packages
// run on, do as a browser does where blockr.ui's components need it, once it
// loads, as the stubs load it (./index.ts):
// - In happy-dom, every element measures 0 by 0 pixels, through getters of
//   clientWidth and clientHeight on the prototype of HTMLElement, where a
//   browser's sit on the prototype of Element.
//   React Aria's Virtualizer takes getters on the prototype of HTMLElement
//   for those of a test that sizes the page, and then draws the rows that fit
//   0 pixels, which are none.
//   So the getters move to the prototype of Element, and the Virtualizer
//   draws every row in a test, as React Aria does for a DOM that sizes
//   nothing.
//   A test that checks which rows the Virtualizer draws defines the getters
//   on the prototype of HTMLElement again, with the sizes it wants.
// - In happy-dom, an empty min, max or step of a number input reads as 0,
//   where a browser reads none, and React Aria's NumberField empties them on
//   the number input that it checks a number with.
//   So setting one of them to "" removes the attribute.
//   This is happy-dom's bug #2484
//   (https://github.com/capricorn86/happy-dom/issues/2484), which its PR
//   #2516 fixes, and this part goes once a release of happy-dom holds the
//   fix.
// - In happy-dom, the page's document is no instance of the window's
//   Document.
//   The window's Document is a class that extends happy-dom's own Document.
//   The page's document is an instance of happy-dom's own Document, but not
//   of the window's.
//   React Aria finds the focus within an element only where the root of the
//   element is an instance of the window's Document.
//   So in happy-dom, a popover or a dialog of blockr.ui takes the focus as a
//   pointer press opens it.
//   It does so even where one of its elements, such as a search field with
//   autoFocus, has taken the focus already.
//   This part makes every document of happy-dom pass as an instance of the
//   window's Document.
//   This is happy-dom's bug #1781
//   (https://github.com/capricorn86/happy-dom/issues/1781), and this part
//   goes once a release of happy-dom fixes it.
// - In happy-dom, the document fires selectionchange at once as the selection
//   changes.
//   It fires the event also as the selection collapses where it is collapsed
//   already.
//   A browser fires the event later, and only where the selection moved.
//   While one of React Aria's date segments has the focus, the segment
//   collapses the selection into itself on each selectionchange.
//   In happy-dom, that collapse fires selectionchange again, at once, and the
//   segment collapses the selection again, until the stack overflows.
//   So a collapse to where the selection is collapsed already does nothing,
//   and a collapse without an offset collapses to the offset 0, as in a
//   browser.
//   This is happy-dom's bug #2185
//   (https://github.com/capricorn86/happy-dom/issues/2185), and this part
//   goes once a release of happy-dom fires the event as browsers do.
// Each part does no harm where it has done its work before, as where core's
// stubs did the same, and outside happy-dom, none of them changes what the
// DOM does.

for (const name of ["clientWidth", "clientHeight"]) {
  if (typeof HTMLElement === "undefined") break;
  const getter = Object.getOwnPropertyDescriptor(HTMLElement.prototype, name);
  if (getter === undefined) continue;
  Reflect.deleteProperty(HTMLElement.prototype, name);
  if (!Object.hasOwn(Element.prototype, name)) Object.defineProperty(Element.prototype, name, getter);
}

for (const name of ["min", "max", "step"]) {
  if (typeof HTMLInputElement === "undefined") break;
  const property = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, name);
  if (property?.set === undefined) continue;
  Object.defineProperty(HTMLInputElement.prototype, name, {
    ...property,
    set(this: HTMLInputElement, value: string) {
      if (value === "") this.removeAttribute(name);
      else property.set?.call(this, value);
    },
  });
}

if (typeof document !== "undefined") {
  const view = document.defaultView;
  if (view !== null && !(document instanceof view.Document)) {
    const own = Object.getPrototypeOf(view.Document) as typeof Document;
    Object.defineProperty(view.Document, Symbol.hasInstance, { value: (value: unknown) => value instanceof own });
  }
}

if (typeof Selection !== "undefined") {
  const property = Object.getOwnPropertyDescriptor(Selection.prototype, "collapse");
  const collapse = property?.value as Selection["collapse"] | undefined;
  if (collapse !== undefined) {
    Object.defineProperty(Selection.prototype, "collapse", {
      ...property,
      value(this: Selection, node: Node | null, offset = 0) {
        if (node !== null && this.rangeCount > 0 && this.isCollapsed && this.anchorNode === node && this.anchorOffset === offset) return;
        collapse.call(this, node, offset);
      },
    });
  }
}
