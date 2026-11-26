// What the inputs read of the plain JSON that their `bound` holds, as core's
// client reads it: an object's own value under a key, whether two values are
// equal, and the strings in an array.

// An object's own value under a key, which an id such as "constructor" never
// reads off the prototype.
export function own<T>(record: Readonly<Record<string, T>>, key: string): T | undefined {
  return Object.hasOwn(record, key) ? record[key] : undefined;
}

function isObject(x: unknown): x is Readonly<Record<string, unknown>> {
  return typeof x === "object" && x !== null && !Array.isArray(x);
}

// Whether two values are equal as JSON: the same scalar, or arrays or objects
// that hold equal values.
export function jsonEqual(a: unknown, b: unknown): boolean {
  if (a === b) return true;
  if (Array.isArray(a)) {
    return Array.isArray(b) && a.length === b.length && a.every((x, i) => jsonEqual(x, b[i]));
  }
  if (isObject(a) && isObject(b)) {
    const keys = Object.keys(a);
    return keys.length === Object.keys(b).length && keys.every((key) => Object.hasOwn(b, key) && jsonEqual(a[key], b[key]));
  }
  return false;
}

// The strings in an array, such as the picks of an input of several, or none
// where the value is no array.
export function strings(value: unknown): readonly string[] {
  return Array.isArray(value) ? value.filter((x): x is string => typeof x === "string") : [];
}
