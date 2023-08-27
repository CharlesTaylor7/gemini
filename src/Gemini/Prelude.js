export const logAnything = (tag) => (something) => () =>
  console.log(tag, something);
