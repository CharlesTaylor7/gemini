export const logAnythingF = (tag) => (something) => () =>
  console.log(tag, something);
