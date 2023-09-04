export const logAnythingF = (tag) => (something) => () =>
  console.log(tag, something);

export const isTouchDevice = window.matchMedia(
  "(pointer: coarse), (hover: none)",
).matches;
