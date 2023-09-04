export const isTouchDevice = window.matchMedia(
  "(pointer: coarse), (hover: none)",
).matches;
