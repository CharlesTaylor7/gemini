export const getBoundingClientRect = (el) => () => el.getBoundingClientRect();
export const newResizeObserver = listener => () => new ResizeObserver((entries) => listener(entries)());
export const observe = (el) => (ob) => () => ob.observe(el);
export const disconnect = (ob) => () => ob.disconnect();

