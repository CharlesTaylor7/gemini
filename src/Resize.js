export const getBoundingClientRectF = (el) => () => el.getBoundingClientRect();
export const newResizeObserverF = listener => () => new ResizeObserver((entries) => listener(entries)());
export const observeF = (el) => (ob) => () => ob.observe(el);
export const disconnectF = (ob) => () => ob.disconnect();

