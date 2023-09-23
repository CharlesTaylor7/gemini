export const loadDomInfoF = () => ({
  ringRadius: ringRadius(),
  leftRingCenter: ringCenter('[data-ring="left"]'),
  centerRingCenter: ringCenter('[data-ring="center"]'),
  rightRingCenter: ringCenter('[data-ring="right"]'),
});

function ringCenter(selector) {
  let elem = document.querySelector(selector);
  let rect = elem.getBoundingClientRect();
  let width = rect.width;
  let left = rect.left;
  let top = rect.top;
  let radius = width / 2.0;
  return { x: left + radius, y: top + radius };
}

function getDiameter(selector) {
  let elem = document.querySelector(selector);
  let rect = elem.getBoundingClientRect();
  return rect.width;
}

function ringRadius() {
  let ring = getDiameter("[data-ring]");
  let disk = getDiameter("[data-disk]");
  return (ring - disk) / 2.0;
}
