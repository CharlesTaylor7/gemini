export const loadDomInfoF = () => ({ 
  ringRadius: ringRadius(),  
  leftRingCenter: ringCenter(".left.gemini-ring")
  centerRingCenter: ringCenter(".center.gemini-ring")
  rightRingCenter: ringCenter(".right.gemini-ring")
})

function ringCenter(selector) {
  let elem = document.querySelector(selector)
  let rect = elem.getBoundingClientRect()
  let width = rect.width;
  let left = rect.left;
  let top = rect.top;
  let radius = width / 2.0;
  return { x: left + radius, y: top + radius }
}

function getDiameter(selector) {
  let elem = document.querySelector(selector)
  let rect = elem.getBoundingClientRect()
  return rect.width
}

function ringRadius() {
  let ring = getDiameter(".gemini-ring")
  let disk = getDiameter(".disk")
  return (ring - disk) / 2.0;
}
