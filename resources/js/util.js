function send(body) {
  fetch('/data', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(body),
  });
}

function getGridSize() {
  const cursorElement = document.getElementById('cursor');
  return Number(cursorElement.getAttribute("grid-size"));
}

function getFromStore(k) {
  const cursorElement = document.getElementById('cursor');
  let data = JSON.parse(cursorElement.getAttribute("data-store"));
  return data[k];
}

function getCameraLocation() {
  const cursorElement = document.getElementById('cursor');
  const cameraLocation = cursorElement.getAttribute('camera-location');
  return cameraLocation.split(',').map(Number);
}

function getCursorLocation() {
  const cursorElement = document.getElementById('cursor');
  const cursorLocation = cursorElement.getAttribute('cursor-location');
  return cursorLocation.split(',').map(Number);
}

function getCursorSize() {
  const cursorElement = document.getElementById('cursor');
  const cursorSize = cursorElement.getAttribute('cursor-size');
  return cursorSize.split(',').map(Number);
}

function getActiveElementID() {
  const cursorElement = document.getElementById('cursor');
  return cursorElement.getAttribute('active-element-id');
}

function addVectors(v1, v2) {
  if (v1.length !== v2.length) {
    throw new Error('Vectors must be of the same length');
  }
  return v1.map((val, index) => val + v2[index]);
}

function subtractVectors(v1, v2) {
  if (v1.length !== v2.length) {
    throw new Error('Vectors must be of the same length');
  }
  return v1.map((val, index) => val - v2[index]);
}

function debounce(func, wait) {
  let timeout;

  return function executedFunction(...args) {
    const later = () => {
      clearTimeout(timeout);
      func(...args);
    };

    clearTimeout(timeout);
    timeout = setTimeout(later, wait);
  };
}
