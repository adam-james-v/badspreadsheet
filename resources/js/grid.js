let storedCoords = getFromStore("container-coords");
let translateX = storedCoords[0];
let translateY = storedCoords[1];
let targetTranslateX = storedCoords[0];
let targetTranslateY = storedCoords[1];
const smoothness = 0.9;
let isMoving = false;
let movementTimer;
const grid = document.getElementById('grid');
const gridPattern = document.getElementById('gridPattern');
const cellSize = 20;
const movementTimeout = 150; // Adjust this value to set how long to wait before considering movement stopped
const columnIndicatorsContainer = document.getElementById('column-indicators');
const rowIndicatorsContainer = document.getElementById('row-indicators');

// Variables for indicators
const columnIndicatorPool = [];
const rowIndicatorPool = [];
let visibleStartX = null;
let visibleEndX = null;
let visibleStartY = null;
let visibleEndY = null;

function updateColumnIndicators(force) {
  const viewportWidth = window.innerWidth;
  const startX = Math.floor(-translateX / cellSize) * cellSize;
  const endX = Math.ceil((viewportWidth - translateX) / cellSize) * cellSize;

  if (startX !== visibleStartX || endX !== visibleEndX || force === true) {
    updateVisibleIndicators(columnIndicatorPool, columnIndicatorsContainer, startX, endX, 'column');
    visibleStartX = startX;
    visibleEndX = endX;
  }

  for (let i = 0; i < columnIndicatorPool.length; i++) {
    const x = (visibleStartX + i * cellSize);
    columnIndicatorPool[i].style.transform = `rotate(180deg) translateX(${0 - (x + translateX )}px)`;
  }
}

function updateRowIndicators(force) {
  const viewportHeight = window.innerHeight;
  const startY = Math.floor(-translateY / cellSize) * cellSize;
  const endY = Math.ceil((viewportHeight - translateY) / cellSize) * cellSize;

  if (startY !== visibleStartY || endY !== visibleEndY || force === true) {
    updateVisibleIndicators(rowIndicatorPool, rowIndicatorsContainer, startY, endY, 'row');
    visibleStartY = startY;
    visibleEndY = endY;
  }

  for (let i = 0; i < rowIndicatorPool.length; i++) {
    const y = (visibleStartY + i * cellSize);
    rowIndicatorPool[i].style.transform = `translateY(${y + translateY}px)`;
  }
}

function updateVisibleIndicators(pool, container, start, end, type) {
  const requiredIndicators = Math.ceil((end - start) / cellSize) + 1;

  while (pool.length < requiredIndicators) {
    const indicator = document.createElement('div');
    indicator.className = `${type}-indicator`;
    container.appendChild(indicator);
    pool.push(indicator);
  }

  while (pool.length > requiredIndicators) {
    const indicator = pool.pop();
    container.removeChild(indicator);
  }
  let idx = 0;
  if (type === 'row') { idx = 1};
  const cursorPrimary = getCursorPrimary();
  const cursorSecondary = getCursorSecondary();
  const highlightStart = Math.min(cursorPrimary[idx], cursorSecondary[idx]);
  const highlightEnd = Math.max(cursorPrimary[idx], cursorSecondary[idx]);
  const highlightedValues = range(highlightStart, highlightEnd + 1);

  for (let i = 0; i < pool.length; i++) {
    const value = start + i * cellSize;
    const adjustedValue = Math.floor(value / cellSize);
    let spacer = "";
    if ( adjustedValue > -1 ) {
      pool[i].style.paddingBottom = `-10px`;
    }
    pool[i].style.backgroundColor = `#E6E6FA`;
    if ( highlightedValues.includes(adjustedValue) ) {
      pool[i].style.backgroundColor = `rgba(109, 164, 101, 0.25)`;
    }
    pool[i].textContent = `${spacer}${adjustedValue}`;
  }
}

document.addEventListener('cursorchange', (e) => {
  updateColumnIndicators(true);
  updateRowIndicators(true);
})

document.addEventListener('DOMContentLoaded', () => {
  let isCtrlKeyPressed = getFromStore("pin-movement") || false;

  function updateTranslation() {
    if (true) {
      const dx = targetTranslateX - translateX;
      const dy = targetTranslateY - translateY;
      translateX += dx * smoothness;
      translateY += dy * smoothness;
      document.getElementById('cell-container').style.transform = `translate(${translateX}px, ${translateY}px)`;
      // Move the grid
      const gridX = (translateX % cellSize + cellSize) % cellSize - cellSize;
      const gridY = (translateY % cellSize + cellSize) % cellSize - cellSize;
      gridPattern.setAttribute("patternTransform", `translate(${gridX}, ${gridY})`);
      // Update indicators
      updateColumnIndicators();
      updateRowIndicators();
      // Check if there's significant movement
      if (Math.abs(dx) > 0.01 || Math.abs(dy) > 0.01) {
        isMoving = true;
        clearTimeout(movementTimer);
        movementTimer = setTimeout(checkMovementStopped, movementTimeout);
      }
    }
    requestAnimationFrame(updateTranslation);
  }

  function handleWheel(event) {
    if (!getFromStore("pin-movement")) {
      // event.preventDefault();
      targetTranslateX -= event.deltaX;
      targetTranslateY -= event.deltaY;
      isMoving = true;
      clearTimeout(movementTimer);
      movementTimer = setTimeout(checkMovementStopped, movementTimeout);
    }
  }

  function checkMovementStopped() {
    if (isMoving) {
      isMoving = false;
      sendCoordinatesToServer(translateX, translateY);
    }
  }

  // Add event listeners for keydown and keyup to track Ctrl key state
  window.addEventListener('keydown', (event) => {
    if (event.key === 'Control') {
      isCtrlKeyPressed = !isCtrlKeyPressed;
    }
    send({"dispatch": "store",
          "pin-movement": isCtrlKeyPressed});
  });

  // window.addEventListener('keyup', (event) => {
  //   if (event.key === 'Control') {
  //     isCtrlKeyPressed = false;
  //   }
  // });

  window.addEventListener('wheel', handleWheel, { passive: true });
  updateTranslation();
});

function sendCoordinatesToServer(x, y) {
  let roundedX = Math.round(x);
  let roundedY = Math.round(y);
  send({"dispatch": "store",
        //"from-url": window.location.toString(),
        "container-coords": [roundedX, roundedY]});
}

function setContainerPosition(x, y) {
  sendCoordinatesToServer(x, y);
  targetTranslateX = x;
  targetTranslateY = y;
}

function centerPosition(x, y) {
  let nX = ( 0 - x ) +  window.innerWidth / 2;
  let nY = ( 0 - y ) + window.innerHeight / 2;
  sendCoordinatesToServer(nX, nY);
  targetTranslateX = nX;
  targetTranslateY = nY;
}

function cornerPosition(x, y) {
  let nX = ( 0 - x ) +  60;
  let nY = ( 0 - y ) + 60;
  sendCoordinatesToServer(nX, nY);
  targetTranslateX = nX;
  targetTranslateY = nY;
}

function sendPageExtents(gridSize) {
  const w = Math.floor(window.innerWidth / gridSize);
  const h = Math.floor(window.innerHeight / gridSize);
  send({"dispatch": "store",
        "extents": [w, h]});
}

let globalGridSize = 20;

// Run the function on page load.
window.addEventListener('load', () => sendPageExtents(globalGridSize));
// Run the function on window resize.
window.addEventListener('resize', () => sendPageExtents(globalGridSize));
