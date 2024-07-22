document.addEventListener('DOMContentLoaded', () => {
  let storedCoords = getFromStore("container-coords");
  let translateX = storedCoords[0];
  let translateY = storedCoords[1];
  console.log(storedCoords, translateX, translateY);
  let targetTranslateX = storedCoords[0];
  let targetTranslateY = storedCoords[1];
  const smoothness = 0.9;
  let isMoving = false;
  let movementTimer;
  const grid = document.getElementById('grid');
  const gridPattern = document.getElementById('gridPattern');
  const cellSize = 20;
  const movementTimeout = 150; // Adjust this value to set how long to wait before considering movement stopped

  function updateTranslation() {
    const dx = targetTranslateX - translateX;
    const dy = targetTranslateY - translateY;

    translateX += dx * smoothness;
    translateY += dy * smoothness;

    document.getElementById('cell-container').style.transform = `translate(${translateX}px, ${translateY}px)`;
    // Move the grid
    const gridX = (translateX % cellSize + cellSize) % cellSize - cellSize;
    const gridY = (translateY % cellSize + cellSize) % cellSize - cellSize;
    gridPattern.setAttribute("patternTransform", `translate(${gridX}, ${gridY})`);

    // Check if there's significant movement
    if (Math.abs(dx) > 0.01 || Math.abs(dy) > 0.01) {
      isMoving = true;
      clearTimeout(movementTimer);
      movementTimer = setTimeout(checkMovementStopped, movementTimeout);
    }

    requestAnimationFrame(updateTranslation);
  }

  function handleWheel(event) {
    event.preventDefault();
    targetTranslateX -= event.deltaX;
    targetTranslateY -= event.deltaY;

    isMoving = true;
    clearTimeout(movementTimer);
    movementTimer = setTimeout(checkMovementStopped, movementTimeout);
  }

  function checkMovementStopped() {
    if (isMoving) {
      isMoving = false;
      sendCoordinatesToServer();
    }
  }

  function sendCoordinatesToServer() {
    const roundedX = Math.round(translateX);
    const roundedY = Math.round(translateY);
    send({"dispatch": "store",
          //"from-url": window.location.toString(),
          "container-coords": [roundedX, roundedY]});
  }

  window.addEventListener('wheel', handleWheel, { passive: true });
  updateTranslation();
});

function sendPageExtents(gridSize) {
  const w = Math.floor(window.innerWidth / gridSize);
  const h = Math.floor(window.innerHeight / gridSize);
  send({"dispatch": "store-extents", "extents": [w, h]});
}

let globalGridSize = 20;

// Run the function on page load.
window.addEventListener('load', () => sendPageExtents(globalGridSize));
// Run the function on window resize.
window.addEventListener('resize', () => sendPageExtents(globalGridSize));
