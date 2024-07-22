let cellContainer = document.getElementById('cell-container');

function getGridSize() {
  const cursorElement = document.getElementById('cursor');
  return Number(cursorElement.getAttribute("grid-size"));
}

function clickGridLocation(e) {
  const gridSize = getGridSize();
  const rect = cellContainer.getBoundingClientRect();
  const x = e.clientX - rect.left; // x position within the element.
  const y = e.clientY - rect.top;  // y position within the element.
  let cameraLoc = getCameraLocation();
  const gridX = Math.floor(x / gridSize) + cameraLoc[0];
  const gridY = Math.floor(y / gridSize) + cameraLoc[1];
  return { x: gridX, y: gridY };
}

function clickIsInCursor(e) {
  let clickLoc = clickGridLocation(e);
  let cursorLoc = getCursorLocation(e);
  let cursorSize = getCursorSize(e);

  let x1 = cursorLoc[0];
  let x2 = cursorLoc[0] + cursorSize[0];
  let y1 = cursorLoc[1];
  let y2 = cursorLoc[1] + cursorSize[1];
  return ( clickLoc.x >= x1 && clickLoc.x <= x2 ) && ( clickLoc.y >= y1 && clickLoc.y <= y2 );
}

function elementIsActive() {
  return getActiveElementID() !== null
}

function isDragHandle(el) {
  const id = el.getAttribute("id");
  return id !== null && id.includes("drag-handle");
}

function shouldPreventMove(e) {
  return ( e.target.classList.contains("cm-content") );
  //return ( clickIsInCursor(e) && elementIsActive() || e.target.hasAttribute("onclick") ) && !isDragHandle(e.target);
}

let dragHandleActive = false;

function initMouseEventsListener() {
  let gridSize = getGridSize();
  let lastGridX = -1;
  let lastGridY = -1;
  let mouseDown = false;
  let dragging = false;
  let startLocX = -1;
  let startLocY = -1;

  document.addEventListener('mousedown', (e) => {

    if (shouldPreventMove(e)) { return };

    const rect = cellContainer.getBoundingClientRect();
    const x = e.clientX - rect.left; // x position within the element.
    const y = e.clientY - rect.top;  // y position within the element.

    let cameraLoc = getCameraLocation();
    let loc = dragHandleActive
        ? subtractVectors(getCursorLocation(), cameraLoc)
        : [-1, -1];

    startLocX = dragHandleActive
      ? loc[0]
      : Math.floor(x / gridSize) + cameraLoc[0];

    startLocY = dragHandleActive
      ? loc[1]
      : Math.floor(y / gridSize) + cameraLoc[1];

    lastGridX = startLocX;
    lastGridY = startLocY;
    mouseDown = true;
    dragging = isDragHandle(e.target);
  });

  document.addEventListener('mouseup', (e) => {
    mouseDown = false;
    dragging = false;
    dragHandleActive = false;
    // Ignore clicks on buttons and other specified elements
    if (shouldPreventMove(e)) { return }
  });

  document.addEventListener('click', (e) => {
    // Ignore clicks on buttons and other specified elements
    if (shouldPreventMove(e)) { return }

    if (!dragging) {
      // Handle click event here, as there was no mouse movement
      sendCursorData([startLocX, startLocY], [0, 0]);
    }
  });

  document.addEventListener('mousemove', (e) => {
    if (!mouseDown || ( !dragging && shouldPreventMove(e) )) return; // Do nothing if the mouse is not pressed down

    const rect = cellContainer.getBoundingClientRect();
    const x = e.clientX - rect.left; // x position within the element.
    const y = e.clientY - rect.top;  // y position within the element.
    let cameraLoc = getCameraLocation();
    const gridX = Math.floor(x / gridSize) + cameraLoc[0];
    const gridY = Math.floor(y / gridSize) + cameraLoc[1];
    let startSize = getCursorSize();
    let startSizeX = startSize[0];
    let startSizeY = startSize[1];

    if (dragging &&
        (gridX !== lastGridX || gridY !== lastGridY)) {
      lastGridX = gridX;
      lastGridY = gridY;
      let sizeX = (gridX - startLocX + 1);
      let sizeY = (gridY - startLocY + 1);
      let location = dragHandleActive ? [(gridX - startSizeX + 1), (gridY - startSizeY + 1)] : [startLocX, startLocY];
      let size = dragHandleActive ? [0, 0] : [sizeX, sizeY];
      // Send data to the server in the 'dragging' state
      sendCursorData(location, size);
    }
    if (!dragging &&
        (gridX !== lastGridX || gridY !== lastGridY)) {
      dragging = false; // Mouse is moving while pressed down, indicating a drag
      lastGridX = gridX;
      lastGridY = gridY;
      let sizeX = (gridX - startLocX + 1);
      let sizeY = (gridY - startLocY + 1);
      let location = [startLocX, startLocY];
      let size = [sizeX, sizeY];
      // Send data to the server in the 'dragging' state
      sendCursorData(location, size);
    }
  });

  document.addEventListener('dblclick', (e) => {
    // Ignore clicks on buttons and other specified elements
    if (shouldPreventMove(e)) {
      return;
    }

    let cameraLoc = getCameraLocation();
    const rect = cellContainer.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;
    const gridX = Math.floor(x / gridSize) + cameraLoc[0];
    const gridY = Math.floor(y / gridSize) + cameraLoc[1];

    // Reset size to 7x3 on double-click and send data to the server
    sendCursorData([gridX, gridY], [7, 3]);
  });

  function sendCursorData(location, size) {
    let dispatch = "mouse-event";
    let dragging = dragHandleActive;
    fetch(`/data`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ dispatch, location, size, dragging })
    });
  }
}

// window.initMouseEventsListener = (gridSize) => {
//   initMouseEventsListener(gridSize);
// }
