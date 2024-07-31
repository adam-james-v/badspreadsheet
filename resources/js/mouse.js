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
  return id !== null && id.includes("dragd-handle");
}

function isDescendantOfClass(element, className) {
  // Check if the element itself has the class
  if (element.classList.contains(className)) {
    return true;
  }

  // Use closest() to find the nearest ancestor with the class
  const ancestor = element.closest('.' + className);

  // If an ancestor is found, return true; otherwise, return false
  return !!ancestor;
}

function shouldPreventMove(e) {
  return ( e.target.tagName.toLowerCase() === "input" ||
           e.target.tagName.toLowerCase() === "button" ||
           isDescendantOfClass( e.target, "cm-editor") ||
           isDescendantOfClass( e.target, "waypoint") ||
           isDescendantOfClass( e.target, "prevent-cursor-move") ||
           isDescendantOfClass( e.target, "resizable") );
  //return ( e.target.classList.contains("cm-content") );
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

    if (shouldPreventMove(e)) {
      return
    };

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

document.addEventListener('DOMContentLoaded', () => {
  initializeResizableElements();
});

let GRID_SIZE = getGridSize(); // Size of grid squares in pixels
const HANDLE_VISIBILITY_THRESHOLD = 500;

function snapToGrid(value) {
  return Math.round(value / GRID_SIZE) * GRID_SIZE;
}

function initializeResizableElements() {
  const container = document.getElementById('cell-container')
  container.addEventListener('mousedown', handleMouseDown);
}

function handleMouseDown(e) {
  const isResize = e.target.classList.contains('resize-handle') || e.target.classList.contains('inner-handle');
  //const isMove = e.altKey && e.target.classList.contains('drag-handle');
  const isMove = ( e.altKey || isDescendantOfClass( e.target, "drag-handle") ) && isDescendantOfClass( e.target, "resizable");

  if (!isResize && !isMove) return;


  const handle = e.target;
  const element = handle.closest('.resizable');
  if (!element) return;

  e.preventDefault();
  const startX = e.clientX;
  const startY = e.clientY;
  const startWidth = element.offsetWidth;
  const startHeight = element.offsetHeight;
  const startLeft = element.offsetLeft;
  const startTop = element.offsetTop;

  element.style.overflow = 'hidden';

  function handleMove(e) {
    const dx = e.clientX - startX;
    const dy = e.clientY - startY;

    const newLeft = snapToGrid(startLeft + dx);
    const newTop = snapToGrid(startTop + dy);

    element.style.left = `${newLeft}px`;
    element.style.top = `${newTop}px`;
  }

  function handleResize(e) {
    let newWidth, newHeight, newLeft, newTop;

    if (handle.classList.contains('tl') ||
        handle.classList.contains('tr') ||
        handle.classList.contains('bl') ||
        handle.classList.contains('br')) {
      // Corner handles - resize both width and height
      newWidth  = snapToGrid(startWidth  + (handle.classList.contains('tl') || handle.classList.contains('bl') ? startX - e.clientX : e.clientX - startX));
      newHeight = snapToGrid(startHeight + (handle.classList.contains('tl') || handle.classList.contains('tr') ? startY - e.clientY : e.clientY - startY));

      if (handle.classList.contains('tl') || handle.classList.contains('tr')) {
        newTop = snapToGrid(startTop - (newHeight - startHeight));
      }
      if (handle.classList.contains('tl') || handle.classList.contains('bl')) {
        newLeft = snapToGrid(startLeft - (newWidth - startWidth));
      }
    } else if (handle.classList.contains('tm') || handle.classList.contains('bm')) {
      // Vertical edge handles - resize height only
      newHeight = snapToGrid(startHeight + (handle.classList.contains('tm') ? startY - e.clientY : e.clientY - startY));
      if (handle.classList.contains('tm')) {
        newTop = snapToGrid(startTop - (newHeight - startHeight));
      }
    } else {
      // Horizontal edge handles - resize width only
      newWidth = snapToGrid(startWidth + (handle.classList.contains('ml') ? startX - e.clientX : e.clientX - startX));
      if (handle.classList.contains('ml')) {
        newLeft = snapToGrid(startLeft - (newWidth - startWidth));
      }
    }

    // Apply new dimensions and position
    if (newWidth !== undefined) element.style.width = `${newWidth}px`;
    if (newHeight !== undefined) element.style.height = `${newHeight}px`;
    if (newLeft !== undefined) element.style.left = `${newLeft}px`;
    if (newTop !== undefined) element.style.top = `${newTop}px`;

  }

  function handleMouseMove(e) {
    if (isMove) {
      handleMove(e);
    } else {
      handleResize(e);
    }
  }

  function stopAction() {
    element.style.overflow = 'visible';
    document.removeEventListener('mousemove', handleMouseMove);
    document.removeEventListener('mouseup', stopAction);
    sendResizeDataToBackend(element.id,
                            element.offsetWidth / GRID_SIZE,
                            element.offsetHeight / GRID_SIZE,
                            element.offsetLeft / GRID_SIZE,
                            element.offsetTop / GRID_SIZE);
  }

  document.addEventListener('mousemove', handleMouseMove);
  document.addEventListener('mouseup', stopAction);
}

function sendResizeDataToBackend(elementId, width, height, x, y) {
  send({"dispatch": "adjust-cell",
        "id": elementId,
        "x": x,
        "y": y,
        "w": width,
        "h": height});
}

function onNewElementsAdded() {
  // If you need to do any specific initialization for new elements, do it here
  console.log('New elements added, ready for resizing');
}
