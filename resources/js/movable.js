function attachDragEvents(triggerElement, elementToMove) {
  let pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;
  let px = 0;
  let py = 0;

  triggerElement.onmousedown = dragMouseDown;

  function dragMouseDown(e) {
    e = e || window.event;
    e.preventDefault();
    // Get the mouse cursor position at startup:
    pos3 = e.clientX;
    pos4 = e.clientY;
    document.onmouseup = closeDragElement;
    // Call a function whenever the cursor moves:
    document.onmousemove = elementDrag;
  }

  function elementDrag(e) {
    e = e || window.event;
    e.preventDefault();
    // Calculate the new cursor position:
    pos1 = pos3 - e.clientX;
    pos2 = pos4 - e.clientY;
    pos3 = e.clientX;
    pos4 = e.clientY;
    // Set the element's new position:
    elementToMove.style.top = (elementToMove.offsetTop - pos2) + "px";
    elementToMove.style.left = (elementToMove.offsetLeft - pos1) + "px";
  }

  function closeDragElement() {
    // Stop moving when mouse button is released:
    document.onmouseup = null;
    document.onmousemove = null;
    // Send data to server
    let id = elementToMove.id;
    let dispatch = "movement";
    let pos = [px, py];
    fetch(`/data`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ dispatch, id, pos })
    })
  }
}

function makeMovable(parentElementId) {
  const parent = document.getElementById(parentElementId);
  if (!parent) {
    console.error('Parent element not found:', parentElementId);
    return;
  }

  // Check if there is a child with the handle class
  const handle = parent.querySelector('.handle');

  if (handle) {
    // If handle exists, only make the handle trigger movement
    attachDragEvents(handle, parent);
  } else {
    // If no handle, make the entire parent element trigger movement
    attachDragEvents(parent, parent);
  }
}

window.makeMovable = (id) => {
  makeMovable(id);
}
