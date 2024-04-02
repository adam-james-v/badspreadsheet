import { default_extensions, complete_keymap } from '@nextjournal/clojure-mode';
import { EditorView, drawSelection, keymap } from  '@codemirror/view';
import { EditorState } from  '@codemirror/state';
import { syntaxHighlighting, defaultHighlightStyle, foldGutter } from '@codemirror/language';

function send(body) {
  fetch('/data', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(body),
  });
}

let theme = EditorView.theme({
  "&.cm-editor" : {"background": "aliceblue",
                   "border-radius": "7px"},
  ".cm-content": {whitespace: "pre-wrap",
                  passing: "10px 0",
                  flex: "1 1 0"},

  "&.cm-focused": {outline: "0 !important",
                   "background": "aliceblue"},
  ".cm-line": {"padding": "0 4px",
               "line-height": "1.2",
               "font-size": "10.5pt",
               //"font-family": "var(--code-font)",
               "font-family": "'Berkeley Mono', monospace"},
  ".cm-matchingBracket": {"border-bottom": "1px solid var(--teal-color)",
                          "color": "inherit"},
  ".cm-gutters": {background: "transparent",
                  border: "none"},
  ".cm-gutterElement": {"margin-left": "3px"},
  // only show cursor when focused
  ".cm-cursor": {visibility: "hidden"},
  "&.cm-focused .cm-cursor": {visibility: "visible"}
});

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

function handleEditorUpdate(update) {
  if (update.docChanged) {
    // Fetch the code from the editor
    let code = update.state.doc.toString();

    // Get the ID of the parent element
    let editorElement = update.view.dom;
    let id = editorElement.parentElement.id;
    let dispatch = "code";

    // Emit a GET request to the endpoint with the ID
    fetch(`/data`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ dispatch, id, code })
    })
  }
}

const debouncedUpdateListener = debounce(handleEditorUpdate, 300); // Adjust the delay as needed

function unfocusActiveElement() {
  const focusedElement = document.activeElement;
  if (focusedElement) { focusedElement.blur(); }
}

let extensions = [
  theme,
  foldGutter(),
  syntaxHighlighting(defaultHighlightStyle),
  drawSelection(),
  keymap.of([
    { key: "Escape", run: (view) => { view.dom.blur(); return true; } },
    ...complete_keymap
  ]),
  ...default_extensions,
  EditorView.updateListener.of(debouncedUpdateListener)
];

function createEditorInstance(elementID) {
  // Get the element by ID
  const element = document.getElementById(elementID);
  if (!element) {
    console.error('Element not found:', elementID);
    return;
  }

  // Store the original dimensions
  let originalDimensions = {
    width: element.style.width,
    height: element.style.height
  };

  // Use innerHTML of the element as the document for the editor
  let docContent = element.textContent.trim();
  // Clear the original element's content
  element.innerHTML = '';
  // Create a new state for the editor with the document content
  let state = EditorState.create({
    doc: docContent,
    extensions: extensions
  });

  // Create a new editor view
  let editor = new EditorView({
    state: state,
    parent: element,
    extensions: extensions
  });

  // immediately set its width/height style to that of the original element
  editor.dom.style.width = originalDimensions.width;
  editor.dom.style.height = originalDimensions.height;
  // "border-radius": element.style.borderRadius

  editor.dom.addEventListener("keydown", (e) => {
    e.stopPropagation();
    if (e.key === 'Escape') {
      unfocusActiveElement();
    }
  });

    // Event listener for editor focus
  editor.dom.addEventListener('focus', () => {
    // Set editor dimensions to 500px by 500px
    //editor.dom.style.width = '500px';
    //editor.dom.style.height = '500px';
    editor.dom.style.zIndex = '1000';
    // turn on all cell info divs
    const elements = document.querySelectorAll('.id-info');

    for (let i = 0; i < elements.length; i++) {
      let tmp = elements[i].style.display;
      elements[i].setAttribute(`data-tmp`, tmp);
      elements[i].style.display = ''; // Revert to default display value
    }

  }, true); // Use capture to ensure the event is detected early

  // Add an event listener to handle the focusout event (when the editor loses focus)
  editor.dom.addEventListener('focusout', () => {

    // Revert to original dimensions
    //editor.dom.style.width = originalDimensions.width;
    //editor.dom.style.height = originalDimensions.height;
    editor.dom.style.zIndex = '';

    // turn off all cell info divs
    const elements = document.querySelectorAll('.id-info');

    for (let i = 0; i < elements.length; i++) {
      let tmp = elements[i].getAttribute('data-tmp');
      elements[i].removeAttribute('data-tmp');
      elements[i].style.display = tmp;
    }

    // Fetch the code from the editor
    let code = editor.state.doc.toString();
    let id = elementID;
    let dispatch = "code";

    // Emit a GET request to the endpoint with the ID
    fetch(`/data`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ dispatch, id, code })
    })
  });

  return editor; // Return the editor instance in case it needs to be used externally
}

let isHovering = null;

function attachEntityListeners(elementID) {
  const entity = document.getElementById(elementID);

  let id = elementID;
  let dispatch = "make-active";
  entity.addEventListener('mouseenter', () => {
    fetch(`/data`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ dispatch, id })
    })
  });
}

var dragHandleActive = false;

function toggleDragHandle() {
  dragHandleActive = !dragHandleActive;
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

let globalGridSize = 20;
function initMouseEventsListener(gridSize) {
  globalGridSize = gridSize;
  let lastGridX = -1;
  let lastGridY = -1;
  let mouseDown = false;
  let dragging = false;
  let startLocX = -1;
  let startLocY = -1;
  let el = document.getElementById('bg');

  document.addEventListener('mousedown', (e) => {
    // Ignore clicks on buttons and other specified elements
    if (e.target.className.includes('prevent-cursor-move') && !dragHandleActive) {
      return;
    }
    const rect = el.getBoundingClientRect();
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
  });

  document.addEventListener('mouseup', (e) => {
    mouseDown = false;
    dragging = false;
    dragHandleActive = false;
    // Ignore clicks on buttons and other specified elements
    if (e.target.className.includes('prevent-cursor-move')) {
      return;
    }
  });

  document.addEventListener('click', (e) => {
    // Ignore clicks on buttons and other specified elements
    if (e.target.className.includes('prevent-cursor-move')) {
      return;
    }

    if (!dragging) {
      // Handle click event here, as there was no mouse movement
      sendCursorData([startLocX, startLocY], [0, 0]);
    }
  });

  document.addEventListener('mousemove', (e) => {
    if (!mouseDown || e.target.className.includes('prevent-cursor-move')) return; // Do nothing if the mouse is not pressed down

    const rect = el.getBoundingClientRect();
    const x = e.clientX - rect.left; // x position within the element.
    const y = e.clientY - rect.top;  // y position within the element.
    let cameraLoc = getCameraLocation();
    const gridX = Math.floor(x / gridSize) + cameraLoc[0];
    const gridY = Math.floor(y / gridSize) + cameraLoc[1];
    let startSize = getCursorSize();
    let startSizeX = startSize[0];
    let startSizeY = startSize[1];

    if (dragHandleActive &&
        !e.target.className.includes('prevent-cursor-move') &&
        (gridX !== lastGridX || gridY !== lastGridY)) {
      dragging = true; // Mouse is moving while pressed down, indicating a drag
      lastGridX = gridX;
      lastGridY = gridY;
      let sizeX = (gridX - startLocX + 1);
      let sizeY = (gridY - startLocY + 1);
      let location = dragHandleActive ? [(gridX - startSizeX + 1), (gridY - startSizeY + 1)] : [startLocX, startLocY];
      let size = dragHandleActive ? [0, 0] : [sizeX, sizeY];
      // Send data to the server in the 'dragging' state
      sendCursorData(location, size);
    }
    if (!dragHandleActive &&
        !e.target.className.includes('prevent-cursor-move') &&
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
    if (e.target.closest('.prevent-cursor-move')) {
      return;
    }
    let cameraLoc = getCameraLocation();
    const rect = el.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;
    const gridX = Math.floor(x / gridSize) + cameraLoc[0];
    const gridY = Math.floor(y / gridSize) + cameraLoc[1];

    // Reset size to 1x1 on double-click and send data to the server
    sendCursorData([gridX, gridY], [1, 1]);
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


function initKeyPressListener() {
  let dispatch = "keypress";
  let keys = [];

  document.addEventListener('keydown', (e) => {
    keys = [];
    if (e.key === 'Escape') { keys = ["escape"]; }
    if (e.key === 'Enter') { keys = ["enter"]; }
    if (e.shiftKey && (e.key === 'n' || e.key === 'N')) { keys = ["shift", "n"]; }
    if (e.ctrlKey  && (e.key === 'n' || e.key === 'N')) { keys = ["ctrl", "n"]; }
    if (e.ctrlKey  && (e.key === 'd' || e.key === 'D')) { keys = ["ctrl", "d"]; }
    if (e.ctrlKey  && (e.key === 's' || e.key === 'S')) { keys = ["ctrl", "s"]; }
    if (e.ctrlKey  && (e.key === 'c' || e.key === 'C')) { keys = ["ctrl", "c"]; }
    if (e.ctrlKey  && (e.key === 'v' || e.key === 'V')) { keys = ["ctrl", "v"]; }
    if (e.ctrlKey  && (e.key === 'w' || e.key === 'W')) { keys = ["ctrl", "w"]; }
    if (e.ctrlKey && e.shiftKey && (e.key === 'n' || e.key === 'N')) { keys = ["ctrl", "shift", "n"]; }

    if (e.ctrlKey && e.shiftKey && (e.key === 'f' || e.key === 'F')) { keys = ["ctrl", "shift", "f"]; }

    if (e.key === 'ArrowLeft')  { keys = ["left"]; }
    if (e.key === 'ArrowRight') { keys = ["right"]; }
    if (e.key === 'ArrowUp')    { keys = ["up"]; }
    if (e.key === 'ArrowDown')  { keys = ["down"]; }

    if (e.key === 'ArrowLeft' && e.key === 'ArrowRight')  { keys = ["left", "right"]; }

    if (e.shiftKey && e.key === 'ArrowLeft')  { keys = ["shift", "left"]; }
    if (e.shiftKey && e.key === 'ArrowRight') { keys = ["shift", "right"]; }
    if (e.shiftKey && e.key === 'ArrowUp')    { keys = ["shift", "up"]; }
    if (e.shiftKey && e.key === 'ArrowDown')  { keys = ["shift", "down"]; }

    if (e.ctrlKey && e.key === 'ArrowLeft')  { keys = ["ctrl", "left"]; }
    if (e.ctrlKey && e.key === 'ArrowRight') { keys = ["ctrl", "right"]; }
    if (e.ctrlKey && e.key === 'ArrowUp')    { keys = ["ctrl", "up"]; }
    if (e.ctrlKey && e.key === 'ArrowDown')  { keys = ["ctrl", "down"]; }

    if (e.ctrlKey && e.shiftKey && e.key === 'ArrowLeft')  { keys = ["ctrl", "shift", "left"]; }
    if (e.ctrlKey && e.shiftKey && e.key === 'ArrowRight') { keys = ["ctrl", "shift", "right"]; }
    if (e.ctrlKey && e.shiftKey && e.key === 'ArrowUp')    { keys = ["ctrl", "shift", "up"]; }
    if (e.ctrlKey && e.shiftKey && e.key === 'ArrowDown')  { keys = ["ctrl", "shift", "down"]; }

    if (keys.length > 0) {
      fetch(`/data`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ dispatch, keys })
      })
    }
    keys = [];
  });
}

function makeNumberInput(elementID) {
    // Get the element by ID
  const element = document.getElementById(elementID);
  if (!element) {
    console.error('Element not found:', elementID);
    return;
  }

  const value = parseFloat(element.textContent);
  const inputElement = document.createElement('input');

  inputElement.type = 'number';
  inputElement.value = value;
  inputElement.id = elementID; // Carry over the original ID
  inputElement.style.width = '100%';
  inputElement.style.height = '100%';
  inputElement.style.padding = '0';
  inputElement.style.boxSizing = 'border-box';
  element.parentNode.replaceChild(inputElement, element);

  // Event listener for value change
  inputElement.addEventListener('change', () => {
    sendPostRequest(inputElement.id, inputElement.value);
  });

  // Event listener for arrow keys
  inputElement.addEventListener('keydown', (e) => {
    e.stopPropagation();
    if (e.key === 'Escape') {
      unfocusActiveElement();
    }
    if (e.key === 'ArrowUp' || e.key === 'ArrowDown') {
      sendPostRequest(elementID, inputElement.value);
    }
  });

  function sendPostRequest(id, value) {
    let dispatch = "code";
    let code = value;
    fetch('/data', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ dispatch, id, code }),
    });
  }
}

function setElementFocus(elementID) {
  let editorContainer = document.getElementById(elementID);
  if (editorContainer) {
    let editorElement = editorContainer.querySelector('.cm-editor');
    let entity;
    if (editorElement) {
      entity = editorElement.querySelector('.cm-content');
    }
    else {
      entity = editorContainer;
    }
    if (entity) {
      entity.focus();
    }
  }
}

function initGamepadListener() {

  let gamepad;
  function connectGamepad(e) {
    gamepad = e.gamepad;
    console.log('Gamepad connected:', gamepad.id);
    requestAnimationFrame(update);
  }

  function disconnectGamepad(e) {
    if (gamepad && gamepad.index === e.gamepad.index) {
      gamepad = null;
      console.log('Gamepad disconnected');
    }
  }

  window.addEventListener("gamepadconnected", connectGamepad);
  window.addEventListener("gamepaddisconnected", disconnectGamepad);

  let lastZone = -1;
  let buttonStates = {};
  const initialDelay = 150; // ms
  const repeatRate = 75; // ms
  const buttonMap = { 0: "face-down",
                      1: "face-right",
                      2: "face-left",
                      3: "face-up",
                      4: "L1",
                      5: "R1",
                      6: "L2",
                      7: "R2",
                      8: "select",
                      9: "start",
                     10: "L3",
                     11: "R3",
                     12: "d-up",
                     13: "d-down",
                     14: "d-left",
                     15: "d-right"};

  function sendButtonPost(buttons) {
    let dispatch = "gamepad"
    fetch(`/data`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ dispatch, buttons })
    })
  }

  // function handleButtonPress(gp, buttonIndex) {
  //   const isPressed = gp.buttons[buttonIndex].pressed;
  //   if (isPressed) {
  //     if (!buttonStates[buttonIndex] || (buttonStates[buttonIndex].pressed === false)) {
  //       // Button is just pressed
  //       buttonStates[buttonIndex] = {
  //         pressed: true,
  //         lastPressed: Date.now(),
  //         logged: false
  //       };
  //       //sendButtonPost(buttonMap[buttonIndex]);
  //       return true
  //     } else if (buttonStates[buttonIndex].pressed && !buttonStates[buttonIndex].logged) {
  //       // Check for initial delay
  //       if (Date.now() - buttonStates[buttonIndex].lastPressed > initialDelay) {
  //         buttonStates[buttonIndex].logged = true;
  //         buttonStates[buttonIndex].lastPressed = Date.now();
  //         //sendButtonPost(buttonMap[buttonIndex]);
  //         return true
  //       }
  //     } else if (buttonStates[buttonIndex].pressed && buttonStates[buttonIndex].logged) {
  //       // Check for repeat rate
  //       if (Date.now() - buttonStates[buttonIndex].lastPressed > repeatRate) {
  //         buttonStates[buttonIndex].lastPressed = Date.now();
  //         //sendButtonPost(buttonMap[buttonIndex]);
  //         return true
  //       }
  //     }
  //   } else {
  //     // Reset state when button is released
  //     buttonStates[buttonIndex] = { pressed: false, logged: false };
  //     return false;
  //   }
  // }

  function handleButtonPress(gp, buttonIndex) {
    const isPressed = gp.buttons[buttonIndex].pressed;
    const isModifier = ['L1', 'L2', 'R1', 'R2'].includes(buttonMap[buttonIndex]);
    const isDpad = ['d-up', 'd-down', 'd-left', 'd-right'].includes(buttonMap[buttonIndex]);

    if (isPressed) {
      if (!buttonStates[buttonIndex]) {
        buttonStates[buttonIndex] = {
          pressed: true,
          lastPressed: Date.now(),
          logged: false
        };
        if (!isModifier && !isDpad) {
          return null; // Other buttons don't send on initial press
        }
      } else if (isDpad && !buttonStates[buttonIndex].logged) {
        // D-pad handling with initial delay
        if (Date.now() - buttonStates[buttonIndex].lastPressed > initialDelay) {
          buttonStates[buttonIndex].logged = true;
          buttonStates[buttonIndex].lastPressed = Date.now();
          return buttonMap[buttonIndex];
        }
      } else if (isDpad && buttonStates[buttonIndex].logged) {
        // D-pad handling with repeat rate
        if (Date.now() - buttonStates[buttonIndex].lastPressed > repeatRate) {
          buttonStates[buttonIndex].lastPressed = Date.now();
          return buttonMap[buttonIndex];
        }
      }
      // No action for modifiers while pressed
      return null;
    } else {
      if (buttonStates[buttonIndex] && buttonStates[buttonIndex].pressed) {
        buttonStates[buttonIndex] = { pressed: false, logged: false };
        if (isModifier) {
          // Check if any other button was pressed while this modifier was held
          let combinedPress = checkCombinedPress(gp, buttonIndex);
          if (combinedPress) {
            return combinedPress;
          }
        }
        return isDpad ? null : buttonMap[buttonIndex]; // D-pad doesn't send on release
      }
    }
    return null;
  }

  function checkCombinedPress(gp, modifierIndex) {
    for (let i = 0; i < gp.buttons.length; i++) {
      if (i !== modifierIndex && buttonStates[i] && buttonStates[i].pressed) {
        return buttonMap[modifierIndex] + '+' + buttonMap[i];
      }
    }
    return null;
  }




  function update() {
    if (!gamepad) {
      return;
    }
    const gp = navigator.getGamepads()[gamepad.index];
    const xAxis = gp.axes[0];
    const yAxis = gp.axes[1];

    // Deadzone threshold
    const deadzone = 0.15; // Adjust this value as needed

    // Calculate the angle
    let angle = Math.atan2(yAxis, xAxis) * (180 / Math.PI);
    if (angle < 0) {
        angle += 360;
    }

    // Calculate the zone
    const zoneSize = 360 / 16;
    let zone = Math.floor((angle + zoneSize / 2) / zoneSize);
    zone = zone === 16 ? 0 : zone;

    // Store the last zone in a variable outside the update function
    if ((Math.hypot(xAxis, yAxis) > deadzone) && (zone !== lastZone)) {
        console.log(`Left Analog Stick Angle: ${angle.toFixed(2)}°, Zone: ${zone}`);
        lastZone = zone; // Update the last zone
    }
    if ((Math.hypot(xAxis, yAxis) < deadzone) && (lastZone !== -1)) {
      zone = -1;
      console.log(`Left Analog Stick Angle: ${angle.toFixed(2)}°, Zone: ${zone}`);
      lastZone = -1; // Update the last zone to -1 which we count as 'not touched'
    }

    let buttonsPressed = [];
    gp.buttons.forEach((button, index) => {
      let btnPress = handleButtonPress(gp, index);
      if (btnPress) {
        buttonsPressed.push(btnPress);
      }
    });

    if (buttonsPressed.length > 0) {
      sendButtonPost(buttonsPressed);
    }

    requestAnimationFrame(update);
  }
}

document.addEventListener('wheel', function(e) {
  const threshold = 3; // Adjust threshold value as needed
  const direction = {
    horizontal: e.deltaX > threshold ? 'right' : e.deltaX < -threshold ? 'left' : null,
    vertical: e.deltaY > threshold ? 'down' : e.deltaY < -threshold ? 'up' : null,
  };

  if (direction.horizontal !== null || direction.vertical !== null) {
    send({"dispatch": "scroll", "direction": direction});
  }
}, {passive: true}); // Use passive listener for better performance


function sendPageExtents(gridSize) {
  const w = Math.floor(window.innerWidth / gridSize);
  const h = Math.floor(window.innerHeight / gridSize);
  send({"dispatch": "store-extents", "extents": [w, h]});
}


// experiment with a webcomponent
class PointsEditor extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
    const template = document.getElementById('points-editor-template').content.cloneNode(true);
    this.shadowRoot.appendChild(template);

    this.svg = this.shadowRoot.querySelector('svg');
    //this.svg.classList.add('prevent-cursor-move');

    this.points = [];
    this.selectedPoints = [];
    this.mode = 'polyline'; // or 'polyline', 'polygon'
    this.initEventListeners();
    this.selectionBox = null; // Object to store selection box's start and end point
    this.isPanning = false;
    this.startPan = { x: 0, y: 0 };
  }
  connectedCallback() {
    if (!this.hasAttribute('data-points')) {
      this.setAttribute('data-points', '[]');
    } else {
      this.updatePoints(JSON.parse(this.getAttribute('data-points')));
    }

    this.updateViewBoxSize();
    window.addEventListener('resize', this.updateViewBoxSize.bind(this));

    this.svg.addEventListener('mousedown', this.handleMouseDown.bind(this));
    window.addEventListener('mouseup', this.handleMouseUp.bind(this));
    window.addEventListener('mousemove', this.handleMouseMove.bind(this));

    // Mouse enter event listener to add a class
    this.svg.addEventListener('mousedown', () => {
      this.classList.add('prevent-cursor-move');
    });

    // Mouse leave event listener to remove the class
    this.svg.addEventListener('mouseup', () => {
      this.classList.remove('prevent-cursor-move');
    });
  }

  disconnectedCallback() {
    window.removeEventListener('resize', this.updateViewBoxSize.bind(this));
  }

  updateViewBoxSize() {
    // Use getBoundingClientRect() to get the current size of the component
    const rect = this.svg.getBoundingClientRect();
    // Update the viewBox attribute to match the component size
    // You might want to adjust the values slightly if there are borders or paddings
    this.svg.setAttribute('viewBox', `0 0 ${rect.width} ${rect.height}`);
    // Also, update your currentViewBox object if you're using it for panning
    this.currentViewBox = { x: 0, y: 0, width: rect.width, height: rect.height };
  }

  initEventListeners() {
    this.shadowRoot.getElementById('toggle-mode').addEventListener('click', () => {
      this.toggleMode();
    });
    this.shadowRoot.getElementById('add-point').addEventListener('click', () => {
      this.addPoint();
    });
    this.shadowRoot.getElementById('remove-point').addEventListener('click', () => {
      this.removePoint();
    });
  }

  handleMouseDown(event) {
    event.preventDefault();
    if (event.target === this.svg) {
      this.isPanning = true;
      this.startPan.x = event.clientX;
      this.startPan.y = event.clientY;
    }
  }

  handleMouseUp(event) {
    this.isPanning = false;
  }

  handleMouseMove(event) {
    event.preventDefault();
    if (!this.isPanning) return;

    const dx = event.clientX - this.startPan.x;
    const dy = event.clientY - this.startPan.y;

    // Convert dx and dy to SVG units. This conversion depends on the SVG's current view size and the actual SVG size.
    const scaleFactorX = 1; //this.currentViewBox.width / this.svg.getBoundingClientRect().width;
    const scaleFactorY = 1; //this.currentViewBox.height / this.svg.getBoundingClientRect().height;

    this.currentViewBox.x -= dx * scaleFactorX;
    this.currentViewBox.y -= dy * scaleFactorY;

    this.startPan.x = event.clientX;
    this.startPan.y = event.clientY;

    this.updateViewBox();
  }

  updateViewBox() {
    this.svg.setAttribute('viewBox', `${this.currentViewBox.x} ${this.currentViewBox.y} ${this.currentViewBox.width} ${this.currentViewBox.height}`);
  }

  toggleMode() {
    const modes = ['points', 'polyline', 'polygon'];
    this.mode = modes[(modes.indexOf(this.mode) + 1) % modes.length];
    this.shadowRoot.getElementById('toggle-mode').textContent = `Mode (${this.mode})`;
    this.draw();
  }

  addPoint() {
    const newPoint = {x: Math.random() * this.svg.clientWidth, y: Math.random() * this.svg.clientHeight};
    this.points.push(newPoint);
    this.sendPointsToServer();
    this.draw();
  }

  removePoint() {
    this.points.pop();
    this.sendPointsToServer();
    this.draw();
  }

  updatePoints(points) {
    this.points = points;
    this.draw();
  }

  draw() {
    //this.svg.innerHTML = ''; // Clear existing content
    Array.from(this.svg.childNodes).forEach(child => {
      if (child.id !== 'keep') {
        this.svg.removeChild(child);
      }
    });

    const pathData = this.points.map(p => `${p.x},${p.y}`).join(' ');

    if (this.mode === 'polyline' || this.mode === 'polygon') {
      const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
      path.setAttribute('d', `M ${pathData} ${this.mode === 'polygon' ? 'Z' : ''}`);
      this.svg.appendChild(path);
    }
    this.points.forEach(point => {
      const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
      circle.setAttribute('cx', point.x);
      circle.setAttribute('cy', point.y);
      circle.setAttribute('r', 5);
      circle.setAttribute('fill', "blue");
      circle.setAttribute('cursor', 'move');
      circle.classList.add('draggable');
      circle.classList.add('prevent-cursor-move');
      circle.onmousedown = (e) => this.dragStart(e, point);
      this.svg.appendChild(circle);
    });
  }

  dragStart(e, point) {
    // Prevent default to avoid unwanted behaviors like text selection
    e.preventDefault();

    // Get SVG rectangle and viewBox properties
    const svgRect = this.svg.getBoundingClientRect();
    const viewBox = this.svg.viewBox.baseVal;

    // Calculate the scale between the SVG's physical dimensions and its viewBox dimensions
    const scaleX = 1;
    const scaleY = 1;

    const doDrag = (e) => {
      // Convert the mouse coordinates to SVG coordinates
      const svgX = (e.clientX - svgRect.left) * scaleX + viewBox.x;
      const svgY = (e.clientY - svgRect.top) * scaleY + viewBox.y;
      // Update the point's position
      point.x = svgX;
      point.y = svgY;

      this.draw();
      this.sendPointsToServer(); // Assume this method sends the updated points to your server
    };

    const stopDrag = () => {
      document.removeEventListener('mousemove', doDrag);
      document.removeEventListener('mouseup', stopDrag);
    };

    document.addEventListener('mousemove', doDrag);
    document.addEventListener('mouseup', stopDrag);
  }


  formatPointsForServer() {
    // Map each point object to a string in the format "[x y]"
    const formattedPoints = this.points.map(point => `[${point.x} ${point.y}]`);
    // Join all the formatted points into a single string
    return `[${formattedPoints.join(' ')}]`;
  }

  sendPointsToServer() {
    const id = this.id;
    const pointsString = this.formatPointsForServer();
    send({"dispatch": "code", "id": id, "code": pointsString});
    // Implement AJAX/fetch call here
  }
}

window.customElements.define('points-editor', PointsEditor);

























// Run the function on page load.
window.addEventListener('load', () => sendPageExtents(globalGridSize));

// Run the function on window resize.
window.addEventListener('resize', () => sendPageExtents(globalGridSize));

// attach functions to the window so they can be used globally
window.createEditorInstance = (id) => {
  createEditorInstance(id);
}

window.attachEntityListeners = (id) => {
  attachEntityListeners(id);
}

window.initKeyPressListener = () => {
  initKeyPressListener();
}

window.initGamepadListener = () => {
  initGamepadListener();
}

window.initMouseEventsListener = (gridSize) => {
  initMouseEventsListener(gridSize);
}

window.makeNumberInput = (id) => {
  makeNumberInput(id);
}

window.setElementFocus = (id) => {
  setElementFocus(id);
}

window.unfocusActiveElement = () => {
  unfocusActiveElement();
}
window.send = (body) => {
  send(body);
}

window.toggleDragHandle = () => {
  toggleDragHandle();
}
