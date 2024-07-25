import { default_extensions, complete_keymap } from '@nextjournal/clojure-mode';
import { EditorView, drawSelection, keymap } from  '@codemirror/view';
import { EditorState } from  '@codemirror/state';
import { syntaxHighlighting, defaultHighlightStyle, foldGutter } from '@codemirror/language';

let theme = EditorView.theme({
  "&.cm-editor" : {"background": "lavender",
                   "border-radius": "2px",
                   "border": "1px solid #C0B6D0"},
  ".cm-content": {whitespace: "pre-wrap",
                  passing: "10px 0",
                  flex: "1 1 0"},

  "&.cm-focused": {outline: "0 !important",
                   "background": "lavender"},
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

function sendEditorCode(view) {
  // Fetch the code from the editor
  let code = view.state.doc.toString();

  // Get the ID of the parent element
  let editorElement = view.dom;
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
    { key: "Alt-Enter", run: (view) => { sendEditorCode(view) } },
    ...complete_keymap
  ]),
  ...default_extensions
  //EditorView.updateListener.of(debouncedUpdateListener)
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
    this.mode = 'polyline'; // or 'polyline', 'polygon'
    this.initEventListeners();
    //this.selectedPoints = [];
    //this.selectionBox = null; // Object to store selection box's start and end point
    this.isPanning = false;
    this.startPan = { x: 0, y: 0 };
    console.log("points-editor available");
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



// touch tilt controller
class TouchTiltControl extends HTMLElement {
  constructor() {
    super();
    this.x = 0;
    this.y = 0;
    this.isPressed = false;
    this.lastClickTime = 0;
    this.initialized = false;
    this.initialGamma = 0;
    this.initialBeta = 0;
    this.lastOrientationEventTime = 0;
    this.orientationEventThreshold = 100;
  }

  connectedCallback() {
    this.style.display = 'flex';
    this.style.alignItems = 'center'; // Center content vertically
    this.style.justifyContent = 'center'; // Center content horizontally
    this.style.width = '100%';
    this.style.height = '100%';
    this.style.display = 'block';
    this.style.backgroundColor = 'honeydew'; // Customize as needed

    // Create and append the display element
    this.display = document.createElement('div');
    this.display.style.pointerEvents = 'none';
    this.display.style.userSelect = 'none';
    this.display.textContent = `[${this.x} ${this.y}]`; // Initialize text content
    this.appendChild(this.display);

    // Event listeners
    this.addEventListener('mousedown', this.handleMouseDown.bind(this));
    this.addEventListener('mouseup', this.handleMouseUp.bind(this));
    this.addEventListener('mousemove', this.handleMouseMove.bind(this));
    this.addEventListener('touchstart', this.handleTouchStart.bind(this));
    this.addEventListener('touchend', this.handleTouchEnd.bind(this));
    this.addEventListener('click', this.handleDoubleClick.bind(this));
    this.addEventListener('touchend', this.handleDoubleTap.bind(this));
    //window.addEventListener('deviceorientation', this.handleOrientation.bind(this));

    // Create a button to request permissions
    this.permissionButton = document.createElement('button');
    this.permissionButton.textContent = 'Enable Orientation';
    // DeviceOrientationEvent.requestPermission() MUST fire off of click or touchend! touchstart doesn't work.. Holy shit
    this.permissionButton.addEventListener('touchend', () => {
      this.permissionButton.style.backgroundColor = 'skyblue';
      this.requestDeviceOrientationPermission();
    });

    // Add the button to the component if permission is needed
    if (typeof DeviceOrientationEvent !== 'undefined' && typeof DeviceOrientationEvent.requestPermission === 'function') {
      this.appendChild(this.permissionButton);
    } else {
      // If no permission is needed, directly add the event listener
      window.addEventListener('deviceorientation', this.handleOrientation.bind(this));
    }
  }

  // Ensure to clean up the event listeners on disconnect
  disconnectedCallback() {
    this.removeEventListener('mousedown', this.handleMouseDown);
    this.removeEventListener('mouseup', this.handleMouseUp);
    this.removeEventListener('mousemove', this.handleMouseMove);
    this.removeEventListener('touchstart', this.handleTouchStart);
    this.removeEventListener('touchend', this.handleTouchEnd);
    window.removeEventListener('deviceorientation', this.handleOrientation);
  }

  requestDeviceOrientationPermission() {
    // Check if DeviceOrientationEvent is available
    this.permissionButton.textContent = 'Enable Orientation PENDING!';
    if (typeof DeviceOrientationEvent !== 'undefined' && typeof DeviceOrientationEvent.requestPermission === 'function') {
      DeviceOrientationEvent.requestPermission()
        .then(permissionState => {
          this.permissionButton.textContent = `Permission: ${permissionState}`;
          if (permissionState === 'granted') {
            window.addEventListener('deviceorientation', this.handleOrientation.bind(this));
            this.permissionButton.remove(); // Optionally remove the button after permission is granted
          } else {
            console.error('DeviceOrientation permission not granted');
            this.permissionButton.textContent = 'Enable Orientation ERROR!';
            this.sendValues2();
          }
        })
        .catch(console.error);
    } else {
      // Automatically add event listener if permission is not required (non-iOS 13+ devices)
      window.addEventListener('deviceorientation', this.handleOrientation.bind(this));
      this.permissionButton.remove(); // Optionally remove the button after permission is granted
    }
  }

  handleMouseDown(event) {
    this.isPressed = true;
    this.startX = event.clientX;
    this.startY = event.clientY;
  }

  handleMouseUp() {
    this.isPressed = false;
  }

  handleMouseMove(event) {
    if (!this.isPressed) return;
    this.x += event.clientX - this.startX;
    this.y += event.clientY - this.startY;
    this.startX = event.clientX;
    this.startY = event.clientY;
    this.sendValues();
  }

  handleTouchStart(event) {
    this.isPressed = true;
  }

  handleTouchEnd() {
    this.initialized = false;
    this.isPressed = false;
  }

  // Handle double click
  handleDoubleClick() {
    const currentTime = new Date().getTime();
    if (currentTime - this.lastClickTime < 300) { // 300ms threshold for double-click
      this.resetValues();
    }
    this.lastClickTime = currentTime;
  }

  // Handle double tap - we'll also use 'touchend' event here
  handleDoubleTap(event) {
    // Prevent double firing with click events on mobile
    event.preventDefault();

    const currentTime = new Date().getTime();
    if (currentTime - this.lastClickTime < 300) { // 300ms threshold for double-tap
      this.resetValues();
    }
    this.lastClickTime = currentTime;
  }

  handleOrientation(event) {
    if (!this.isPressed) return;

    if (!this.initialized) {
      this.initialized = true;
      this.initialGamma = event.gamma;
      this.initialBeta = event.beta;
      return
    }

    const currentTime = new Date().getTime();
    if (currentTime - this.lastOrientationEventTime > this.orientationEventThreshold) {
      // Proceed only if the threshold time has passed since the last event
      const gamma = event.gamma; // Left to right
      const beta = event.beta;  // Front to back

      this.x += gamma - initialGamma;
      this.y += beta - initialBeta;
      this.sendValues();

      this.lastOrientationEventTime = currentTime; // Update the time of the last event handled
    }
  }

  resetValues() {
    this.x = 0;
    this.y = 0;
    this.sendValues(); // Update display and optionally send values to backend
  }

  sendValues2() {
    // Update the display element whenever values change
    this.display.textContent = `[${this.x.toFixed(2)} ${this.y.toFixed(2)}]`; // Use toFixed(2) for cleaner display
    const id = this.id;
    send({"dispatch": "asdf",
          "id": id,
          "code": `{:control :touch-tilt-control :x ${this.x} :y ${this.y} }` });
  }

  sendValues() {
    // Update the display element whenever values change
    this.display.textContent = `[${this.x.toFixed(2)} ${this.y.toFixed(2)}]`; // Use toFixed(2) for cleaner display
    const id = this.id;
    send({"dispatch": "code",
          "id": id,
          "code": `{:control :touch-tilt-control :x ${this.x} :y ${this.y} }` });
  }
}

window.customElements.define('touch-tilt-control', TouchTiltControl);




// Drawing canvas webcomponent

class DrawingCanvas extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
    const canvas = document.createElement('canvas');
    canvas.width = 500;
    canvas.height = 500;
    canvas.style.border = '1px solid black';
    this.canvas = canvas;

    const button = document.createElement('button');
    button.textContent = 'Clear Points';

    this.shadowRoot.append(canvas, button);

    const ctx = canvas.getContext('2d');
    this.ctx = ctx;
    let isDrawing = false;
    let lastX = 0;  // Last drawn X position
    let lastY = 0;  // Last drawn Y position
    const minRadius = 15;  // Minimum radius to draw next line segment
    this.points = [];  // Array to store points

    canvas.addEventListener('mousedown', (e) => {
      isDrawing = true;
      ctx.beginPath();
      ctx.moveTo(e.offsetX, e.offsetY);
      lastX = e.offsetX;
      lastY = e.offsetY;
      // Store the initial point and call send
      this.points.push({x: lastX, y: lastY});
      this.send({x: lastX, y: lastY});
    });

    canvas.addEventListener('mousemove', (e) => {
      if (isDrawing) {
        let dx = e.offsetX - lastX;
        let dy = e.offsetY - lastY;
        if (Math.sqrt(dx * dx + dy * dy) >= minRadius) {
          ctx.lineTo(e.offsetX, e.offsetY);
          ctx.stroke();
          ctx.beginPath();
          ctx.moveTo(e.offsetX, e.offsetY);
          lastX = e.offsetX;
          lastY = e.offsetY;
          // Store the point and call send
          this.points.push({x: lastX, y: lastY});
          this.sendValues();
        }
      }
    });

    canvas.addEventListener('mouseup', () => {
      isDrawing = false;
    });

    button.addEventListener('click', () => {
      this.clearPoints();
    });
  }

  connectedCallback() {
    if (!this.hasAttribute('data-points')) {
      this.setAttribute('data-points', '[]');
    } else {
      this.points = JSON.parse(this.getAttribute('data-points'));
      this.drawDataPoints();
    }
    this.style.display = 'flex';
    this.style.alignItems = 'center'; // Center content vertically
    this.style.justifyContent = 'center'; // Center content horizontally
    this.style.width = '100%';
    this.style.height = '100%';
    this.style.display = 'block';
    this.style.backgroundColor = 'honeydew'; // Customize as needed
  }

  drawDataPoints() {
    this.points.forEach((point, index) => {
      if (index === 0) {
        this.ctx.beginPath();
        this.ctx.moveTo(point.x, point.y);
      } else {
        this.ctx.lineTo(point.x, point.y);
        this.ctx.stroke();
      }
    });
    if (this.points.length > 0) {
      this.ctx.beginPath();  // Start a new path for subsequent drawings
      this.ctx.moveTo(this.points[this.points.length - 1].x, this.points[this.points.length - 1].y);
    }
  }

  markPoints(ctx) {
    // Draw blue circles at each stored point
    this.points.forEach(point => {
      ctx.fillStyle = 'blue';
      ctx.beginPath();
      ctx.arc(point.x, point.y, 5, 0, 2 * Math.PI);
      ctx.fill();
    });
  }

  clearPoints() {
    // Draw blue circles at each stored point
    this.points = [];
    this.ctx.clearRect(0,0,this.canvas.width,this.canvas.height);
    this.sendValues();
  }


  send(data) {
    console.log("Sending data to server:", data);
    // Actual implementation of sending data to server should be here
  }

  formatPointsForServer() {
    // Map each point object to a string in the format "[x y]"
    const formattedPoints = this.points.map(point => `[${point.x} ${point.y}]`);
    // Join all the formatted points into a single string
    return `[${formattedPoints.join(' ')}]`;
  }

  sendValues() {
    const pointsString = this.formatPointsForServer();
    const id = this.id;
    send({"dispatch": "code",
          "id": id,
          "code": `{:control :drawing-canvas :pts ${pointsString} }` });
  }
}

window.customElements.define('drawing-canvas', DrawingCanvas);

// attach functions to the window so they can be used globally
window.createEditorInstance = (id) => {
  createEditorInstance(id);
}

window.attachEntityListeners = (id) => {
  attachEntityListeners(id);
}

window.initGamepadListener = () => {
  initGamepadListener();
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

window.toggleDragHandle = () => {
  toggleDragHandle();
}
