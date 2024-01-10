import { default_extensions, complete_keymap } from '@nextjournal/clojure-mode';
import { EditorView, drawSelection, keymap } from  '@codemirror/view';
import { EditorState } from  '@codemirror/state';
import { syntaxHighlighting, defaultHighlightStyle, foldGutter } from '@codemirror/language';

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
               "font-family": "Berkeley Mono"},
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

function initMouseEventsListener(gridSize) {
  let lastGridX = -1;
  let lastGridY = -1;
  let mouseDown = false;
  let startLocX = -1;
  let startLocY = -1;
  let el = document.getElementById('bg');

  document.addEventListener('mousedown', (e) => {
    const rect =  el.getBoundingClientRect();
    const x = e.clientX - rect.left; // x position within the element.
    const y = e.clientY - rect.top;  // y position within the element.

    startLocX = Math.floor(x / gridSize);
    startLocY = Math.floor(y / gridSize);
    mouseDown = true;
  });

  document.addEventListener('mouseup', (e) => {
    startLocX = -1;
    startLocY = -1;
    mouseDown = false;
  });

  document.addEventListener('mousemove', (e) => {

    const rect =  el.getBoundingClientRect();
    const x = e.clientX - rect.left; // x position within the element.
    const y = e.clientY - rect.top;  // y position within the element.

    const gridX = Math.floor(x / gridSize);
    const gridY = Math.floor(y / gridSize);

    let dispatch = "mouse-event";
    if (gridX !== lastGridX || gridY !== lastGridY) {
      lastGridX = gridX;
      lastGridY = gridY;
      let location = [gridX, gridY];
      let size = [1, 1];
      if (mouseDown === true) {
        location = [startLocX, startLocY];
        size = [(1 + (gridX - startLocX)), (1 + (gridY - startLocY))];
      }
      fetch(`/data`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ dispatch, location, size })
      })
    }
  })
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
