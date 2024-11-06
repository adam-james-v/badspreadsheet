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

// attach functions to the window so they can be used globally
window.createEditorInstance = (id) => {
  createEditorInstance(id);
}

window.attachEntityListeners = (id) => {
  attachEntityListeners(id);
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
