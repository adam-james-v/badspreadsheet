class SpreadsheetCursor extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
    this.primaryPosition = { x: 0, y: 0 };
    this.secondaryPosition = { x: 0, y: 0 };
    this.cellContainer = null;
    this.isDragging = false;
  }

  connectedCallback() {
    this.connectToCellContainer('grid-container');
    this.initPositions();
    this.render();
  }

  render() {
    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: block;
          position: absolute;
          pointer-events: none;
          z-index: 1000;
        }
        #cursor2 {
          position: absolute;
          border: 1px solid rgba(109, 164, 101, 1);
          transform: translate(-1px, -1px);
          background-color: rgba(109, 164, 101, 0.1);
        }
        #cursor2-inner {
          position: absolute;
          border: 1px solid rgba(109, 164, 101, 1);
          transform: translate(-1px, -1px);
          width: 22px;
          height: 22px;
          background-color: rgba(109, 164, 101, 0.1);
        }
      </style>
      <div id="cursor2"></div>
      <div id="cursor2-inner"></div>
    `;
    this.updateCursorStyle();
  }

  connectToCellContainer(containerId) {
    this.cellContainer = document.getElementById(containerId);
    if (!this.cellContainer) {
      console.error(`Cell container with id '${containerId}' not found`);
      return;
    }
    window.addEventListener('keydown', this.handleKeyDown.bind(this));
    this.cellContainer.addEventListener('mousedown', this.handleMouseDown.bind(this));
    window.addEventListener('mousemove', this.handleMouseMove.bind(this));
    this.cellContainer.addEventListener('mouseup', this.handleMouseUp.bind(this));
    this.resizeStartPosition = null;
    this.resizeStartSize = null;
  }

  initPositions() {
    let primary = getCursorPrimary();
    let secondary = getCursorSecondary();
    this.primaryPosition.x = primary[0];
    this.primaryPosition.y = primary[1];
    this.secondaryPosition.x = secondary[0];
    this.secondaryPosition.y = secondary[1];
  }

  handleKeyDown(e) {
    if ( elementIsActive() ) { return };
    const isShiftPressed = e.shiftKey;
    let delta = { x: 0, y: 0 };

    switch (e.key) {
      case 'ArrowUp':
        delta.y = -1;
        break;
      case 'ArrowDown':
        delta.y = 1;
        break;
      case 'ArrowLeft':
        delta.x = -1;
        break;
      case 'ArrowRight':
        delta.x = 1;
        break;
      default:
        return; // Exit if it's not an arrow key
    }

    if (isShiftPressed) {
      this.moveSecondaryPosition(delta);
    } else {
      this.movePrimaryPosition(delta);
    }
    e.preventDefault(); // Prevent scrolling
  }

  movePrimaryPosition(delta) {
    const newPrimary = {
      x: this.primaryPosition.x + delta.x,
      y: this.primaryPosition.y + delta.y
    };
    const currentWidth = Math.abs(this.secondaryPosition.x - this.primaryPosition.x);
    const currentHeight = Math.abs(this.secondaryPosition.y - this.primaryPosition.y);

    const newSecondary = {
      x: newPrimary.x + (this.secondaryPosition.x >= this.primaryPosition.x ? currentWidth : -currentWidth),
      y: newPrimary.y + (this.secondaryPosition.y >= this.primaryPosition.y ? currentHeight : -currentHeight)
    };

    this.updatePositions(newPrimary, newSecondary);
  }

  moveSecondaryPosition(delta) {
    const newSecondary = {
      x: this.secondaryPosition.x + delta.x,
      y: this.secondaryPosition.y + delta.y
    };
    this.updatePositions(this.primaryPosition, newSecondary);
  }

  handleMouseDown(e) {
    if ( elementIsActive() ) { return };
    const clickedPosition = clickGridLocation(e);
    this.primaryPosition = clickedPosition;
    this.secondaryPosition = clickedPosition;
    this.isDragging = true;
    this.updatePositions(this.primaryPosition, this.secondaryPosition);
  }

  handleMouseMove(e) {
    if (this.isDragging) {
      const currentPosition = clickGridLocation(e);
      this.updatePositions(this.primaryPosition, currentPosition);
    }
  }

  handleMouseUp() {
    this.isDragging = false;
  }

  updatePositions(primary, secondary) {
    this.primaryPosition = primary;
    this.secondaryPosition = secondary;
    this.updateCursorStyle();
  }

  updateCursorStyle() {
    const cursor = this.shadowRoot.getElementById('cursor2');
    const inner = this.shadowRoot.getElementById('cursor2-inner');
    const left = Math.min(this.primaryPosition.x, this.secondaryPosition.x);
    const top = Math.min(this.primaryPosition.y, this.secondaryPosition.y);
    const width = Math.abs(this.secondaryPosition.x - this.primaryPosition.x) + 1;
    const height = Math.abs(this.secondaryPosition.y - this.primaryPosition.y) + 1;
    const primaryLeft = this.primaryPosition.x;
    const primaryTop = this.primaryPosition.y;

    cursor.style.left = `${left * 20}px`;
    cursor.style.top = `${top * 20}px`;
    cursor.style.width = `${(width * 20) + 2}px`;
    cursor.style.height = `${(height * 20) + 2}px`;
    inner.style.left = `${primaryLeft * 20}px`;
    inner.style.top = `${primaryTop * 20}px`;

    const cursorElem = document.getElementById('cursor');
    cursorElem.setAttribute("cursor-location", `${this.primaryPosition.x},${this.primaryPosition.y}`);
    cursorElem.setAttribute("cursor-size", `${width},${height}`);
    cursorElem.setAttribute("cursor-primary", `${this.primaryPosition.x},${this.primaryPosition.y}`);
    cursorElem.setAttribute("cursor-secondary", `${this.secondaryPosition.x},${this.secondaryPosition.y}`);

    send({"dispatch": "adjust-cursor",
          "location":  [left, top],
          "size":      [width, height],
          "primary":   [primaryLeft, primaryTop],
          "secondary": [this.secondaryPosition.x, this.secondaryPosition.y]});
    this.dispatchCursorChange();
  }

  dispatchCursorChange() {
    const event = new CustomEvent('cursorchange', {
      detail: {
        primary: this.primaryPosition,
        secondary: this.secondaryPosition
      },
      bubbles: true,
      composed: true
    });
    this.dispatchEvent(event);
  }

}

customElements.define('spreadsheet-cursor', SpreadsheetCursor);
