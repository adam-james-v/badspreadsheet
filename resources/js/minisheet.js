class MiniSpreadsheet extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
    this.rows = 5;
    this.cols = 5;
    this.data = [];
    this.activeCell = { row: 0, col: 0 };
    this.isEditing = false;
    this.selection = { start: null, end: null };
  }

  connectedCallback() {
    this.rows = parseInt(this.getAttribute('rows') || '5');
    this.cols = parseInt(this.getAttribute('cols') || '5');
    this.initializeData();
    this.render();
    this.setupEventListeners();
  }

  initializeData() {
    this.data = Array(this.rows).fill().map(() => Array(this.cols).fill(''));
  }

  render() {
    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: inline-block;
          font-family: Arial, sans-serif;
        }
        table {
          border-collapse: collapse;
        }
        td {
          box-sizing: border-box;
          border: 1px solid #ccc;
          padding: 1px;
          min-width: 60px;
          height: 20px;
          background-color: white;
        }
        td.active {
          outline: 2px solid blue;
        }
        td.selected {
          background-color: rgba(0, 0, 255, 0.1);
        }
        input {
          width: 100%;
          border: none;
          outline: none;
          font-family: inherit;
          font-size: inherit;
        }
      </style>
      <table>${this.renderCells()}</table>
    `;
  }

  renderCells() {
    return this.data.map((row, i) => `
      <tr>
        ${row.map((cell, j) => `
          <td
            data-row="${i}"
            data-col="${j}"
            class="${this.getCellClasses(i, j)}"
          >${cell}</td>
        `).join('')}
      </tr>
    `).join('');
  }

  getCellClasses(row, col) {
    const classes = [];
    if (row === this.activeCell.row && col === this.activeCell.col) {
      classes.push('active');
    }
    if (this.isInSelection(row, col)) {
      classes.push('selected');
    }
    return classes.join(' ');
  }

  isInSelection(row, col) {
    if (!this.selection.start || !this.selection.end) return false;
    const minRow = Math.min(this.selection.start.row, this.selection.end.row);
    const maxRow = Math.max(this.selection.start.row, this.selection.end.row);
    const minCol = Math.min(this.selection.start.col, this.selection.end.col);
    const maxCol = Math.max(this.selection.start.col, this.selection.end.col);
    return row >= minRow && row <= maxRow && col >= minCol && col <= maxCol;
  }

  setupEventListeners() {
    this.shadowRoot.addEventListener('click', this.handleClick.bind(this));
    this.shadowRoot.addEventListener('dblclick', this.handleDblClick.bind(this));
    document.addEventListener('keydown', this.handleKeyDown.bind(this));
  }

  handleClick(event) {
    const cell = event.target.closest('td');
    if (!cell) return;
    const row = parseInt(cell.dataset.row);
    const col = parseInt(cell.dataset.col);
    this.setActiveCell(row, col);
    if (event.shiftKey) {
      this.selection.end = { row, col };
    } else {
      this.selection = { start: { row, col }, end: { row, col } };
    }
    this.render();
  }

  handleDblClick(event) {
    const cell = event.target.closest('td');
    if (!cell) return;
    this.startEditing();
  }

  handleKeyDown(event) {
    if (this.isEditing) {
      if (event.key === 'Enter') {
        this.stopEditing();
        event.preventDefault();
      }
      return;
    }

    switch (event.key) {
      case 'ArrowUp':
        this.moveActiveCell(-1, 0);
        break;
      case 'ArrowDown':
        this.moveActiveCell(1, 0);
        break;
      case 'ArrowLeft':
        this.moveActiveCell(0, -1);
        break;
      case 'ArrowRight':
        this.moveActiveCell(0, 1);
        break;
      case 'Enter':
        this.startEditing();
        break;
      // Add more shortcuts here
    }
    event.preventDefault();
  }

  setActiveCell(row, col) {
    this.activeCell = {
      row: Math.max(0, Math.min(row, this.rows - 1)),
      col: Math.max(0, Math.min(col, this.cols - 1))
    };
  }

  moveActiveCell(rowDelta, colDelta) {
    this.setActiveCell(
      this.activeCell.row + rowDelta,
      this.activeCell.col + colDelta
    );
    this.render();
  }

  startEditing() {
    const { row, col } = this.activeCell;
    const cell = this.shadowRoot.querySelector(`td[data-row="${row}"][data-col="${col}"]`);
    const input = document.createElement('input');
    input.value = this.data[row][col];
    cell.textContent = '';
    cell.appendChild(input);
    input.focus();
    this.isEditing = true;
  }

  stopEditing() {
    const { row, col } = this.activeCell;
    const cell = this.shadowRoot.querySelector(`td[data-row="${row}"][data-col="${col}"]`);
    const input = cell.querySelector('input');
    if (!input) return;
    this.data[row][col] = input.value;
    this.isEditing = false;
    this.render();
    this.sendUpdateToServer();
  }

  sendUpdateToServer() {
    // Implement server update logic here
    console.log('Sending update to server:', this.data);
  }
}

customElements.define('mini-spreadsheet', MiniSpreadsheet);
