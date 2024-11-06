class PointsEditor extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
    this.points = {};
    this.mode = 'pts';
    this.nextId = 0;
    this.gridSize = 20;
    this.snapToGrid = false;
  }

  render() {
    this.shadowRoot.innerHTML = `
      <style>
        :host {
          width: 100%;
          height: 100%;
          pointer-events: none;
        }
        svg {
          pointer-events: auto;
          box-sizing: border-box;
          width: 100%;
          height: 100%;
          border: 1px solid #000;
        }
        button {
          pointer-events: auto;
        }
        .draggable {
          pointer-events: auto;
          cursor: move;
        }
        path {
          fill: none;
          stroke: black;
          stroke-width: 2;
        }
        .point-menu {
          position: absolute;
          background: white;
          border: 1px solid black;
          padding: 5px;
        }
        .point-id {
          font-size: 10px;
          fill: #888;
        }
      </style>
      <div class="prevent-cursor-move" style="position: absolute; padding: 5px; white-space: nowrap;">
        <button id="toggle-grid" class="prevent-cursor-move">[grid]</button>
      </div>
      <svg class="prevent-cursor-move" xmlns="http://www.w3.org/2000/svg">
        <g id="keep">
          <circle cx="0" cy="0" r="7" fill="green"></circle>
        </g>
      </svg>
    `;
  }

  connectedCallback() {
    this.render();
    this.svg = this.shadowRoot.querySelector('svg');
    this.initEventListeners();
    if (!this.hasAttribute('data-points')) {
      this.setAttribute('data-points', '{}');
    } else {
      this.updatePoints(JSON.parse(this.getAttribute('data-points')));
    }

    this.updateViewBoxSize();
    window.addEventListener('resize', this.updateViewBoxSize.bind(this));

    this.svg.addEventListener('mousedown', this.handleMouseDown.bind(this));
    this.svg.addEventListener('mouseup', this.handleMouseUp.bind(this));
    this.svg.addEventListener('dblclick', this.handleDoubleClick.bind(this));

    this.svg.addEventListener('mousedown', () => {
      this.classList.add('prevent-cursor-move');
    });

    this.svg.addEventListener('mouseup', () => {
      this.classList.remove('prevent-cursor-move');
    });
  }

  disconnectedCallback() {
    window.removeEventListener('resize', this.updateViewBoxSize.bind(this));
  }

  initEventListeners() {
    this.shadowRoot.getElementById('toggle-grid').addEventListener('click', () => {
      this.snapToGrid = !this.snapToGrid;
      this.draw();
    });
  }

  updateViewBoxSize() {
    const rect = this.svg.getBoundingClientRect();
    this.svg.setAttribute('viewBox', `0 0 ${rect.width} ${rect.height}`);
    this.currentViewBox = { x: 0, y: 0, width: rect.width, height: rect.height };
  }

  snapToGridPoint(x, y) {
    if (!this.snapToGrid) return [x, y];
    return [
      Math.round(x / this.gridSize) * this.gridSize,
      Math.round(y / this.gridSize) * this.gridSize
    ];
  }

  handleMouseDown(event) {
    send({"dispatch": "store", "active-element": Number(this.id)});
    event.preventDefault();
  }

  handleMouseUp(event) {
    send({"dispatch": "store", "active-element": null});
  }

  handleDoubleClick(event) {
    const svgRect = this.svg.getBoundingClientRect();
    let x = event.clientX - svgRect.left;
    let y = event.clientY - svgRect.top;
    [x, y] = this.snapToGridPoint(x, y);
    this.addPoint(x, y);
  }

  addPoint(x, y) {
    const newPointId = this.nextId++;
    [x, y] = this.snapToGridPoint(x, y);
    this.points[newPointId] = {
      pos: [x, y],
      constraint: 'free'
    };
    this.sendPointsToServer();
    this.draw();
  }

  removePoint(id) {
    delete this.points[id];
    this.sendPointsToServer();
    this.draw();
  }

  updatePoints(pointsData) {
    this.points = pointsData;
    this.nextId = Math.max(...Object.keys(this.points).map(Number)) + 1;
    this.draw();
  }

  draw() {
    Array.from(this.svg.childNodes).forEach(child => {
      if (child.id !== 'keep') {
        this.svg.removeChild(child);
      }
    });
    const pointCols = {free: "green",
                       horizontal: "blue",
                       vertical: "blue",
                       fixed: "grey"}

    const pathData = Object.entries(this.points).map(([id, point]) => point.pos.join(',')).join(' ');

    Object.entries(this.points).forEach(([id, point]) => {
      const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
      const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
      circle.setAttribute('cx', point.pos[0]);
      circle.setAttribute('cy', point.pos[1]);
      circle.setAttribute('opacity', 0.4);
      circle.setAttribute('stroke', "black");
      circle.setAttribute('r', 4);
      circle.setAttribute('fill', pointCols[point.constraint]);
      circle.setAttribute('cursor', 'move');
      circle.classList.add('draggable');
      circle.classList.add('prevent-cursor-move');
      circle.onmousedown = (e) => this.dragStart(e, id);
      circle.onclick = (e) => this.showPointMenu(e, id);

      const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
      text.setAttribute('x', point.pos[0] + 10);
      text.setAttribute('y', point.pos[1] - 10);
      text.textContent = id;
      text.classList.add('point-id');

      group.appendChild(circle);
      group.appendChild(text);
      this.svg.appendChild(group);
    });
  }

  dragStart(e, id) {
    e.preventDefault();
    const svgRect = this.svg.getBoundingClientRect();
    const viewBox = this.svg.viewBox.baseVal;
    const scaleX = 1;
    const scaleY = 1;

    const point = this.points[id];

    const doDrag = (e) => {
      let svgX = (e.clientX - svgRect.left) * scaleX + viewBox.x;
      let svgY = (e.clientY - svgRect.top) * scaleY + viewBox.y;

      [svgX, svgY] = this.snapToGridPoint(svgX, svgY);

      switch (point.constraint) {
        case 'horizontal':
          point.pos[0] = svgX;
          break;
        case 'vertical':
          point.pos[1] = svgY;
          break;
        case 'fixed':
          // Do nothing
          break;
        default:
          point.pos = [svgX, svgY];
      }

      this.draw();
      this.sendPointsToServer();
    };

    const stopDrag = () => {
      document.removeEventListener('mousemove', doDrag);
      document.removeEventListener('mouseup', stopDrag);
    };

    document.addEventListener('mousemove', doDrag);
    document.addEventListener('mouseup', stopDrag);
  }

  showPointMenu(e, id) {
    e.preventDefault();
    const existingMenu = this.shadowRoot.querySelector('.point-menu');
    if (existingMenu) existingMenu.remove();

    let pt = this.points[id];
    const menu = document.createElement('div');
    menu.classList.add('point-menu');
    menu.style.left = `${pt.pos[0]}px`;
    menu.style.top = `${pt.pos[1]}px`;

    const options = ['free', 'horizontal', 'vertical', 'fixed'];
    options.forEach(option => {
      const button = document.createElement('button');
      button.textContent = option;
      button.onclick = () => {
        this.points[id].constraint = option;
        menu.remove();
        this.sendPointsToServer();
      };
      menu.appendChild(button);
    });

    const deleteButton = document.createElement('button');
    deleteButton.textContent = 'Delete';
    deleteButton.onclick = () => {
      this.removePoint(id);
      menu.remove();
    };
    menu.appendChild(deleteButton);

    this.shadowRoot.appendChild(menu);

    const closeMenu = (e) => {
      if (!menu.contains(e.target)) {
        menu.remove();
        document.removeEventListener('click', closeMenu);
      }
    };
    setTimeout(() => document.addEventListener('click', closeMenu), 0);
  }

  convertToClojureEDN(pointsData) {
    const entries = Object.entries(pointsData).map(([id, point]) => {
      return `${id} {:pos [${point.pos[0]} ${point.pos[1]}] :constraint :${point.constraint}}`;
    });
    return `{${entries.join(' ')}}`;
  }

  debounce(func, delay) {
    clearTimeout(this.debounceTimer);
    this.debounceTimer = setTimeout(func, delay);
  }

  sendPointsToServer() {
    const id = Number(this.id);
    const clojureEDN = this.convertToClojureEDN(this.points);
    this.debounce(() => send({"dispatch": "code", "id": id, "code": `{:control :points-editor}\n${clojureEDN}`}), 16);
  }
}

window.customElements.define('points-editor', PointsEditor);
