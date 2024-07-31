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
      send({"dispatch": "store",
            "active-element": Number(this.id)});
      isDrawing = true;
      ctx.beginPath();
      ctx.moveTo(e.offsetX, e.offsetY);
      lastX = e.offsetX;
      lastY = e.offsetY;
      // Store the initial point and call send
      this.points.push({x: lastX, y: lastY});
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
      send({"dispatch": "store",
            "active-element": null});
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
          "id": Number(id),
          "code": `{:control :drawing-canvas}\n${pointsString}` });
  }
}

customElements.define('drawing-canvas', DrawingCanvas);
