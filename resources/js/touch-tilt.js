
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
