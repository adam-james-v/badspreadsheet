class NumberInput extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  connectedCallback() {
    this.render();
    this.input = this.shadowRoot.querySelector('input');
    this.setupEventListeners();
  }

  render() {
    const value = parseFloat(this.textContent) || 0;
    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: inline-block;
          width: 100%;
          height: 100%;
        }
        input {
          width: 100%;
          height: 100%;
          padding: 0;
          box-sizing: border-box;
        }
      </style>
      <input type="number" value="${value}">
    `;
  }

  setupEventListeners() {
    let input = this.input;
    input.addEventListener('focus', () => {
      send({"dispatch": "store",
            "active-element": Number(this.id)});
    });

    input.addEventListener('blur', () => {
      send({"dispatch": "store",
            "active-element": null});
    });

    input.addEventListener('change', () => {
      this.sendPostRequest(input.value);
    });
    this.addEventListener('keydown', this.handleKeyDown.bind(this), true);
  }

  handleKeyDown(e) {
    let input = this.input;

    if (e.key === 'Escape') {
      input.blur();
      return;
    }

    if (e.key === 'ArrowUp' || e.key === 'ArrowDown') {
      let input = this.shadowRoot.querySelector('input');
    }
  }

  sendPostRequest(value) {
    let dispatch = "code";
    let code = `{:control :number}\n${value}`;
    fetch('/data', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ dispatch, id: Number(this.id), code }),
    });
  }
}

customElements.define('number-input', NumberInput);





class SliderInput extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  connectedCallback() {
    this.render();
    this.input = this.shadowRoot.querySelector('input');
    this.setupEventListeners();
  }

  render() {
    const value = parseFloat(this.textContent) || 0;
    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: inline-block;
          width: 100%;
          height: 100%;
        }
        input {
          width: 100%;
          height: 100%;
          padding: 0;
          box-sizing: border-box;
        }
      </style>
      <input type="range" value="${value}">
    `;
  }

  setupEventListeners() {
    this.input.addEventListener('mousedown', () => {
      send({"dispatch": "store", "active-element": Number(this.id)});
    });

    this.input.addEventListener('mouseup', () => {
      this.sendPostRequest(this.input.value);
    });

    this.input.addEventListener('blur', () => {
      console.log("slider blur");
      send({"dispatch": "store", "active-element": null});
    });

    this.input.addEventListener('input', () => {
      this.value = this.input.value;
      this.debounce(() => this.sendPostRequest(this.value), 16);
    });
  }

  debounce(func, delay) {
    clearTimeout(this.debounceTimer);
    this.debounceTimer = setTimeout(func, delay);
  }

  sendPostRequest(value) {
    let dispatch = "code";
    let code = `{:control :slider}\n${value}`;
    fetch('/data', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ dispatch, id: Number(this.id), code }),
    });
  }
}

customElements.define('slider-input', SliderInput);
