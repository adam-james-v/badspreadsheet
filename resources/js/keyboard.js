function initKeyPressListener() {
  let dispatch = "keypress";
  let keys = [];

  document.addEventListener('keydown', (e) => {
    // First check if there's an active element
    if (shouldProcessKeypress(e)) {
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

      // if (e.key === 'ArrowLeft')  { keys = ["left"]; }
      // if (e.key === 'ArrowRight') { keys = ["right"]; }
      // if (e.key === 'ArrowUp')    { keys = ["up"]; }
      // if (e.key === 'ArrowDown')  { keys = ["down"]; }

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
    }
  });
}

function shouldProcessKeypress(event) {
  const activeElement = document.activeElement;

  // Check if the active element is the body or html
  if (activeElement === document.body || activeElement === document.documentElement) {
    return true;
  }

  // Check if the active element is an input-like element
  const inputLikeElements = ['INPUT', 'TEXTAREA', 'SELECT'];
  if (inputLikeElements.includes(activeElement.tagName)) {
    return false;
  }

  // Check for contenteditable elements
  if (activeElement.isContentEditable) {
    return false;
  }

  // Check for custom elements that might be handling their own key events
  if (activeElement.tagName.includes('-')) {
    return false;
  }

  return true;
}
