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

window.initGamepadListener = () => {
  initGamepadListener();
}
