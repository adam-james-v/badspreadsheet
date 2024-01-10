function createGridSquare(elementID) {
  // Get the element by ID
  const element = document.getElementById(elementID);
  if (!element) {
    console.error('Element not found:', elementID);
    return;
  }

  square.addEventListener('click', () => {
    let coords = square.dataset.coords;
    let id = elementID;
    let dispatch = "activate-grid-square";

    // Emit a GET request to the endpoint with the ID
    fetch(`/data`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ dispatch, id, coords })
    })
  });
}
