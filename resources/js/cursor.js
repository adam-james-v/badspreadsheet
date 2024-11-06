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
    window.addEventListener('mouseup', this.handleMouseUp.bind(this));
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
    if ( elementIsActive() || shouldPreventMove(e) ) { return };
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




function reinitializeX3DElement(x3dElement) {
    const parent = x3dElement.parentNode;
    const placeholder = document.createElement('div');

    // Temporarily replace the x3d element with a placeholder
    parent.replaceChild(placeholder, x3dElement);

    // Re-insert the x3d element, forcing it to reinitialize
    setTimeout(() => {
        parent.replaceChild(x3dElement, placeholder);
    }, 10);
}



let x3dState = {};

// Function to save the current X3D state
function saveX3DState(x3dElement) {
  if (x3dElement) {
    const viewpoint = x3dElement.querySelector('viewpoint');
    if (viewpoint) {
      x3dState.position = viewpoint.getAttribute('position');
      x3dState.orientation = viewpoint.getAttribute('orientation');
      x3dState.fieldOfView = viewpoint.getAttribute('fieldOfView');
    }
  }
}

// Function to restore the saved X3D state
function restoreX3DState(x3dElement) {
  if (x3dElement) {
    const viewpoint = x3dElement.querySelector('viewpoint');
    if (viewpoint) {
      if (x3dState.position) {
        viewpoint.setAttribute('position', x3dState.position);
      }
      if (x3dState.orientation) {
        viewpoint.setAttribute('orientation', x3dState.orientation);
      }
      if (x3dState.fieldOfView) {
        viewpoint.setAttribute('fieldOfView', x3dState.fieldOfView);
      }
    }
  }
}

// Automatically save X3D state before HTMX request
document.body.addEventListener('htmx:oobBeforeSwap', function(evt) {
  const targetElement = evt.detail.target;
  const x3dElement = targetElement.querySelector('x3d');
  if (x3dElement) {
    saveX3DState(x3dElement);
  }
});

// Restore X3D state after the fragment is swapped in
document.body.addEventListener('htmx:oobAfterSwap', function(evt) {
  const x3dElement = evt.target.querySelector('x3d');
  if (x3dElement) {
    //x3dom.reload();
    initx3d(x3dElement);
    restoreX3DState(x3dElement);
  }
});



var initx3d = function (el)
{
  var i,
      j;  // counters

  // Search all X3D elements in the page
  var x3ds_unfiltered = [el];
  var x3ds = [];

  // check if element already has been processed
  for ( i = 0; i < x3ds_unfiltered.length; i++ )
  {
    if ( x3ds_unfiltered[ i ].hasRuntime === undefined )
    {x3ds.push( x3ds_unfiltered[ i ] );}
  }

  // Components and params
  var params;
  var settings = new x3dom.Properties();  // stores the stuff in <param>
  var validParams = array_to_object( [
    "showLog",
    "showStat",
    "showProgress",
    "PrimitiveQuality",
    "components",
    "loadpath",
    "disableDoubleClick",
    "backend",
    "altImg",
    "runtimeEnabled",
    "disableKeys",
    "showTouchpoints",
    "disableTouch",
    "maxActiveDownloads",
    "useGeoCache",
    "baseURL"
  ] );

  var showLoggingConsole = false;

  // for each X3D element
  for ( i = 0; i < x3ds.length; i++ )
  {
    // default parameters
    settings.setProperty( "showLog", x3ds[ i ].getAttribute( "showLog" ) || "false" );
    settings.setProperty( "showStat", x3ds[ i ].getAttribute( "showStat" ) || "false" );
    settings.setProperty( "showProgress", x3ds[ i ].getAttribute( "showProgress" ) || "true" );
    settings.setProperty( "PrimitiveQuality", x3ds[ i ].getAttribute( "PrimitiveQuality" ) || "High" );
    settings.setProperty( "useGeoCache", x3ds[ i ].getAttribute( "useGeoCache" ) || "true" );
    settings.setProperty( "baseURL", x3ds[ i ].getAttribute( "baseURL" ) || "" );

    // for each param element inside the X3D element
    // add settings to properties object
    params = x3ds[ i ].getElementsByTagName( "PARAM" );
    for ( j = 0; j < params.length; j++ )
    {
      if ( params[ j ].getAttribute( "name" ) in validParams )
      {
        settings.setProperty( params[ j ].getAttribute( "name" ), params[ j ].getAttribute( "value" ) );
      }
      else
      {
        // x3dom.debug.logError("Unknown parameter: " + params[j].getAttribute('name'));
      }
    }

    // enable log
    if ( settings.getProperty( "showLog" ) === "true" )
    {
      showLoggingConsole = true;
    }
  }

  if ( showLoggingConsole == true )
  {
    x3dom.debug.activate( true );
  }
  else
  {
    x3dom.debug.activate( false );
  }

  // Convert the collection into a simple array (is this necessary?)
  x3ds = x3ds.map( function ( n )
                   {
                     n.hasRuntime = true;
                     return n;
                   } );

  if ( x3dom.about !== undefined )
  {
    x3dom.debug.logInfo( "X3DOM " + x3dom.about.version + ", " +
                         "Build: " + x3dom.about.build + ", " +
                         "Revison: <a href='https://github.com/x3dom/x3dom/tree/" + x3dom.about.revision + "'>"
                         + x3dom.about.revision + "</a>, " +
                         "Date: " + x3dom.about.date );
  }

  x3dom.debug.logInfo( "Found " + x3ds.length + " X3D and nodes..." );

  // Create a HTML canvas for every X3D scene and wrap it with
  // an X3D canvas and load the content
  var x3d_element,
      x3dcanvas,
      altDiv,
      altP,
      aLnk,
      altImg,
      t0,
      t1;

  for ( i = 0; i < x3ds.length; i++ )
  {
    x3d_element = x3ds[ i ];

    x3dcanvas = new x3dom.X3DCanvas( x3d_element, x3dom.canvases.length );

    x3dom.canvases.push( x3dcanvas );

    if ( x3dcanvas.gl === null )
    {
      altDiv = document.createElement( "div" );
      altDiv.setAttribute( "class", "x3dom-nox3d" );
      altDiv.setAttribute( "id", "x3dom-nox3d" );

      altP = document.createElement( "p" );
      altP.appendChild( document.createTextNode( "WebGL is not yet supported in your browser. " ) );
      aLnk = document.createElement( "a" );
      aLnk.setAttribute( "href", "http://www.x3dom.org/?page_id=9" );
      aLnk.appendChild( document.createTextNode( "Follow link for a list of supported browsers... " ) );

      altDiv.appendChild( altP );
      altDiv.appendChild( aLnk );

      x3dcanvas.x3dElem.appendChild( altDiv );

      // remove the stats div (it's not added when WebGL doesn't work)
      if ( x3dcanvas.stateViewer )
      {
        x3d_element.removeChild( x3dcanvas.stateViewer.viewer );
      }

      continue;
    }

    t0 = Date.now();

    x3ds[ i ].runtime = new x3dom.Runtime( x3ds[ i ], x3dcanvas );
    x3ds[ i ].runtime.initialize( x3ds[ i ], x3dcanvas );

    if ( x3dom.runtime.ready )
    {
      x3ds[ i ].runtime.ready = x3dom.runtime.ready;
    }

    // no backend found method system wide call
    if ( x3dcanvas.backend == "" )
    {
      x3dom.runtime.noBackendFound();
    }

    x3dcanvas.load( x3ds[ i ], i, settings );

    // show or hide statistics based on param/x3d attribute settings
    if ( settings.getProperty( "showStat" ) === "true" )
    {
      x3ds[ i ].runtime.statistics( true );
    }
    else
    {
      x3ds[ i ].runtime.statistics( false );
    }

    if ( settings.getProperty( "showProgress" ) === "true" )
    {
      if ( settings.getProperty( "showProgress" ) === "bar" )
      {
        x3dcanvas.progressDiv.setAttribute( "class", "x3dom-progress bar" );
      }
      x3ds[ i ].runtime.processIndicator( true );
    }
    else
    {
      x3ds[ i ].runtime.processIndicator( false );
    }

    t1 = Date.now() - t0;
    x3dom.debug.logInfo( "Time for setup and init of GL element no. " + i + ": " + t1 + " ms." );
  }


  var ready = ( function ( eventType )
                {
                  var evt = null;

                  if ( document.createEvent )
                  {
                    evt = document.createEvent( "Events" );
                    evt.initEvent( eventType, true, true );
                    document.dispatchEvent( evt );
                  }
                  else if ( document.createEventObject )
                  {
                    evt = document.createEventObject();
                    // http://stackoverflow.com/questions/1874866/how-to-fire-onload-event-on-document-in-ie
                    document.body.fireEvent( "on" + eventType, evt );
                  }
                } )( "load" );
};
