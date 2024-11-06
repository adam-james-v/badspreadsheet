// Define the web component
class SDFView extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({mode: 'open'});

    // Instance variables
    this.canvas = null;
    this.gl = null;
    this.ext = null;
    this.animation = null;
    this.shaderProgram = null;
    this.vertexBuffer = null;
    this.mouseDown = false;
    this.ctrlDown = false;
    this.lastMouseX = null;
    this.lastMouseY = null;
    this.rotationAngleZ = 0.0;
    this.rotationAngleScreenX = 0.0;
    this.translationX = 0.0;
    this.translationY = 0.0;
    this.zoom = 1.0;

    // Bind methods to instance
    this.handleKeyDown = this.handleKeyDown.bind(this);
    this.handleKeyUp = this.handleKeyUp.bind(this);
    this.handleWebGLContextLost = this.handleWebGLContextLost.bind(this);
    this.handleWheelEvent = this.handleWheelEvent.bind(this);
    this.handleMouseDownEvent = this.handleMouseDownEvent.bind(this);
    this.handleMouseUpEvent = this.handleMouseUpEvent.bind(this);
    this.handleWheelZoom = this.handleWheelZoom.bind(this);
    this.handleMouseMoveEvent = this.handleMouseMoveEvent.bind(this);
    this.draw = this.draw.bind(this);

    // Define quad vertices
    this.quadVertices = new Float32Array([
      -1.0,  1.0,
      -1.0, -1.0,
       1.0,  1.0,
       1.0,  1.0,
      -1.0, -1.0,
       1.0, -1.0
    ]);
  }

  // Shader code parts
  get fragCodeA() {
    return `#version 300 es

precision highp float;

uniform vec2 iResolution;
uniform float u_rotationAngleZ;
uniform float u_rotationAngleScreenX;
uniform float u_translationX;
uniform float u_translationY;
uniform float u_zoom;
out vec4 outputColor;

const int MAX_STEPS = 100;

vec3 lightDir = normalize(vec3(-0.3, -1.0, -0.4));

float sdBox( vec3 p, vec3 b ) {
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

float sdSphere(vec3 p, float r) {
  return length(p) - r;
}

vec2 opRevolve( vec3 p ) {
  return vec2( length(p.xz), p.y );
}

float sdPlane(vec3 p, vec3 n, float h) {
    // n must be normalized
    return dot(p, n) - h;
}

float sdWedge(vec3 p, float deg) {
    float theta = atan(p.z, p.x); // angle in radians
    if (theta < 0.0) theta += 2.0 * 3.14159265; // map theta to [0, 2PI]

    float rad = radians(deg); // convert deg to radians
    float r = 1000.0;
    vec3 p1 = vec3(r*cos(rad), 0.0, r*sin(rad));
    vec3 n1 = vec3(r*-sin(rad), 0.0, r*cos(rad));
    vec3 p2 = vec3(r, 0.0, 0.0);
    vec3 n2 = vec3(0.0, 0.0, -1.0);

    float d1 = sdPlane(p, n1, dot(n1, p1));
    float d2 = sdPlane(p, n2, dot(n2, p2));
    float dcap = max(-p.y, p.y - 1000.0); // assuming unit height cylinder

    if (theta > rad) {
        return length(max(vec2(d1, d2), 0.0)) - length(min(vec2(d1, d2), 0.0));
    }
    return dcap;
}

//vec2 opExtrudeAlong( vec3 p ) {
//  return vec2( length(p.xz), p.y );/
//}

float opExtrude( vec3 p, float sdf, float h ) {
  vec2 w = vec2( sdf, abs(p.z) - h);
  return min(max(w.x,w.y), 0.0) + length(max(w,0.0));
}

// https://iquilezles.org/articles/distfunctions2d/
// create 2D shape fns that take a 3D pt so that they can be viewed as-is

vec3 opSlice( vec2 p, float h ) {
  return vec3( p, h );
}

//vec3 opSlice( vec3 p, float h ) {
//  return vec3( p.xy, h );
//}

float sdCircle( vec2 p, float r ) {
  return length(p.xy) - r;
}

float sdCircle( vec3 p, float r ) {
  return opExtrude(p, sdCircle(p.xy, r), 0.001);
}

float sdPolygon( vec2 p, vec2 v[200], int num ) {
    float d = dot(p-v[0],p-v[0]);
    float s = 1.0;
    for( int i=0, j=num-1; i<num; j=i, i++ ) {
        // distance
        vec2 e = v[j] - v[i];
        vec2 w =    p - v[i];
        vec2 b = w - e*clamp( dot(w,e)/dot(e,e), 0.0, 1.0 );
        d = min( d, dot(b,b) );
        // winding number from http://geomalgorithms.com/a03-_inclusion.html
        bvec3 cond = bvec3( p.y>=v[i].y,
                            p.y <v[j].y,
                            e.x*w.y>e.y*w.x );
        if( all(cond) || all(not(cond)) ) s=-s;
    }
    return s*sqrt(d);
}

float sdPolygon( vec3 p, vec2 v[200], int num ) { return opExtrude(p, sdPolygon(p.xy, v, num), 1.0); }

// WIP: can I 'extrude' by just subtracting the dist of a 2D sdf?
float sdLine( in vec3 p, in vec3 a, in vec3 b, float r ) {
    vec3 pa = p-a, ba = b-a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h ) - r;
}

float sdLine( in vec3 p, in vec2 aa, in vec2 bb, float r ) {
    vec3 a = vec3(aa.x, aa.y ,0.0);
    vec3 b = vec3(bb.x, bb.y ,0.0);
    vec3 pa = p-a, ba = b-a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h ) - r;
}

vec3 opTranslate( vec3 p, vec3 d) { return(p - d); }
vec2 opTranslate( vec2 p, vec3 d) { return(p - d.xy); }
vec2 opTranslate( vec2 p, vec2 d) { return(p - d); }

vec3 opRotateX( vec3 p, float theta) {
    theta = radians(theta);
    // Rotation about X-axis
    mat3 Rx = mat3(
        1.0, 0.0, 0.0,
        0.0, cos(theta), -sin(theta),
        0.0, sin(theta),  cos(theta)
    );
    return Rx * p;
}

vec3 opRotateY( vec3 p, float theta) {
    theta = radians(theta);
    // Rotation about Y-axis
    mat3 Ry = mat3(
        cos(theta), 0.0, sin(theta),
        0.0, 1.0, 0.0,
        -sin(theta), 0.0, cos(theta)
    );
    return Ry * p;
}

vec3 opRotateZ( vec3 p, float theta) {
    theta = radians(theta);
    // Rotation about Z-axis
    mat3 Rz = mat3(
        cos(theta), -sin(theta), 0.0,
        sin(theta), cos(theta), 0.0,
        0.0, 0.0, 1.0
    );
    return Rz * p;
}

vec3 opRotate( vec3 p, vec3 rs) {
    float thetaX = rs.x;
    float thetaY = rs.y;
    float thetaZ = rs.z;

    p = opRotateZ(p, thetaZ);
    p = opRotateY(p, thetaY);
    p = opRotateX(p, thetaX);
    return p;
}
vec2 opRotate( vec2 p, vec3 rs) {
  vec3 pp = opRotateZ( vec3(p, 0.0), rs.z);
  return pp.xy;
}
vec2 opRotate( vec2 p, float theta) {
  vec3 pp = opRotateZ( vec3(p, 0.0), theta);
  return pp.xy;
}

float opOnion( in float d, in float t ) {
  return abs(d)-t;
}

vec3 opRepetition( in vec3 p, in vec3 s ) {
  vec3 q = p - s*round(p/s);
  return q;
}

float opUnion( float d1, float d2 ) { return min(d1,d2); }

float opDifference( float d1, float d2 ) { return max(-d1,d2); }

float opIntersection( float d1, float d2 ) { return max(d1,d2); }

float opSmoothUnion( float d1, float d2, float k ) {
    float h = clamp( 0.5 + 0.5*(d2-d1)/k, 0.0, 1.0 );
    return mix( d2, d1, h ) - k*h*(1.0-h); }

float opSmoothDifference( float d1, float d2, float k ) {
    float h = clamp( 0.5 - 0.5*(d2+d1)/k, 0.0, 1.0 );
    return mix( d2, -d1, h ) + k*h*(1.0-h); }

float opSmoothIntersection( float d1, float d2, float k ) {
    float h = clamp( 0.5 - 0.5*(d2-d1)/k, 0.0, 1.0 );
    return mix( d2, d1, h ) + k*h*(1.0-h); }

// utils

float dot2( in vec2 v ) { return dot(v,v); }
float dot2( in vec3 v ) { return dot(v,v); }
float ndot( in vec2 a, in vec2 b ) { return a.x*b.x - a.y*b.y; }

mat3 rotationMatrix(vec3 axis, float angle) {
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;

    return mat3(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,
                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,
                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c);
}

`;
  }

  get fragCodeB() {
    return `
vec3 normal(vec3 p) {
    vec2 e = vec2(0.0001, 0.0);
    return normalize(vec3(mySdf(p + e.xyy) - mySdf(p - e.xyy),
                          mySdf(p + e.yxy) - mySdf(p - e.yxy),
                          mySdf(p + e.yyx) - mySdf(p - e.yyx)));
}

struct OrthoCamera {
    vec3 position;
    vec3 forward;
    vec3 up;
    vec3 rightv;
    float left;
    float right;
    float bottom;
    float top;
};

OrthoCamera createOrthoCamera(vec3 pos, vec3 lookAt, float width, float height) {
    OrthoCamera cam;
    cam.position = pos;
    cam.forward = normalize(lookAt - pos);
    cam.up = vec3(0, 0, 1);  // assuming Z is up
    cam.rightv = normalize(cross(cam.up, cam.forward));

    // how to avoid gimbal lock and other flipping problems?
    cam.position = rotationMatrix(cam.rightv, clamp(u_rotationAngleScreenX, -87.0, 38.0) * -0.025) * pos;
    cam.forward = normalize(lookAt - cam.position);
    cam.up = vec3(0, 0, 1);  // assuming Z is up
    cam.rightv = normalize(cross(cam.up, cam.forward));

    cam.up = cross(cam.forward, cam.rightv);  // Re-orthogonalize up vector to ensure it's perpendicular

    cam.left = -width / 2.0;
    cam.right = width / 2.0;
    cam.bottom = -height / 2.0;
    cam.top = height / 2.0;

    return cam;
}

struct Ray {
    vec3 origin;
    vec3 direction;
};

const float panCoefficient = 0.4;
Ray getRay(OrthoCamera cam, vec2 uv) {
    // Use the adjusted UV and the camera's parameters to calculate the ray's origin.
    vec2 adjustedUV = vec2((uv.x - u_translationX * panCoefficient),
                            (uv.y + u_translationY * panCoefficient));
    vec3 rayOrigin = cam.position +
                     adjustedUV.x * (cam.right - cam.left) * cam.rightv +
                     adjustedUV.y * (cam.top - cam.bottom) * cam.up;

    return Ray(rayOrigin, cam.forward);
}


const float max_t = 1000.0;
const float min_d = 0.0001;

vec4 raymarch_simple_normal_colors(Ray ray) {
    float t = 0.0;
    for (int i = 0; i < MAX_STEPS; i++) {
        vec3 p = ray.origin + ray.direction * t;
        float d = mySdf(p);
        if (d < min_d) {
            vec3 n = normal(p);
            vec3 col = n*0.5+0.5;
            float gradLen = length(vec3(d - mySdf(p - ray.direction * 0.001)));
            float alpha = 1.0 - smoothstep(0.0, 0.2, gradLen);
            return vec4(col * alpha, alpha);
        }
        t += d;
        if (t > max_t) break;
    }
    return vec4(0.0);
}

// basic raymarcher that renders the SDF's edges
vec4 raymarch_edges(Ray ray) {
    vec4 col = vec4(0.0); // default color
    float t = 0.0;
    for(int i = 0; i < MAX_STEPS; i++) {
        vec3 p = ray.origin + ray.direction * t;
        float d = mySdf(p);

        if (d < min_d) { // we hit the surface
            vec3 n = normal(p);

            // Sample normals in neighboring points around p
            vec3 n1 = normal(p + 0.0075 * n);
            vec3 n2 = normal(p - 0.0075 * n);

            // Compare the normals using dot product
            float dotProd = dot(n1, n2);

            if(dotProd < 0.1) {  // threshold should be adjusted based on the specifics of the SDF
                col = vec4(1.0, 1.0, 1.0, 1.0); // edge color
            }

            break;
        }
        t += d;
        if(t > max_t) break;
    }
    return vec4(col);
}

// Helper function to convert HSV to RGB
vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}


vec4 raymarch_zColor(Ray ray) {
    vec4 col = vec4(0.0); // default transparent color

    float t = 0.0;
    for(int i = 0; i < MAX_STEPS; i++) {
        vec3 p = ray.origin + ray.direction * t;

        float d = mySdf(p);

        if (d < 0.00001) {
            vec3 n = normal(p);

            // Check if the normal is approximately Z-facing
            if(abs(n.z) > 0.95) { // 0.95 is a threshold, you can adjust it
                // Use the z value of the point to derive a color, for example:
                float hue = mod(p.z * 0.125, 360.0); // you can adjust scaling and modulation
                col = vec4(hsv2rgb(vec3(hue, 1.0, 1.0)), 1.0); // Assuming you have a function to convert HSV to RGB
            } else {
                col = vec4(0.0, 0.0, 0.0, 0.0); // Some other color for non Z-facing surfaces
            }

            return col;
        }

        t += d;
        if(t > max_t) break;
    }

    return col;
}

// Main function
void main() {
    vec2 uv = (gl_FragCoord.xy - iResolution * 0.5) / min(iResolution.y, iResolution.x);

    // Define and construct your camera
    vec3 camPos = vec3(50.0, 50.0, 50.0);

    camPos = opRotate(camPos, vec3(0.0, 0.0, -u_rotationAngleZ));
    vec3 camLookAt = vec3(0.0, 0.0, 0.0);
    float width = 8.0/u_zoom;
    OrthoCamera cam = createOrthoCamera(camPos, camLookAt, width, width);

    // Get ray for current pixel
    Ray ray = getRay(cam, uv);

    // Raymarch and fetch color
    vec4 col = vec4(0.0);
    if (normalCols == true) col += raymarch_simple_normal_colors(ray);
    if (contourCols == true) col += raymarch_zColor(ray);
    if (outlines == true) col += raymarch_edges(ray);

    outputColor = col;
}


`;
  }

  static get observedAttributes() {
    return ['sdf-src'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (name === 'sdf-src' && this.gl) {
      this.initialize();
    }
  }

  connectedCallback() {
    // Create and style canvas
    const canvas = document.createElement('canvas');
    canvas.width = 400;
    canvas.height = 400;
    canvas.style.width = '100%';
    canvas.style.height = '100%';

    // Add canvas to shadow DOM
    this.shadowRoot.appendChild(canvas);
    this.canvas = canvas;

    // Initialize WebGL
    this.initContext();
    this.initialize();

    // Add event listeners
    window.addEventListener('keydown', this.handleKeyDown);
    window.addEventListener('keyup', this.handleKeyUp);
    this.canvas.addEventListener('webglcontextlost', this.handleWebGLContextLost);
    this.canvas.addEventListener('webglcontextrestored', this.initContext.bind(this));
    this.canvas.addEventListener('wheel', this.handleWheelEvent);
    this.canvas.addEventListener('mousedown', this.handleMouseDownEvent);
    this.canvas.addEventListener('mouseup', this.handleMouseUpEvent);
    this.canvas.addEventListener('wheel', this.handleWheelZoom);
    this.canvas.addEventListener('mousemove', this.handleMouseMoveEvent);

    // Start rendering
    this.draw();
  }

  disconnectedCallback() {
    // Clean up WebGL resources
    if (this.gl) {
      this.gl.deleteProgram(this.shaderProgram);
      this.gl.deleteBuffer(this.vertexBuffer);
    }

    // Remove event listeners
    window.removeEventListener('keydown', this.handleKeyDown);
    window.removeEventListener('keyup', this.handleKeyUp);
    this.canvas.removeEventListener('webglcontextlost', this.handleWebGLContextLost);
    this.canvas.removeEventListener('webglcontextrestored', this.initContext);
    this.canvas.removeEventListener('wheel', this.handleWheelEvent);
    this.canvas.removeEventListener('mousedown', this.handleMouseDownEvent);
    this.canvas.removeEventListener('mouseup', this.handleMouseUpEvent);
    this.canvas.removeEventListener('wheel', this.handleWheelZoom);
    this.canvas.removeEventListener('mousemove', this.handleMouseMoveEvent);

    cancelAnimationFrame(this.animation);
  }

  initContext() {
    this.gl = this.canvas.getContext('webgl2', {
      preserveDrawingBuffer: true
    });

    if (!this.gl) {
      console.error('WebGL 2 is not available in your browser.');
      return;
    }

    this.ext = this.gl.getExtension('WEBGL_lose_context');
  }

  createShader(type, source) {
    const shader = this.gl.createShader(type);
    this.gl.shaderSource(shader, source);
    this.gl.compileShader(shader);

    if (!this.gl.getShaderParameter(shader, this.gl.COMPILE_STATUS)) {
      console.error('Shader compilation error:', this.gl.getShaderInfoLog(shader));
      this.gl.deleteShader(shader);
      return null;
    }

    return shader;
  }

  initialize() {
    const gl = this.gl;

    // Clean up existing program
    if (this.shaderProgram) {
      gl.deleteProgram(this.shaderProgram);
    }

    // Create shader program
    this.shaderProgram = gl.createProgram();

    // Create/update vertex buffer
    if (!this.vertexBuffer) {
      this.vertexBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, this.quadVertices, gl.STATIC_DRAW);
    }

    // Create shaders
    const vertexShaderSource = `#version 300 es
      in vec2 coordinates;
      uniform float aspectRatio;
      void main() {
        gl_Position = vec4(coordinates.x, coordinates.y * aspectRatio, 0.0, 1.0);
      }`;

    const vertexShader = this.createShader(gl.VERTEX_SHADER, vertexShaderSource);

    // Get SDF source from attribute
    const sdfCode = this.getAttribute('sdf-src') || '';
    const fragCode = `${this.fragCodeA}${sdfCode}${this.fragCodeB}`;
    const fragmentShader = this.createShader(gl.FRAGMENT_SHADER, fragCode);

    // Link program
    gl.attachShader(this.shaderProgram, vertexShader);
    gl.attachShader(this.shaderProgram, fragmentShader);
    gl.linkProgram(this.shaderProgram);

    if (!gl.getProgramParameter(this.shaderProgram, gl.LINK_STATUS)) {
      console.error('Shader program linking error:', gl.getProgramInfoLog(this.shaderProgram));
      return;
    }

    gl.deleteShader(vertexShader);
    gl.deleteShader(fragmentShader);

    gl.useProgram(this.shaderProgram);
  }

  setUniforms() {
    const gl = this.gl;

    gl.uniform2f(
      gl.getUniformLocation(this.shaderProgram, 'iResolution'),
      this.canvas.width,
      this.canvas.height
    );

    const position = gl.getAttribLocation(this.shaderProgram, 'coordinates');
    gl.enableVertexAttribArray(position);
    gl.vertexAttribPointer(position, 2, gl.FLOAT, false, 0, 0);

    gl.uniform1f(
      gl.getUniformLocation(this.shaderProgram, 'u_rotationAngleZ'),
      this.rotationAngleZ
    );

    gl.uniform1f(
      gl.getUniformLocation(this.shaderProgram, 'u_rotationAngleScreenX'),
      this.rotationAngleScreenX
    );

    gl.uniform1f(
      gl.getUniformLocation(this.shaderProgram, 'u_translationX'),
      this.translationX
    );

    gl.uniform1f(
      gl.getUniformLocation(this.shaderProgram, 'u_translationY'),
      this.translationY
    );

    gl.uniform1f(
      gl.getUniformLocation(this.shaderProgram, 'u_zoom'),
      this.zoom
    );

    gl.uniform1f(
      gl.getUniformLocation(this.shaderProgram, 'aspectRatio'),
      this.canvas.width / this.canvas.height
    );
  }

  draw() {
    const gl = this.gl;

    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    gl.clearColor(0.0, 0.0, 0.0, 0.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    gl.useProgram(this.shaderProgram);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);

    this.setUniforms();

    gl.drawArrays(gl.TRIANGLES, 0, 6);

    this.animation = requestAnimationFrame(this.draw);
  }

  // Event handlers
  handleKeyDown(event) {
    if (event.key === 'Shift') {
      this.ctrlDown = true;
    }
  }

  handleKeyUp(event) {
    if (event.key === 'Shift') {
      this.ctrlDown = false;
    }
  }

  handleWebGLContextLost(event) {
    event.preventDefault();
  }

  handleWheelEvent(event) {
    event.preventDefault();
  }

  handleMouseDownEvent(e) {
    this.mouseDown = true;
    this.lastMouseX = e.clientX;
    this.lastMouseY = e.clientY;
  }

  handleMouseUpEvent() {
    this.mouseDown = false;
  }

  handleWheelZoom(e) {
    this.zoom += e.deltaY * 0.01;
  }

  handleMouseMoveEvent(e) {
    if (!this.mouseDown) return;

    const deltaX = e.clientX - this.lastMouseX;
    const deltaY = e.clientY - this.lastMouseY;

    if (this.ctrlDown) {
      this.rotationAngleZ += deltaX * 0.5;
      this.rotationAngleScreenX += deltaY * 0.5;
    } else {
      this.translationX += deltaX * 0.005;
      this.translationY += deltaY * 0.005;
    }

    this.lastMouseX = e.clientX;
    this.lastMouseY = e.clientY;
  }
}

// Register the web component
customElements.define('sdf-view', SDFView);
