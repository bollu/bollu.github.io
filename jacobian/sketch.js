const W = 720;
const H = 400;
const R = 50;

let pX = 200;
let pY = 0;

// f(x, y) = (u: x (x^2+y^2)^{-1/2}, v: y(x^2 + y^2)^{-1/2})
// du/dx = 1. (x^2+y^2)^{-1/2} + x(-1/2)(x^2+y^2){-3/2}(2x) = (x^2 + y^2)^{-1/2} - x^2(x^2 + y^2)^{-3/2}
// du/dy = 1.  x(-1/2)(x^2+y^2){-3/2}(2y) = - xy(x^2 + y^2)^{-3/2}
function jac(x, y, tx, ty) {
    let xsqysq = x*x+y*y;
    let dudx = Math.pow(xsqysq, -1/2) - x*x*Math.pow(xsqysq, -3/2);
    let dudy =  -x*y*Math.pow(xsqysq, -3/2);
    let dvdx = dudy;
    let dvdy = Math.pow(xsqysq, -1/2) - y*y*Math.pow(xsqysq, -3/2);


    let ox = tx*dudx + ty*dudy;
    let oy = tx*dvdx +  ty*dvdy;
    console.log("dudx: ", dudx, "dudy: ", dudy, "dvdx: ", dvdx, "dvdy: ", dvdy, "ox: ", ox, "oy: ", oy);
    return [ox, oy];
}


const interactive_derivative = ( sketch ) => {
    let x = 100;
    let y = 100;

    sketch.setup = () => {
	let myCanvas = sketch.createCanvas(W, H);
	// myCanvas.parent(document.getElementById('myContainer'));
	myCanvas.parent('interactive-derivative');
    };

    sketch.draw = () => {
	sketch.background(238,238,238);

	if (sketch.mouseIsPressed) {
	    pX = sketch.mouseX - W/2;
	    pY = sketch.mouseY - H/2;
	}

	let vecX = (sketch.mouseX -W/2) - pX;
	let vecY = (sketch.mouseY - H/2) - pY;


	

	sketch.strokeWeight(6);
	sketch.strokeCap(sketch.SQUARE);
	sketch.stroke(216,27,96);;
	sketch.line(pX+W/2, pY+H/2, sketch.mouseX, sketch.mouseY);


	const TGTWT = 4;
	let sX = (TGTWT+R)*pX/Math.sqrt(pX*pX + pY*pY);
	let sY = (TGTWT+R)*pY/Math.sqrt(pX*pX + pY*pY);

	let stv = jac(pX, pY, vecX, vecY);
	sketch.strokeWeight(0);
	sketch.fill(69,90,100);
	sketch.ellipse(W/2, H/2, 2*R, 2*R);


	sketch.strokeWeight(6);
	sketch.strokeCap(sketch.SQUARE);
	sketch.stroke(30,136,229);
	sketch.line(W/2 + sX,
		    H/2 + sY,
		    W/2 +  sX+ 100*stv[0],
		    H/2 +  sY+ 100*stv[1]);

	sketch.strokeWeight(0);;

    };
};






const transform = ( s ) => {
    let pts = [];
    s.setup = () => {
	let myCanvas = s.createCanvas(W, H);
	myCanvas.parent('transform');
    };

    s.draw = () => {

	pts.push([s.mouseX - W/2, s.mouseY -H/2]);
	const NUM_POINTS = 10;
	if (pts.length > NUM_POINTS) {
	    pts.shift();
	}

	s.background(238,238,238);
	s.noFill();
	s.stroke(216,27,96);
	s.strokeWeight(6);
	s.beginShape();
	for (let i = 0; i < pts.length; ++i) {
	    s.stroke(216,27,96);
	    s.curveVertex(pts[i][0] + W/2, pts[i][1] +H/2);
	}
	s.endShape();

	s.strokeWeight(0);
	s.fill(69,90,100);
	s.ellipse(W/2, H/2, 2*R, 2*R);


	s.stroke(33,150,243);
	s.strokeWeight(6);
	s.beginShape();
	for (let i = 0; i < pts.length; ++i) {
	    const len = Math.sqrt(pts[i][0] * pts[i][0] + pts[i][1] * pts[i][1]);
	    s.curveVertex(W/2 + R*pts[i][0]/len, H/2 + R*pts[i][1]/len);
	}
	s.endShape();


    };
};
const crumple = ( s ) => {
    const fn = 100; // total # of frames
    let fi = 0; // current frame.
    let pts = []
    s.setup = () => {
	let myCanvas = s.createCanvas(W, H);
	myCanvas.parent(document.getElementById('crumple'));

	NPOINTS = 0;
	let n = 0; let d = 1;
	let rs = []; // random numbers from halton sequence;
	
	for(let i = 0; i < 2*NPOINTS; ++i) {
	    const x = d - n;
	    if (x == 1) {
		n = 1; d *= 2;
	    } else {
		let y = Math.floor(d / 2);
		while(x <= y) {
		    y = Math.floor(y/2);
		}
		const n = 3*y - x;
		rs.push(n/2);
	    }
	}

	for(let i = 0; i < NPOINTS; i += 2) {
	    pts.push([rs[i], rs[i+1]]);
	}
    };

    s.draw = () => {
	s.background(238,238,238);
	s.noFill();

	s.strokeWeight(0);
	s.fill(69,90,100);
	s.ellipse(W/2, H/2, 2*R, 2*R);

	// generate points using halton sequence?

	// homotope points.
	fi++;
	if (fi > 2*fn) { fi = 0; }
	let t = Math.min(fn, fi)/fn;

	s.strokeWeight(10);
	let c0 = s.color(216,27,96, 255);
	let c1 = s.color(30,136,229, 255);
	const BORDER = 60;
	s.stroke(s.lerpColor(c0, c1, t));
	const N = 100;
	for(let x0 = -400; x0 < 400; x0 += 50 )  {
	    let y0 = H/2 - BORDER;
	    // point is at (i, y)
	    let x1 = R*i/Math.sqrt(x0*x0 + y0*y0);
	    let y1 = R*y0/Math.sqrt(x0*x0 + y0*y0);
	    s.point(W/2 + (1 - t)*x0 + t*x1, H/2 + (1-t)*y0 + t*y1);
	}

    };
};

let p5_crumple = new p5(crumple);
let p5_transform = new p5(transform);
let p5_interactive_derivative = new p5(interactive_derivative);


