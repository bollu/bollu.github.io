const W = 720;
const H = 400;
const R = 50;

let pX = 200;
let pY = 0;

function easeOutQuart(x) {
    return 1 - Math.pow(1 - x, 4);
}

function easeInQuart(x) {
    return x * x * x * x;
}

function halton (index, base) {
  let fraction = 1;
  let result = 0;
  while (index > 0) {
    fraction /= base;
    result += fraction * (index % base);
    index = Math.floor(index / base); // floor division
  }
  return result;
}


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
    // console.log("dudx: ", dudx, "dudy: ", dudy, "dvdx: ", dvdx, "dvdy: ", dvdy, "ox: ", ox, "oy: ", oy);
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
    }

    s.draw = () => {
	s.background(238,238,238);
	s.noFill();

	s.strokeWeight(0);
	s.fill(69,90,100);
	s.ellipse(W/2, H/2, 2*R, 2*R);

	// generate points using halton sequence?
	// homotope points.
	const NFRAME_POINTS = 100;
	const NFRAME_TRANSFORM = 40;
	const NFRAME_STAY = 20;
	const NFRAME_HIDE = 20;


	let c0 = s.color(216,27,96, 255);
	let c1 = s.color(30,136,229, 128);
	const BORDER = 60;

	fi++;
	let fcur = fi;
	
	s.strokeWeight(10);

	if (fcur < NFRAME_POINTS) {
	    if (fcur == 0) {
		pts = [];
	    } else {
		let rRand = 2*R + halton(fcur, 2) * 6*R;
		let thetaRand = halton(fcur, 3) * 2 * Math.PI
		
		pts.push([rRand * Math.cos(thetaRand), rRand * Math.sin(thetaRand)]);
	    }
	    for(let i = 0; i < pts.length; ++i) {
		let x0 = pts[i][0];
		let y0 = pts[i][1];
		
		let t = Math.min(1, (fcur - i)/5);
		let ct = s.color(c0);
		ct.setAlpha(255*t);
		s.stroke(ct);
		s.point(W/2 + x0, H/2 + y0);
	    }
	    return;
	} else {
	    fcur -= NFRAME_POINTS;
        }

	if (fcur < NFRAME_STAY) {
	    let t = fcur /NFRAME_STAY;
	    for(let i = 0; i < pts.length; ++i) {
		let x0 = pts[i][0];
		let y0 = pts[i][1];
		let x1 = R*x0/Math.sqrt(x0*x0 + y0*y0);
		let y1 = R*y0/Math.sqrt(x0*x0 + y0*y0);
		s.stroke(c0);
		s.point(W/2 + x0, H/2 + y0);		
	    }
	    return;
	} else { fcur -= NFRAME_STAY };

	
	if (fcur < NFRAME_TRANSFORM) {
	    let t = fcur/NFRAME_TRANSFORM;
	    t = easeOutQuart(t);
	    console.log("transform t", t);
	    for(let i = 0; i < pts.length; ++i) {
		let x0 = pts[i][0];
		let y0 = pts[i][1];
		let x1 = R*x0/Math.sqrt(x0*x0 + y0*y0);
		let y1 = R*y0/Math.sqrt(x0*x0 + y0*y0);

		s.stroke(s.lerpColor(c0, c1, t));
		s.point(W/2 + (1-t)*x0 + t*x1, H/2 + (1-t)*y0 + t*y1);		
	    }
	    return;
	    
	} else {
	    fcur -= NFRAME_TRANSFORM;
	}

	if (fcur < NFRAME_STAY) {
	   for(let i = 0; i < pts.length; ++i) {
		let x0 = pts[i][0];
		let y0 = pts[i][1];
		let x1 = R*x0/Math.sqrt(x0*x0 + y0*y0);
		let y1 = R*y0/Math.sqrt(x0*x0 + y0*y0);
	        s.stroke(c1);
		s.point(W/2 + x1, H/2 + y1);		
	   }
	   return;
	} else {
	    fcur -= NFRAME_STAY;
	}
	
	if (fcur < NFRAME_HIDE) {
	    let t = fcur /NFRAME_HIDE;
	    t = easeInQuart(t);
	    for(let i = 0; i < pts.length; ++i) {
		let x0 = pts[i][0];
		let y0 = pts[i][1];
		let x1 = R*x0/Math.sqrt(x0*x0 + y0*y0);
		let y1 = R*y0/Math.sqrt(x0*x0 + y0*y0);
		let ct = s.color(c1);
		ct.setAlpha(128*(1.0 - t));
		s.stroke(ct);
		s.point(W/2 + x1, H/2 + y1);		
	    }
	    return;
	}

	// pause.
	if (fcur < 50) { return; }

	// ran no animation. exhausted.
	fi = 0;
	pts = [];
	return;
    };
};

let p5_crumple = new p5(crumple);
let p5_transform = new p5(transform);
let p5_interactive_derivative = new p5(interactive_derivative);


