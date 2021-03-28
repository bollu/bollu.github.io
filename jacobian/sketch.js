const W = 720;
const H = 400;
const R = 50;

let pX = 200;
let pY = 0;

function easeInOutQuad(x) {
    return x < 0.5 ? 2 * x * x : 1 - Math.pow(-2 * x + 2, 2) / 2;
}


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


const interactive_derivative = ( s ) => {
    let x = 100;
    let y = 100;

    s.setup = () => {
	let myCanvas = s.createCanvas(W, H);
	// myCanvas.parent(document.getElementById('myContainer'));
	myCanvas.parent('interactive-derivative');
    };

    s.draw = () => {
    s.background(255,253,231);

	if (s.mouseIsPressed) {
	    pX = s.mouseX - W/2;
	    pY = s.mouseY - H/2;
	}

	let vecX = (s.mouseX -W/2) - pX;
	let vecY = (s.mouseY - H/2) - pY;


	

	s.strokeWeight(6);
	s.strokeCap(s.SQUARE);
	s.stroke(216,27,96);;
	s.line(pX+W/2, pY+H/2, s.mouseX, s.mouseY);


	const TGTWT = 4;
	let sX = (TGTWT+R)*pX/Math.sqrt(pX*pX + pY*pY);
	let sY = (TGTWT+R)*pY/Math.sqrt(pX*pX + pY*pY);

	let stv = jac(pX, pY, vecX, vecY);
	s.strokeWeight(0);
	s.fill(69,90,100);
	s.ellipse(W/2, H/2, 2*R, 2*R);


	s.strokeWeight(6);
	s.strokeCap(s.SQUARE);
	s.stroke(30,136,229);
	s.line(W/2 + sX,
		    H/2 + sY,
		    W/2 +  sX+ 100*stv[0],
		    H/2 +  sY+ 100*stv[1]);

	s.strokeWeight(0);;

    };
};



const transform_anim_normal = ( s ) => {
    let pts = [];
    s.setup = () => {
	let myCanvas = s.createCanvas(W, H);
	myCanvas.parent('transform-anim-normal');
    };

    let t = 0;

    s.draw = () => {


	const V = 0.02;
    t = (t + V) % (4);
	const NUM_POINTS = 40;
	if (pts.length > NUM_POINTS) {
	    pts.shift();
	}

    const RMOVE = 200;
    if (t <= 1) { // 0-1
	    pts.push([R*2 + RMOVE * easeInOutQuad(t), 0]);
	} else if (t <= 2) { // 1-2
	    pts.push([R*2 + RMOVE * easeInOutQuad(1), 0]);
    } else if (t <= 3) {
        let tcur = t - 2;
        let trev = 1 - tcur; // 1 - (t - 1) = 1 - t + 1 = 2 - t
	    pts.push([R*2 + RMOVE * easeInOutQuad(trev), 0]);
    } else if (t <= 4) {
	    pts.push([R*2 + RMOVE * easeInOutQuad(0), 0]);
    }


	s.background(238,238,238);
    s.background(255,253,231);
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

const transform_anim_tangential = ( s ) => {
    let pts = [];
    s.setup = () => {
	let myCanvas = s.createCanvas(W, H);
	myCanvas.parent('transform-anim-tangential');
    };

    let t = 0;

    s.draw = () => {


	const V = 0.02;
    t = (t + V) % (4);
	const NUM_POINTS = 40;
	if (pts.length > NUM_POINTS) {
	    pts.shift();
	}

    const RMOVE = 300;
    if (t <= 1) { // 0-1
	    pts.push([R*2, -R*3 + RMOVE * easeInOutQuad(t)]);
	} else if (t <= 2) { // 1-2
	    pts.push([R*2, -R*3 + RMOVE * easeInOutQuad(1)]);
    } else if (t <= 3) {
        let tcur = t - 2;
        let trev = 1 - tcur; // 1 - (t - 1) = 1 - t + 1 = 2 - t
	    pts.push([R*2, -R*3 + RMOVE * easeInOutQuad(trev)]);
    } else if (t <= 4) {
	    pts.push([R*2, -R*3 + RMOVE * easeInOutQuad(0)]);
    }


	s.background(238,238,238);
    s.background(255,253,231);
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



const transform_anim = ( s ) => {
    let pts = [];
    s.setup = () => {
	let myCanvas = s.createCanvas(W, H);
	myCanvas.parent('transform-anim');
    };

    let t1 = 0; let t2 = 0;

    s.draw = () => {

    const R1 = 150; const R2 = 35; const V1 = 0.01; V2 = 0.05;
    pts.push([R1 * Math.cos(t1) + R2 * Math.cos(t2), R1 * Math.sin(t1) + R2 * Math.sin(t2)]);
    t1 = (t1 + V1) % (2*Math.PI);
    t2 = (t2 + V2) % (2*Math.PI);

	const NUM_POINTS = 40;
	if (pts.length > NUM_POINTS) {
	    pts.shift();
	}

	s.background(238,238,238);
    s.background(255,253,231);
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
    s.background(255,253,231);
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
    s.background(255,253,231);
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

const static_derivative = ( s ) => {

    let pts = [];

    let fi = 0;
    let fv = 1;

    s.setup = () => {
	let myCanvas = s.createCanvas(W, H);
	// myCanvas.parent(document.getElementById('myContainer'));
	myCanvas.parent('static-derivative');

	for (let i = -200; i < 200; ++i) {
        let x = 2*i;
        let y =  2*R;
        pts.push([i, x, y]);
	}
    };

    s.draw = () => {
    s.background(255,253,231);

	s.strokeWeight(0);
	s.fill(69,90,100);
	s.ellipse(W/2, H/2, 2*R, 2*R);


	s.stroke(66,66,66);
	s.strokeWeight(4);
    s.strokeCap(s.SQUARE);
	s.noFill();
	s.beginShape();
	for (let i = 0; i < pts.length; ++i) {
        let x = pts[i][1];
        let y =  pts[i][2];
	    s.curveVertex(W/2 + x, H/2 + y);
	}
	s.endShape();



    const LEN = 50;
    if (fv == 1 && fi == pts.length - LEN - 1) { fv = -1; }
    if (fv == -1 && fi == 1 ) { fv = 1; }
    fi += fv;

    // blue
	s.strokeWeight(6);
    s.stroke(33,150,243);
	s.noFill();
	s.beginShape();
	for (let i = fi; i < fi + LEN; ++i) {
        let x = pts[i][1];
        let y =  pts[i][2];
	    s.curveVertex(W/2 + x, H/2 + y);
	}
	s.endShape();

    const xmid = pts[fi+LEN/2][1];
    const ymid = pts[fi+LEN/2][2];

    s.strokeWeight(0);
    s.fill(33,150,243);
    s.ellipse(W/2 + xmid, H/2 + ymid, 15, 15);

    // pink
    s.stroke(233,30,99);
	s.strokeWeight(6);
    s.strokeCap(s.SQUARE);
	s.noFill();
	s.beginShape();
	for (let i = fi; i < fi + LEN; ++i) {
        let x = pts[i][1];
        let y =  pts[i][2];
        let px = R*x/Math.sqrt(x*x+y*y);
        let py = R*y/Math.sqrt(x*x+y*y);
	    s.curveVertex(W/2 + px, H/2 + py);
	}
	s.endShape();


    const pxmid = R*xmid/Math.sqrt(xmid*xmid+ymid*ymid);
    const pymid = R*ymid/Math.sqrt(xmid*xmid+ymid*ymid);
    s.strokeWeight(0);
    s.fill(233,30,99);
    s.ellipse(W/2 + pxmid, H/2 + pymid, 15, 15);




    // const i = pts[fi][0];
    // const x1 = pts[fi][1];
    // const y1 = pts[fi][2];
    // const x2 = pts[fi+1][1];
    // const y2 = pts[fi+1][2];

    // let m = (y2 - y1)/(x2 - x1);

	// s.strokeWeight(0);
    // s.fill(33,150,243);
	// s.ellipse(W/2 + x1, H/2 + y1, 20, 20);

    // s.strokeWeight(6);
    // s.strokeCap(s.SQUARE);
	// s.stroke(33,150,243);
    // s.line(W/2 + x1 - LEN, H/2 + y1 - m*LEN, W/2 + x1 + LEN, H/2 + y1 + m*LEN);

    // const px1 = R*x1 / Math.sqrt(x1*x1 + y1*y1);
    // const py1 = R*y1 / Math.sqrt(x1*x1 + y1*y1);
    // const px2 = R*x2 / Math.sqrt(x2*x2 + y2*y2);
    // const py2 = R*y2 / Math.sqrt(x2*x2 + y2*y2);
    // const pm = (py2 - py1)/(px2 - px1);

	// s.strokeWeight(0);
    // s.fill(233,30,99);
	// s.ellipse(W/2 + px1, H/2+py1, 20, 20);

    // s.strokeWeight(6);
    // s.strokeCap(s.SQUARE);
    // s.stroke(233,30,99);
    // s.line(W/2 + px1 - LEN, H/2 + py1 - pm*LEN, W/2 + px1 + LEN, H/2 + py1 + pm*LEN);


	// for (let i = 0; i < pts.length - 1; i += 100) {
    //     let x1 = pts[i][0];
    //     let y1 =  pts[i][1];

    //     let x2 = pts[i+1][0];
    //     let y2 = pts[i+1][1];

    //     let m = (y2 - y1)/(x2 - x1);

    //     s.strokeWeight(6);
    //     s.strokeCap(s.SQUARE);
    //     s.stroke(26,35,126);
    //     const LEN = 50;
    //     s.line(x1, y1, x2 + LEN, y2 + m*LEN);
    // }


    };
};


let p5_crumple = new p5(crumple);
let p5_transform = new p5(transform);
let p5_transform_anim = new p5(transform_anim);
let p5_transform_anim_normal = new p5(transform_anim_normal);
let p5_transform_anim_tangential = new p5(transform_anim_tangential);
let p5_interactive_derivative = new p5(interactive_derivative);
let p5_static_derivative = new p5(static_derivative);


