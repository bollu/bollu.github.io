const W = 720;
const H = 400;
function setup() {
    createCanvas(W, H);
}

let pX = 200;
let pY = 0;

// sage: x, y = var("x y")
// sage: fx = x/sqrt(x*x+y*y)
// sage: Jx = Matrix([fx.derivative(x), fx.derivative(y)])
// sage: fy = y/sqrt(x*x+y*y)
// sage: Jy = Matrix([fy.derivative(x), fy.derivative(y)])
// sage:                                                                                                                                         


// sage: Jy.T                                                                                                                                    
// [                    -x*y/(x^2 + y^2)^(3/2)]
// [-y^2/(x^2 + y^2)^(3/2) + 1/sqrt(x^2 + y^2)]

function jacY(x, y) {
    let x2y2 = x*x+y*y;
    let dx = (-x*y)/Math.pow(x2y2, 3/2);
    let dy = -y*y/Math.pow(x2y2,3/2) + 1/Math.sqrt(x2y2);
    return [dx, dy];
    
}


// sage: Jx.T                                                                                                                                    
// [-x^2/(x^2 + y^2)^(3/2) + 1/sqrt(x^2 + y^2)]
// [                    -x*y/(x^2 + y^2)^(3/2)]

function jacX(x, y) {
    let x2y2 = x*x+y*y;
    let dx = -x*x/Math.pow(x2y2, 3/2) + 1/Math.sqrt(x2y2);
    let dy = (-x*y)/Math.pow(x2y2, 3/2);
    return [dx, dy];
}

// compute image of (vX, vY) under J|_(pX, pY)
function jac(pX, pY, vX, vY) {
    let jX = jacX(pX, pY);
    let jY = jacY(pX, pY);
    // return [vX*jX[0] + vY*jX[1], vY*jY[0] + vY*jY[1]];
    return [vX*jX[0] + vY*jY[0], vX*jX[1] + vY*jY[1]];
}


function draw() {
    background(102);

    if (mouseIsPressed) {
	pX = mouseX - W/2;
	pY = mouseY - H/2;
    }

    let vecX = (mouseX -W/2) - pX;
    let vecY = (mouseY - H/2) - pY;

    const R = 50;
    
    let sX = R*pX/Math.sqrt(pX*pX + pY*pY);
    let sY = R*pY/Math.sqrt(pX*pX + pY*pY);


    stroke(0);
    strokeWeight(4);
    line(pX+W/2, pY+H/2, mouseX, mouseY);

   
    let JACAMP = 10; // amplication of input to JAC
    let stv = jac(pX, pY, vecX, vecY);
    ellipse(W/2, H/2, 2*R, 2*R);
    line(W/2 + sX,
	 H/2 + sY,
	 W/2 + 2*sX + 20*stv[0],
	 H/2 + 2*sY + 20*stv[1]);

    strokeWeight(0);;


}

function polygon(x, y, radius, npoints) {
    let angle = TWO_PI / npoints;
    beginShape();
    for (let a = 0; a < TWO_PI; a += angle) {
	let sx = x + cos(a) * radius;
	let sy = y + sin(a) * radius;
	vertex(sx, sy);
    }
    endShape(CLOSE);
}

