const W = 720;
const H = 400;
function setup() { createCanvas(W, H); }

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


function draw() {
    background(238,238,238);

    if (mouseIsPressed) {
	pX = mouseX - W/2;
	pY = mouseY - H/2;
    }

    let vecX = (mouseX -W/2) - pX;
    let vecY = (mouseY - H/2) - pY;

    const R = 50;
    

    strokeWeight(6);
    strokeCap(SQUARE);
    stroke(13,71,161);
    line(pX+W/2, pY+H/2, mouseX, mouseY);


    const TGTWT = 4;
    let sX = (TGTWT+R)*pX/Math.sqrt(pX*pX + pY*pY);
    let sY = (TGTWT+R)*pY/Math.sqrt(pX*pX + pY*pY);

    let stv = jac(pX, pY, vecX, vecY);
    strokeWeight(0);
    fill(69,90,100);
    ellipse(W/2, H/2, 2*R, 2*R);


    strokeWeight(6);
    strokeCap(SQUARE);
    stroke(30,136,229);
    line(W/2 + sX,
	 H/2 + sY,
	 W/2 +  sX+ 100*stv[0],
	 H/2 +  sY+ 100*stv[1]);

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

