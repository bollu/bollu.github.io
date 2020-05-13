//LIBRARY: Promises
var _promiseTimeouts = new Map;
// TODO: pull in width as well


function* now() {
    while (true) {  yield Date.now(); }
}

function constant(x) { return function() { return x; }; }

function _whenTimeout(now, time) {
    var t = new Promise(function(resolve) {
        _promiseTimeouts.delete(time);
        var delay = time - now;
        if (!(delay > 0)) throw new Error("invalid time");
        if (delay > 0x7fffffff) throw new Error("too long to wait");
        setTimeout(resolve, delay);
    });
    _promiseTimeouts.set(time, t);
    return t;
}

function promiseWhen(time, value) {
    var now;
    return (now = _promiseTimeouts.get(time = +time)) ? now.then(constant(value))
    : (now = Date.now()) >= time ? Promise.resolve(value)
    : _whenTimeout(now, time).then(constant(value));
}

function promiseTick(duration, value) {
    return promiseWhen(Math.ceil((Date.now() + 1) / duration) * duration, value);
}

function promiseDelay(duration, value) {
    return new Promise(function(resolve) {
        setTimeout(function() {
            resolve(value);
        }, duration);
    });
}

//LIBRARY:LINALG

const EPS = 1e-4;
// solve p1 x + q1 y = b1 | p2 x + q2 y = b2
function matvec(m, v) {
    return [m[0][0] * v[0] + m[0][1] * v[1], m[1][0] *v[0] + m[1][1] * v[1]]
}

function det(m) {
    return m[0][0] * m[1][1] - m[1][0] * m[0][1];
}

function inv(m) {
    var d = det(m);
    console.assert(Math.abs(d) > EPS);
    return [[m[1][1]/d, -m[0][1]/d], [-m[1][0]/d, m[0][0]/d]];
}

function matmat(m1, m2) {
    m3 = [[0, 0], [0, 0]]
    for(var i = 0; i < 2; ++i) {
        for(var j = 0; j < 2; ++j) {
            for(var k = 0; k < 2; ++k) {
                m3[i][k] += m1[i][j] * m2[j][k];
            }
        }
    }
    return m3;
}

// solve (p1x + q1 y = b1 | p2x + q2y = b2)
function solve2d(p1, q1, b1, p2, q2, b2) {
    return matvec(inv([[p1, q1], [p2, q2]]), [b1, b2])
}

// convert pair of points (pt1, pt2) into [[p, q], b] for line eqn px + qy = b
function points2line(pt1, pt2) {
    // (y - y1) = (y2-y1)/(x2-x1) * (x - x1)
    // (y - y1) (x2 - x1) = (y2 - y1) (x - x1)
    // y (x2 - x1) - (y2 - y1) x = y1(x2 - x1) - x1 (y2 - y1)
    // y δx - x δy = y1 δx - x1 δy | TODO: think about this properly.
    delta = [pt2[1] - pt1[1], pt2[0] - pt1[0]]
    return [delta[1], delta[0], pt1[1] * delta[0] - pt1[0] * delta[1]];
}

function make_anim1_gen(container, points) {
    return (async function*() {
        var i = 0;
        while(true) {
            container.innerHTML = i;
            i += 1;
            yield;
            await promiseTick(1000);
        }
    })()
}

function animator_from_generator(gen) {
    gen.next().then(function() { 
        gen.next().then(animator_from_generator(gen));
    });
}

function init_interior_point() {
    const anim1 = make_anim1_gen(document.getElementById("animation-1"), 
         [[0, 0], [0, 1], [1, 0]])
    animator_from_generator(anim1);
}                      
