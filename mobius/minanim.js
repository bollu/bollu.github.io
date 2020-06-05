function assert_precondition(t, out, tstart) {
    console.assert(typeof(t) === "number");
    if (out === undefined) { out = {}; }
    console.assert(typeof(out) === "object");
    if (tstart === undefined) { tstart = 0; }
    else { console.assert(typeof(tstart) === "number"); }
    console.assert(t >= tstart);
    return [out, tstart];
}

function anim_delay(duration) {
    console.assert(typeof(duration) === "number");
    let f = function(t, out, tstart) { 
        [out, tstart] = assert_precondition(t, out, tstart); return out;
    }
    f.duration = duration;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;
}

function anim_const(field, v) {
    let f = function(t, out, tstart) {
        [out, tstart] = assert_precondition(t, out, tstart); out[field] = v; return out;
    };
    f.duration = 0;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;
}

function ease_linear(vstart, tlin, vend) { return (1.0 - tlin) * vstart + tlin * vend; }

function ease_cubic(vstart, tlin, vend) {
    const cube = (1 - tlin)*(1-tlin)*(1-tlin); return cube * vstart + (1 - cube) * vend;
}
function ease_out_back(vstart, tlin, vend) {
    const c1 = 1.70158; const c3 = c1 + 1; const t = 1 + c3 * Math.pow(tlin - 1, 3) + c1 * Math.pow(tlin - 1, 2);
    return (1-t) * vstart + t*vend;
}

                                        
function anim_interpolated(fease, field, vend, duration) {
    let f =  function(t, out, tstart) {
        [out, tstart] = assert_precondition(t, out, tstart);
        if (t < tstart + duration && duration !== 0) {
            const tlin = (t - tstart) /duration;
            console.assert(tlin >= 0);
            console.assert(tlin <= 1);
            const vstart = out[field];
            out[field] = fease(vstart, tlin, vend);
        } else { out[field] = vend; }
        return out;
    };
    f.duration = duration;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;

}

function anim_sequence(anim1, anim2) {
    const duration = anim1.duration + anim2.duration;
    let f =  function(t, out, tstart) {
        [out, tstart] = assert_precondition(t, out, tstart);
        anim1(t, out, tstart);
        if (t >= tstart + anim1.duration) { anim2(t, out, tstart + anim1.duration); }
        return out;
    }
    f.duration = duration;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;
}

function anim_parallel(anim1, anim2) {
    const duration = Math.max(anim1.duration, anim2.duration);
    let f =  function(t, out, tstart) {
        [out, tstart] = assert_precondition(t, out, tstart);
        if (t >= tstart) { anim1(t, out, tstart); anim2(t, out, tstart); }
        return out;
    }
    f.duration = duration;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;
}

function anim_parallel_list(xs) { var x = xs[0]; for(var i = 1; i < xs.length; ++i) { x = x.par(xs[i]); } return x; }
function anim_sequence_list(xs) { var x = xs[0]; for(var i = 1; i < xs.length; ++i) { x = x.seq(xs[i]); } return x; }

function anim_stagger(xs, delta) {
    console.assert(typeof(delta) == "number");
    var ys = [];
    for(var i = 0; i < xs.length; ++i) {
        ys.push(anim_delay(delta*i).seq(xs[i]));
    }
    var y = ys[0];
    for(var i = 1; i < ys.length; ++i) {
        y = y.par(ys[i]);
    }
    return y;
}
