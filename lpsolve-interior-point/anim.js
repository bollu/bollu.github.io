function assert_precondition(t, out, tstart) {
    console.assert(typeof(t) === "number");
    console.assert(typeof(out) === "object");
    if (tstart === undefined) { tstart = 0; }
    else { console.assert(typeof(tstart) === "number"); }
    console.assert(t >= tstart);
    return [out, tstart];
}

function anim_delay(duration) {
    let f = function(t, out, tstart) { 
        [out, tstart] = assert_precondition(t, out, tstart);
        return out;
    }
    f.duration = duration;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;
}

function anim_const(field, v) {
    let f = function(t, out, tstart) {
        [out, tstart] = assert_precondition(t, out, tstart);
        out[field] = v;
        return out;
    };
    f.duration = 0;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;

}

function ease_linear(vstart, tlin, vend) {
    return (1.0 - tlin) * vstart + tlin * vend;
}

function ease_cubic(vstart, tlin, vend) {
    const cube = (1 - tlin)*(1-tlin)*(1-tlin);
    return cube * vstart + (1 - cube) * vend;
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
        } else { 
            out[field] = vend;
        }
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
        if (t >= tstart + anim1.duration) {
            anim2(t, out, tstart + anim1.duration);
        }
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

        if (t >= tstart) {
            anim1(t, out, tstart);
            anim2(t, out, tstart);
        }
        return out;
    }
    f.duration = duration;
    f.par = ((g) => anim_parallel(f, g));
    f.seq = ((g) => anim_sequence(f, g));
    return f;
}
