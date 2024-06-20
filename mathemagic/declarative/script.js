"use strict";
//https://www.d3indepth.com/shapes/
//LIBRARY: Promises
function promiseDuration(dt) {
    return new Promise((resolve) => { setTimeout(() => resolve(), dt); });
}

let anim_circle = anim_const("cx", 50)
    .seq(anim_const("cr", 0))
    // (1) grow in size.
    .seq(anim_interpolated(ease_cubic, "cr", /*val=*/10, /*time=*/3))
    // (2) go to right while growing.
    .seq(anim_interpolated(ease_cubic, "cx", /*val=*/300, /*time=*/1)
        .par(anim_interpolated(ease_cubic, "cr", 70, 1)))
    // (3) pause.
    .seq(anim_delay(/*time=*/3))
    // (4) come back to the left.
    .seq(anim_interpolated(ease_cubic, "cx", 200, 1))
    // (5) pause again.
    .seq(anim_delay(/*time=*/2))
    // (6) shrink to nothing.
    .seq(anim_interpolated(ease_cubic, "cr", 0, 1));


function make_anim1_gen(container) {

    const width = 500;
    const height = 200;
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    
    const circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
    circle.setAttribute("cy", 100);
    circle.setAttribute("fill", "#1a73e8");
    svg.appendChild(circle);
    container.appendChild(svg);
    return (async function*() {

        const DT = 1.0/30.0;
        const TOTALFRAMES = 500;
        while(true) {
            for(var i = 0; i < TOTALFRAMES; ++i) {
                const val = anim_circle(i  / TOTALFRAMES * anim_circle.duration, {});
                circle.setAttribute("cx", val.cx);
                circle.setAttribute("r", val.cr);
                await promiseDuration(DT);
                yield;
            }
            await promiseDuration(100); yield;
        }
    })();
}


function make_anim2_gen(container) {

    const width = 500;
    const height = 200;
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);
    
    const circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
    circle.setAttribute("cy", 100);
    circle.setAttribute("fill", "#1a73e8");
    svg.appendChild(circle);

    return (async function*() {
        const DT = 1.0/30.0;
        while(true) {
            const t = document.getElementById("animation-2-scrubber").value / 1000.0; 
            const anim = anim_circle(t * anim_circle.duration, {});

            circle.setAttribute("cx", anim.cx);
            circle.setAttribute("r", anim.cr);
            await promiseDuration(DT);
            yield;
        }
    })();
}

function make_anim3_gen(container) {
    const width = 500;
    const height = 200;
    const NCIRCLES = 10;
    const STAGGER = 80;

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);
    

    let circles = [];
    let anim_circles_start = [];
    let anim_circles_enter = [];
    let anim_circles_leave = [];
    const RAD = 5;
    for(var i = 0; i < NCIRCLES; ++i) {
        let circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
        svg.appendChild(circle);

        circle.setAttribute("fill", "#1a73e8");
        circle.setAttribute("r", RAD);
        circle.setAttribute("cx", RAD *3 + RAD * 5 * i);
        circle.setAttribute("cy", 300);
        circles.push(circle);
        anim_circles_start.push(anim_const("cy" + i, 300)
                                .seq(anim_const("cr"+i, 10)));
        anim_circles_enter.push(anim_interpolated(ease_cubic, "cy" + i, 100, 300));
        anim_circles_leave.push(anim_interpolated(ease_cubic, "cr" + i, 0, 800));
    }

    
    const anim = anim_parallel_list(anim_circles_start)
        .seq(anim_stagger([anim_stagger(anim_circles_enter, STAGGER),
                           anim_stagger(anim_circles_leave, STAGGER)], 300));

    return (async function*() {
        const TOTALFRAMES = 1000;
        const DT = 1.0 / 30.0;
        while(true) {
            for(let n = 0; n < TOTALFRAMES; ++n) {
                const val = anim(n / TOTALFRAMES * anim.duration, {});
                for(var i = 0; i < NCIRCLES; ++i) {
                    circles[i].setAttribute("cy", val["cy" + i]);
                    circles[i].setAttribute("r", val["cr" + i]);
                }
                await promiseDuration(DT);
                yield;
            }
        }
    })();
}


function make_anim31_gen(container) {
    const width = 500;
    const height = 200;
    const NCIRCLES = 10;
    const STAGGER = 80;

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);
    

    let circles = [];
    let anim_circles_start = [];
    let anim_circles_enter = [];
    let anim_circles_leave = [];
    const RAD = 5;
    for(var i = 0; i < NCIRCLES; ++i) {
        let circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
        svg.appendChild(circle);

        circle.setAttribute("fill", "#1a73e8");
        circle.setAttribute("r", RAD);
        circle.setAttribute("cx", RAD *3 + RAD * 5 * i);
        circle.setAttribute("cy", 300);
        circles.push(circle);
        anim_circles_start.push(anim_const("cy" + i, 300)
                                .seq(anim_const("cr"+i, 10)));
        anim_circles_enter.push(anim_interpolated(ease_cubic, "cy" + i, 100, 300));
        anim_circles_leave.push(anim_interpolated(ease_cubic, "cr" + i, 0, 800));
    }

    
    // const anim = anim_parallel_list(anim_circles_start)
    const anim = anim_parallel_list(anim_circles_start)
            .seq(anim_parallel_list(anim_circles_enter));

    return (async function*() {
        const TOTALFRAMES = 1000;
        const DT = 1.0 / 30.0;
        while(true) {
            for(let n = 0; n < TOTALFRAMES; ++n) {
                const val = anim(n / TOTALFRAMES * anim.duration, {});
                for(var i = 0; i < NCIRCLES; ++i) {
                    circles[i].setAttribute("cy", val["cy" + i]);
                    circles[i].setAttribute("r", val["cr" + i]);
                }
                await promiseDuration(DT);
                yield;
            }
        }
    })();
}

function make_anim32_gen(container) {
    const width = 500;
    const height = 200;
    const NCIRCLES = 10;
    const STAGGER = 80;

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);
    

    let circles = [];
    let anim_circles_start = [];
    let anim_circles_enter = [];
    let anim_circles_leave = [];
    const RAD = 5;
    for(var i = 0; i < NCIRCLES; ++i) {
        let circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
        svg.appendChild(circle);

        circle.setAttribute("fill", "#1a73e8");
        circle.setAttribute("r", RAD);
        circle.setAttribute("cx", RAD *3 + RAD * 5 * i);
        circle.setAttribute("cy", 300);
        circles.push(circle);
        anim_circles_start.push(anim_const("cy" + i, 300)
                                .seq(anim_const("cr"+i, 10)));
        anim_circles_enter.push(anim_interpolated(ease_cubic, "cy" + i, 100, 300));
        anim_circles_leave.push(anim_interpolated(ease_cubic, "cr" + i, 0, 800));
    }

    
    // const anim = anim_parallel_list(anim_circles_start)
    const anim = anim_parallel_list(anim_circles_start)
        .seq(anim_stagger(anim_circles_enter, STAGGER));

    return (async function*() {
        const TOTALFRAMES = 1000;
        const DT = 1.0 / 30.0;
        while(true) {
            for(let n = 0; n < TOTALFRAMES; ++n) {
                const val = anim(n / TOTALFRAMES * anim.duration, {});
                for(var i = 0; i < NCIRCLES; ++i) {
                    circles[i].setAttribute("cy", val["cy" + i]);
                    circles[i].setAttribute("r", val["cr" + i]);
                }
                await promiseDuration(DT);
                yield;
            }
        }
    })();
}



function make_anim33_gen(container) {
    const width = 500;
    const height = 200;
    const NCIRCLES = 10;
    const STAGGER = 80;

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);
    

    let circles = [];
    let anim_circles_start = [];
    //let anim_circles_enter = [];
    let anim_circles_leave = [];
    const RAD = 5;
    for(var i = 0; i < NCIRCLES; ++i) {
        let circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
        svg.appendChild(circle);

        circle.setAttribute("fill", "#1a73e8");
        circle.setAttribute("r", RAD);
        circle.setAttribute("cx", RAD *3 + RAD * 5 * i);
        circle.setAttribute("cy", 100);
        circles.push(circle);
        anim_circles_start.push(anim_const("cy" + i, 100)
                                .seq(anim_const("cr"+i, 10)));
        //anim_circles_enter.push(anim_interpolated(ease_cubic, "cy" + i, 100, 300));
        anim_circles_leave.push(anim_interpolated(ease_cubic, "cr" + i, 0, 800));
    }

    
    // const anim = anim_parallel_list(anim_circles_start)
    const anim = anim_parallel_list(anim_circles_start)
        .seq(anim_stagger(anim_circles_leave, STAGGER));

    return (async function*() {
        const TOTALFRAMES = 1000;
        const DT = 1.0 / 30.0;
        while(true) {
            for(let n = 0; n < TOTALFRAMES; ++n) {
                const val = anim(n / TOTALFRAMES * anim.duration, {});
                for(var i = 0; i < NCIRCLES; ++i) {
                    circles[i].setAttribute("cy", val["cy" + i]);
                    circles[i].setAttribute("r", val["cr" + i]);
                }
                await promiseDuration(DT);
                yield;
            }
        }
    })();
}

function make_anim34_gen(container) {
    const width = 500;
    const height = 200;
    const NCIRCLES = 10;
    const STAGGER = 80;

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);
    

    let circles = [];
    let anim_circles_start = [];
    let anim_circles_enter = [];
    let anim_circles_leave = [];
    const RAD = 5;
    for(var i = 0; i < NCIRCLES; ++i) {
        let circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
        svg.appendChild(circle);

        circle.setAttribute("fill", "#1a73e8");
        circle.setAttribute("r", RAD);
        circle.setAttribute("cx", RAD *3 + RAD * 5 * i);
        circle.setAttribute("cy", 300);
        circles.push(circle);
        anim_circles_start.push(anim_const("cy" + i, 300)
                                .seq(anim_const("cr"+i, 10)));
        anim_circles_enter.push(anim_interpolated(ease_cubic, "cy" + i, 100, 300));
        anim_circles_leave.push(anim_interpolated(ease_cubic, "cr" + i, 0, 800));
    }

    
    const anim = anim_parallel_list(anim_circles_start)
        .seq(anim_stagger([anim_stagger(anim_circles_enter, STAGGER),
                           anim_stagger(anim_circles_leave, STAGGER)], 300));


    return (async function*() {
        const DT = 1.0 / 30.0;
        while(true) {
            const t = document.getElementById("animation-34-scrubber").value / 1000.0; 
            const val = anim(t * anim.duration, {});
            for(var i = 0; i < NCIRCLES; ++i) {
                circles[i].setAttribute("cy", val["cy" + i]);
                circles[i].setAttribute("r", val["cr" + i]);
            }
            await promiseDuration(DT);
            yield;
        }
    })();
}

function make_anim_showoff_easing_gen(container) {
    const width = 500;
    const height = 100;
    const NINTERPOLATORS = 3;

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);
    
    let circles = [];
    let anim_circles_start = [];
    let anim_circles_enter = [];
    const RAD = 10;
    const interpolators = [ease_cubic, ease_linear, ease_out_back]
    for(var i = 0; i < NINTERPOLATORS; ++i) {
        let circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
        svg.appendChild(circle);

        circle.setAttribute("fill", "#1a73e8");
        circle.setAttribute("r", RAD);
        circle.setAttribute("cx", 200);
        circle.setAttribute("cy", 20 + RAD *3 * i);
        circles.push(circle);
        anim_circles_start.push(anim_const("cx" + i, 200));
        anim_circles_enter.push(anim_interpolated(interpolators[i], "cx" + i, 300, 200));
        
    }

    
    // const anim = anim_parallel_list(anim_circles_start)
    const anim = anim_parallel_list(anim_circles_start)
            .seq(anim_parallel_list(anim_circles_enter));


    return (async function*() {
        const DT = 1.0 / 30.0;
        while(true) {
            const t = document.getElementById("animation-showoff-easing-scrubber").value / 1000.0; 
            const val = anim(t * anim.duration, {});
            for(var i = 0; i < NINTERPOLATORS; ++i) {
                circles[i].setAttribute("cx", val["cx" + i]);
            }
            await promiseDuration(DT);
            yield;
        }
    })();
}


function make_anim_interpolated_gen(container) {
    const width = 500;
    const height = 100;
    const NINTERPOLATORS = 3;

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);
    
    let circles = [];
    let anim_circles_start = [];
    let anim_circles_enter = [];
    let anim_circles_leave = [];

    const RAD = 10;
    const interpolators = [ease_cubic, ease_linear, ease_out_back]
    for(var i = 0; i < NINTERPOLATORS; ++i) {
        let circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
        svg.appendChild(circle);

        circle.setAttribute("fill", "#1a73e8");
        circle.setAttribute("r", RAD);
        circle.setAttribute("cx", 200);
        circle.setAttribute("cy", 20 + RAD *3 * i);
        circles.push(circle);
        anim_circles_start.push(anim_const("cx" + i, 200));
        anim_circles_enter.push(anim_interpolated(interpolators[i], "cx" + i, 300, 200));
        anim_circles_leave.push(anim_interpolated(interpolators[i], "cx" + i, 200, 200));

    }

    
    // const anim = anim_parallel_list(anim_circles_start)
    const anim = anim_parallel_list(anim_circles_start)
            .seq(anim_parallel_list(anim_circles_enter))
            .seq(anim_delay(100))
            .seq(anim_parallel_list(anim_circles_leave))
            .seq(anim_delay(100));


    return (async function*() {
        const TOTALFRAMES = 400;
        const DT = 1.0 / 30.0;
        while(true) {
            for(let n = 0; n < TOTALFRAMES; ++n) {
                const val = anim(n / TOTALFRAMES * anim.duration, {});
                for(var i = 0; i < NINTERPOLATORS; ++i) {
                    circles[i].setAttribute("cx", val["cx" + i]);
                }
                await promiseDuration(DT);
                yield;
            }
        }
    })();
}


function make_anim_sequence_gen(container) {
    const width = 500;
    const height = 100;
    const NCIRCLES = 3;

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);
    
    let circles = [];
    let anim_circles_start = [];
    let anim_circles_enter = [];
    let anim_circles_leave = [];

    const RAD = 10;
    for(var i = 0; i < NCIRCLES; ++i) {
        let circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
        svg.appendChild(circle);

        circle.setAttribute("fill", "#1a73e8");
        circle.setAttribute("r", RAD);
        circle.setAttribute("cx", 30 + i * 100);
        circle.setAttribute("cy", 50);
        circles.push(circle);
        anim_circles_start.push(anim_const("cx" + i, 30 + i * 100));
        anim_circles_enter.push(anim_interpolated(ease_linear, "cx" + i, 30 + (i + 1) * 100 - RAD*2, 300));
        anim_circles_leave[NCIRCLES-1-i] = (anim_interpolated(ease_linear, "cx" + i, 30 + i * 100, 300));
    }

    
    // const anim = anim_parallel_list(anim_circles_start)
    const anim = anim_parallel_list(anim_circles_start)
            .seq(anim_sequence_list(anim_circles_enter))
            .seq(anim_delay(100))
            .seq(anim_sequence_list(anim_circles_leave))
            .seq(anim_delay(100));
            
    return (async function*() {
        const TOTALFRAMES = 400;
        const DT = 1.0 / 30.0;
        while(true) {
            for(let n = 0; n < TOTALFRAMES; ++n) {
                const val = anim(n / TOTALFRAMES * anim.duration, {});
                for(var i = 0; i < NCIRCLES; ++i) {
                    circles[i].setAttribute("cx", val["cx" + i]);
                }
                await promiseDuration(DT);
                yield;
            }
        }
    })();
}


function make_anim_parallel_gen(container) {
    const width = 500;
    const height = 100;
    const NCIRCLES = 3;

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);
    
    let circles = [];
    let anim_circles_start = [];
    let anim_circles_enter = [];
    let anim_circles_leave = []
    const RAD = 10;
    for(var i = 0; i < NCIRCLES; ++i) {
        let circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
        svg.appendChild(circle);

        circle.setAttribute("fill", "#1a73e8");
        circle.setAttribute("r", RAD);
        circle.setAttribute("cx", 200);
        circle.setAttribute("cy", 20 + RAD *3 * i);
        circles.push(circle);
        anim_circles_start.push(anim_const("cx" + i, 200));
        anim_circles_enter.push(anim_interpolated(ease_cubic, "cx" + i, 300, 300));
        anim_circles_leave.push(anim_interpolated(ease_cubic, "cx" + i, 200, 300));

    }

    
    // const anim = anim_parallel_list(anim_circles_start)
    const anim = anim_parallel_list(anim_circles_start)
            .seq(anim_parallel_list(anim_circles_enter))
            .seq(anim_parallel_list(anim_circles_leave));


    return (async function*() {
        const TOTALFRAMES = 400;
        const DT = 1.0 / 30.0;
        while(true) {
            for(let n = 0; n < TOTALFRAMES; ++n) {
                const val = anim(n / TOTALFRAMES * anim.duration, {});
                for(var i = 0; i < NCIRCLES; ++i) {
                    circles[i].setAttribute("cx", val["cx" + i]);
                }
                await promiseDuration(DT);
                yield;
            }
        }
    })();
}



function animator_from_generator(gen) {
    gen.next().then(function() { 
        gen.next().then(animator_from_generator(gen));
    });
}

function foo() {
    const str = 
        'anim_circle = anim_const("cx", 100)\n' +
        '    .seq(anim_const("cr", 0))\n' +
        '    .seq(anim_interpolated(ease_cubic, "cr", /*val=*/10, /*time=*/3))\n' +
        '    .seq(anim_interpolated(ease_cubic, "cx", /*val=*/300, /*time=*/1)\n' +
        '        .par(anim_interpolated(ease_cubic, "cr", 70, 1)))\n' +
        '    .seq(anim_interpolated(ease_cubic, "cr", 0, 1)); plot()\n';

    // let data = new ClipboardItem({ "text/plain": str });
    navigator.clipboard.writeText(str);
}

function writeOldToClipboard() {
    const str = 
    'anim_circle = anim_const("cx", 100)\n' +
    '.seq(anim_const("cr", 0))\n' +
    '.seq(anim_interpolated(ease_cubic, "cr", /*val=*/10, /*time=*/3))\n' +
    '.seq(anim_interpolated(ease_cubic, "cx", /*val=*/300, /*time=*/1)\n' +
    '    .par(anim_interpolated(ease_cubic, "cr", 70, 1)))\n' +
    '.seq(anim_delay(/*time=*/3))\n' +
    '.seq(anim_interpolated(ease_cubic, "cx", 100, 1))\n' +
    '.seq(anim_delay(/*time=*/2))\n' +
    '.seq(anim_interpolated(ease_cubic, "cr", 0, 1)); plot()';

    // let data = new ClipboardItem({ "text/plain": str });
    navigator.clipboard.writeText(str);
}

function plot() {
    const container = document.getElementById("plot");

    // allow users to call plot()
    if (container.firstChild !== null) { container.firstChild.remove();}

    const width = 500;
    const height = 300;
    const RADIUS = 3;
    
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("width", width + "px");
    svg.setAttribute("height", height + "px");
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
    svg.setAttribute("font-family", "monospace");
    container.appendChild(svg);

    const NUMPOINTS = 100;
    var cxs = []; var crs = [];
    for(var i = 0; i < NUMPOINTS; ++i) {
        cxs.push(anim_circle(i/NUMPOINTS * anim_circle.duration).cx);
        crs.push(anim_circle(i/NUMPOINTS * anim_circle.duration).cr);
    }

    function data_to_plot_height(data) {
        var miny = data[0]; var maxy = data[0];
        for(var i = 0; i < NUMPOINTS; ++i) {
            miny = Math.min(data[i], miny); maxy = Math.max(data[i], maxy);
        }
        var plotys = [];
        for(var i = 0; i < NUMPOINTS; ++i) {
            plotys.push(height*0.2 - (data[i] - miny)/(maxy - miny) * height * 0.2);
        }
        return plotys;
    }

    cxs = data_to_plot_height(cxs);
    crs = data_to_plot_height(crs);

    const cxlbl =  document.createElementNS("http://www.w3.org/2000/svg", "text");
    cxlbl.setAttribute("x", 0);
    cxlbl.setAttribute("y", 30);
    cxlbl.setAttribute("fill", "#1a73e8");
    cxlbl.innerHTML = "cx";
    svg.appendChild(cxlbl);
    for(var i = 0; i < NUMPOINTS-1; ++i) {
        const l = document.createElementNS("http://www.w3.org/2000/svg", "line");
        l.setAttribute("style", "stroke:#1a73e8;stroke-width:2");
        l.setAttribute("x1", i /NUMPOINTS * width);
        l.setAttribute("x2", (i +1) /NUMPOINTS * width);
        l.setAttribute("y1", height * 0.1 + cxs[i])
        l.setAttribute("y2", height * 0.1 + cxs[i+1])
        svg.appendChild(l);
    }


    const crlbl =  document.createElementNS("http://www.w3.org/2000/svg", "text");
    crlbl.setAttribute("x", 0);
    crlbl.setAttribute("y", height/2 + 30);
    crlbl.setAttribute("fill", "#e91e63");
    crlbl.innerHTML = "cr";
    svg.appendChild(crlbl);
    for(var i = 0; i < NUMPOINTS-1; ++i) {
        const l = document.createElementNS("http://www.w3.org/2000/svg", "line");
        l.setAttribute("style", "stroke:#e91e63;stroke-width:2");
        l.setAttribute("x1", i /NUMPOINTS * width);
        l.setAttribute("x2", (i +1) /NUMPOINTS * width);
        l.setAttribute("y1", height*0.7 + crs[i])
        l.setAttribute("y2", height*0.7 + crs[i+1])
        svg.appendChild(l);
    }
}


function init_animations() {
    const anim1 = make_anim1_gen(document.getElementById("animation-1"))
    animator_from_generator(anim1);

    const anim2 = make_anim2_gen(document.getElementById("animation-2")); 
    animator_from_generator(anim2);

    const anim3 = make_anim3_gen(document.getElementById("animation-3")); 
    animator_from_generator(anim3);

    
    const anim31 = make_anim31_gen(document.getElementById("animation-31")); 
    animator_from_generator(anim31);

    const anim32 = make_anim32_gen(document.getElementById("animation-32")); 
    animator_from_generator(anim32);

    const anim33 = make_anim33_gen(document.getElementById("animation-33")); 
    animator_from_generator(anim33);

    const anim34 = make_anim34_gen(document.getElementById("animation-34")); 
    animator_from_generator(anim34);

    const anim_showoff_easing = make_anim_showoff_easing_gen(document.getElementById("animation-showoff-easing")); 
    animator_from_generator(anim_showoff_easing);


    const anim_interpolated = make_anim_interpolated_gen(document.getElementById("animation-interpolated")); 
    animator_from_generator(anim_interpolated);


    const anim_sequence = make_anim_sequence_gen(document.getElementById("animation-sequence")); 
    animator_from_generator(anim_sequence);


    const anim_parallel = make_anim_parallel_gen(document.getElementById("animation-parallel")); 
    animator_from_generator(anim_parallel);

    plot();
}                      
