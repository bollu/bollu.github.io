<!--https://gomakethings.com/detecting-click-events-on-svgs-with-vanilla-js-event-delegation/-->
<!-- <!DOCTYPE html> -->
<!-- <meta charset='UTF-8'> -->
<!-- <html> -->
    
<!-- <body> -->
# Whalesong

<img src="./whalesong-picture.jpg">

I first heard that whales experience non-euclidian geometry at the splendid
['The non-Euclidian geometry of whales and ants'](https://static01.nyt.com/images/blogs/wordplay/Universe_in_Zero_Words_Copyright.pdf).
It set of a cascade of thoughts, along the lines of ['what is it like to be a bat'](https://warwick.ac.uk/fac/cross_fac/iatl/study/ugmodules/humananimalstudies/lectures/32/nagel_bat.pdf). 
I find it pleasurable to attempt to perceive the world as these beings might,
whose perception of the world is so non-Euclidian. So I set off to learn
more, and hopefully try and simulate this phenomena to get a better
intuition for it.

It's djikstras on a graph, where the edge lengths are obtained by discretizing
hyperbolic space so the single-source shortest paths discovered by djikstras is
a reasonable approximation of the real shortest paths in hyperbolic space Ah,
so the physics is that due to (i) snells law, (ii) changing refractive index
due to depth, sound "bends" inside water, leading to the shortest paths of
sound inside water to be those circular arcs plotted above. So you can consider
the paths that sound travels in space as hyperbolic space. If this is the only
sense organ whales have, then whales perceive the world as hyperbolic space.




<div id="whalesongGrid"></div>
<!-- </body> -->

    <script>   
            const whalesongGrid = document.getElementById("whalesongGrid");
            const width = 800;
            const height = 500;
            const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
            svg.setAttribute("width", width + "px");
            svg.setAttribute("height", height + "px");
            svg.setAttribute("viewBox", `0 0 ${width} ${height}`)
            svg.setAttribute("font-family", "monospace");

            const GRAY = "#607D8B";
            const BLUE = "#64B5F6";

            function create_circle(cx, cy, r, onclick) {
                const elem = document.createElementNS("http://www.w3.org/2000/svg", "circle");
                elem.setAttribute("cy", cy);
                elem.setAttribute("cx", cx);
                elem.setAttribute("r", r);
                elem.setAttribute("fill", BLUE);
                elem.addEventListener("click", () => { onclick(elem); });
                elem.addEventListener("mouseover", () => { onclick(elem); });

                svg.appendChild(elem);
                return elem;
            };

            function create_edge(x1, y1, x2, y2, w, click) {
                const elem = document.createElementNS("http://www.w3.org/2000/svg", "line");
                elem.setAttribute("x1", x1);
                elem.setAttribute("y1", y1);
                elem.setAttribute("x2", x2);
                elem.setAttribute("y2", y2);
                elem.setAttribute("stroke-width", w);
                elem.setAttribute("stroke", "#455A64");
                elem.addEventListener("click", () => { click(elem); });
                svg.appendChild(elem);
                return elem;
            }


            function fact(n) {
                let prod = 1; for(let i = 1; i <= n; ++i) { prod *= i; } return prod;
            }

            const X = 25;
            const Y = 7;
            const XY = X * Y;

            function ix(x, y) {
                console.assert(x >= 0);
                console.assert(y >= 0);
                console.assert(x < X);
                console.assert(y < Y);
                return x*Y+y;
            }

            const infty = 100000;
            adj = [];
            for(let i = 0; i < XY; i++) { 
                adj[i] = [];         
                for(let j = 0; j < XY; ++j) { adj[i][j] = infty; }
            }
            

            // const max_x_dist = fact(Y);
            const MAX_X_DIST = 1.0;
            const Y_DIST = MAX_X_DIST * 0.5;

            // (x, y) -> (x+1, y)
            for(let y = 0; y < Y; ++y) {
                for(let x = 0; x < X-1; ++x) {
                        adj[ix(x, y)][ix(x+1, y)] = MAX_X_DIST / (y+1);
                        adj[ix(x+1, y)][ix(x, y)] = MAX_X_DIST / (y+1);
                }
            }

            // (x, y) -> (x, y+1)
            for(let x = 0; x < X; ++x) {
                for(let y = 0; y < Y-1; ++y) {
                    adj[ix(x, y)][ix(x, y+1)] = Y_DIST;
                    adj[ix(x, y+1)][ix(x, y)] = Y_DIST;
                }
            }

            // (x, y) -> (x+1, y+1)
            for(let x = 0; x < X-1; ++x) {
                for(let y = 0; y < Y-1; ++y) {
                    // avg of (x, y) -> (x+1, y) and (x+1, y) -> (x+1, y+1)
                    const wt = 0.5 * (adj[ix(x, y)][ix(x+1, y)] + adj[ix(x+1, y)][ix(x+1, y+1)]);
                    adj[ix(x, y)][ix(x+1, y+1)] = wt
                    adj[ix(x+1, y+1)][ix(x, y)] = wt;
                }
            }

            // (x, y) -> (x+1, y-1) [/]
            for(let x = 0; x < X-1; ++x) {
                for(let y = 1; y < Y; ++y) {
                    // avg of (x, y) -> (x+1, y) and (x+1, y) -> (x+1, y-1)
                    const wt = 0.5 * (adj[ix(x, y)][ix(x+1, y)] + adj[ix(x+1, y)][ix(x+1, y-1)]);
                    adj[ix(x, y)][ix(x+1, y-1)] = wt
                    adj[ix(x+1, y-1)][ix(x, y)] = wt;
                }
            }


            console.table(adj);

            let dists = [];
            let parents = [];
            for(var i = 0; i < XY; ++i) { dists[i] = infty; parents[i] = -1; }
            dists[0] = 0;

            function single_source_shortest_path() {
                let q = [];
                q.push({ "cur": 0, "parent":0, "dist":0});

                while(q.length > 0) {
                    // ascending order by dist
                    q.sort(function (a, b) { return a["dist"] - b["dist"]; });
                    console.log("q: ");
                    console.table(q);
                    const v = q.shift(1);
                    console.log("v:"  , v);

                    if (parents[v["cur"]] != -1) { continue; }
                    // not an improvement on current known. skip.
                    // if (dists[v["cur"]] <= v["dist"]) {
                    //     continue;
                    // }

                    parents[v["cur"]] = v["parent"];
                    dists[v["cur"]] = v["dist"];

                    for(let i = 0; i < XY; ++i) {
                        const curdist = adj[v["cur"]][i]; 
                        if (curdist == infty) { continue; }
                        console.log("adjcent: " + v["cur"] + " -> " + i)
                        q.push({"cur":i, "parent":v["cur"], "dist": v["dist"] + curdist});
                    }
                }
                
                console.table(dists);
                console.table(parents);
            }

            single_source_shortest_path();
            
            
            const RADIUS = 10; const RECTFATNESS = 4;
            const X_PAD_LEFT = 2*RADIUS; const X_GAP = 30;
            const Y_PAD_TOP = 2*RADIUS; const Y_GAP = 30;
          
            
            let edges = []
            for (let i = 0; i < XY; ++i) {
                edges[i] = [];
            }

            // make edges (x, y) -> (x, y+1)
            for (let x = 0; x < X; ++x) {
                for(let y = 0; y < Y - 1; ++y) {
                    const e = 
                        create_edge(X_PAD_LEFT + x*X_GAP,
                                    Y_PAD_TOP + y*Y_GAP,
                                    X_PAD_LEFT + x*X_GAP,
                                    Y_PAD_TOP + (y+1)*Y_GAP,
                                    RECTFATNESS,
                                    function() {
                            console.log("edge (" + x + ", " + y + ") --(" + adj[ix(x, y)][ix(x, y+1)]  + ")--> " + "(" + (x+1) + ", " + y + ")");
                    }); 
                    edges[ix(x, y)][ix(x, y+1)] = e;
                    edges[ix(x, y+1)][ix(x, y)] = e;
                    e.setAttribute("opacity", 0.2);
                }
            }

            // make edges (x, y) -> (x+1, y)
            for (let x = 0; x < X-1; ++x) {
                for(let y = 0; y < Y; ++y) {
                    const e = 
                    create_edge(X_PAD_LEFT + x*X_GAP,
                                    Y_PAD_TOP + y*Y_GAP,
                                    X_PAD_LEFT + (x+1)*X_GAP,
                                    Y_PAD_TOP + y*Y_GAP,
                                    RECTFATNESS,
                                    function() {
                        console.log("edge (" + x + ", " + y + ") --(" + adj[ix(x, y)][ix(x+1, y)]  + ")--> " + "(" + (x+1) + ", " + y + ")");
                    });
                    edges[ix(x, y)][ix(x+1, y)] = e;
                    edges[ix(x+1, y)][ix(x, y)] = e;
                    e.setAttribute("opacity", 0.2);
                }
            }

            // make edges (x, y) -> (x+1, y+1)  [\]
            for (let x = 0; x < X-1; ++x) {
                for(let y = 0; y < Y-1; ++y) {
                    const e = 
                    create_edge(X_PAD_LEFT + x*X_GAP,
                                    Y_PAD_TOP + y*Y_GAP,
                                    X_PAD_LEFT + (x+1)*X_GAP,
                                    Y_PAD_TOP + (y+1)*Y_GAP,
                                    RECTFATNESS,
                                    function() {
                        console.log("edge (" + x + ", " + y + ") --(" + adj[ix(x, y)][ix(x+1, y)]  + ")--> " + "(" + (x+1) + ", " + y + ")");
                    });
                    edges[ix(x, y)][ix(x+1, y+1)] = e;
                    edges[ix(x+1, y+1)][ix(x, y)] = e;
                    e.setAttribute("opacity", 0.2);
                }
            }



            // make edges (x, y)  -> (x+1, y-1) [/]
            for (let x = 0; x < X-1; ++x) {
                for(let y = 1; y < Y; ++y) {
                    const e = 
                    create_edge(X_PAD_LEFT + x*X_GAP,
                                    Y_PAD_TOP + y*Y_GAP,
                                    X_PAD_LEFT + (x+1)*X_GAP,
                                    Y_PAD_TOP + (y-1)*Y_GAP,
                                    RECTFATNESS,
                                    function() {
                        console.log("edge (" + x + ", " + y + ") --(" + adj[ix(x, y)][ix(x+1, y)]  + ")--> " + "(" + (x+1) + ", " + y + ")");
                    });
                    edges[ix(x, y)][(x+1)*Y+(y-1)] = e;
                    edges[(x+1)*Y+(y-1)][ix(x, y)] = e;
                    e.setAttribute("opacity", 0.2);
                }
            }


            function hide_all_edges() {
                for(let i = 0; i < XY; i++) {
                    for(let j = 0; j < XY; j++) {
                        if (edges[i][j] !== undefined) {
                            edges[i][j].setAttribute("opacity", 0.2);
                        }
                    }
                
                }
            }

            let circles = [];
            for(let x = 0; x < X; ++x) {
                circles[x] = [];
                for(let y = 0; y < Y; ++y) {
                    circles[x][y] = create_circle(X_PAD_LEFT + x * X_GAP, Y_PAD_TOP + y * Y_GAP, RADIUS, function() {
                        console.log("circle (" + x + ", " + y + "): dist: " + dists[ix(x, y)]);
                        hide_all_edges();
                        let cur = x * Y + y;
                        while(cur != 0) {
                            let prev = parents[cur];
                            console.log("cur: ", cur,
                            "curx:", Math.floor(cur/Y), "cury: " , cur%Y,
                            "prevx: ", Math.floor(prev/Y), "prevy: ", prev%Y);

                            edges[prev][cur].setAttribute("opacity", 1.0); 
                            prev = cur;
                            cur = parents[cur];
                                                        
                        }
                    });
                    // circles[x][y].setAttribute("opacity", ((y + 1) / Y));
                }
            }
            whalesongGrid.appendChild(svg); 
    </script>
</html>
