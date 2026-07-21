// Force-directed network graph skeleton for pkgstats, built on D3 v7.
// Populated at runtime with a `{nodes, edges}` data object.
(function () {
  "use strict";

  function pkgstatsNetworkPlot (data, opts) {
    opts = opts || {};

    var nodes = (data.nodes || []).map (function (d) {
      return Object.assign ({}, d);
    });
    var nodeIds = new Set (nodes.map (function (d) {
      return d.id;
    }));
    // drop dangling edges, i.e. those referencing endpoints with no
    // corresponding node (d3-force throws if these are not removed), and
    // map "from"/"to" onto the "source"/"target" fields expected by
    // d3.forceLink
    var links = (data.edges || [])
      .filter (function (d) {
        return nodeIds.has (d.from) && nodeIds.has (d.to);
      })
      .map (function (d) {
        return Object.assign ({}, d, { source: d.from, target: d.to });
      });

    var container = document.getElementById ("pkgstats-network");
    var width = opts.width || container.clientWidth || window.innerWidth;
    var height = opts.height || window.innerHeight - 140;

    var groups = Array.from (new Set (nodes.map (function (d) {
      return d.group;
    }))).sort ();
    var color = d3.scaleOrdinal (groups, d3.schemeTableau10);

    var maxValue = d3.max (nodes, function (d) {
      return d.value;
    }) || 1;
    var radius = d3.scaleSqrt ().domain ([0, maxValue]).range ([4, 22]);

    var maxWidth = d3.max (links, function (d) {
      return d.width;
    }) || 1;
    var strokeWidth = d3.scaleLinear ()
      .domain ([0, maxWidth])
      .range ([0.6, 6]);

    var svg = d3.select ("#pkgstats-network")
      .append ("svg")
      .attr ("viewBox", [0, 0, width, height])
      .attr ("width", width)
      .attr ("height", height)
      .attr ("style", "max-width: 100%; height: auto;");

    var highlightColor = "#e6194b";

    svg.append ("defs").selectAll ("marker")
      .data ([
        { id: "pkgstats-arrow", fill: "#999" },
        { id: "pkgstats-arrow-highlight", fill: highlightColor },
      ])
      .join ("marker")
      .attr ("id", function (d) {
        return d.id;
      })
      .attr ("viewBox", "0 -5 10 10")
      .attr ("refX", 16)
      .attr ("refY", 0)
      .attr ("markerWidth", 5)
      .attr ("markerHeight", 5)
      .attr ("orient", "auto")
      .append ("path")
      .attr ("fill", function (d) {
        return d.fill;
      })
      .attr ("d", "M0,-5L10,0L0,5");

    var g = svg.append ("g");

    svg.on ("click", function () {
      selectedNodeId = null;
      updateHighlight ();
    });

    svg.call (d3.zoom ()
      .scaleExtent ([0.1, 8])
      .on ("zoom", function (event) {
        g.attr ("transform", event.transform);
      }));

    var simulation = d3.forceSimulation (nodes)
      .force ("link", d3.forceLink (links).id (function (d) {
        return d.id;
      }).distance (60).strength (0.3))
      .force ("charge", d3.forceManyBody ().strength (-180))
      .force ("center", d3.forceCenter (width / 2, height / 2))
      .force ("collide", d3.forceCollide ().radius (function (d) {
        return radius (d.value) + 4;
      }));

    var link = g.append ("g")
      .selectAll ("line")
      .data (links)
      .join ("line")
      .attr ("stroke", "#999")
      .attr ("stroke-opacity", 0.6)
      .attr ("stroke-width", function (d) {
        return strokeWidth (d.width);
      })
      .attr ("marker-end", "url(#pkgstats-arrow)");

    var selectedNodeId = null;

    function isConnected (d) {
      return selectedNodeId !== null &&
        (d.source.id === selectedNodeId || d.target.id === selectedNodeId);
    }

    function updateHighlight () {
      link
        .attr ("stroke", function (d) {
          return isConnected (d) ? highlightColor : "#999";
        })
        .attr ("stroke-opacity", function (d) {
          return selectedNodeId !== null && !isConnected (d) ? 0.15 : 0.6;
        })
        .attr ("stroke-width", function (d) {
          return isConnected (d) ?
            strokeWidth (d.width) + 4 :
            strokeWidth (d.width);
        })
        .attr ("marker-end", function (d) {
          return isConnected (d) ?
            "url(#pkgstats-arrow-highlight)" :
            "url(#pkgstats-arrow)";
        });
    }

    var tooltip = d3.select ("body").append ("div")
      .attr ("class", "pkgstats-tooltip")
      .style ("opacity", 0);

    var node = g.append ("g")
      .attr ("stroke", "#fff")
      .attr ("stroke-width", 1.5)
      .selectAll ("circle")
      .data (nodes)
      .join ("circle")
      .attr ("r", function (d) {
        return radius (d.value);
      })
      .attr ("fill", function (d) {
        return color (d.group);
      })
      .style ("cursor", "pointer")
      .call (drag (simulation))
      .on ("click", function (event, d) {
        event.stopPropagation ();
        selectedNodeId = (selectedNodeId === d.id) ? null : d.id;
        updateHighlight ();
      })
      .on ("mouseover", function (event, d) {
        tooltip.transition ().duration (100).style ("opacity", 0.95);
        tooltip.html (
          "<strong>" + d.label + "</strong><br>" +
          d.file
        )
          .style ("left", (event.pageX + 12) + "px")
          .style ("top", (event.pageY - 20) + "px");
      })
      .on ("mousemove", function (event) {
        tooltip.style ("left", (event.pageX + 12) + "px")
          .style ("top", (event.pageY - 20) + "px");
      })
      .on ("mouseout", function () {
        tooltip.transition ().duration (200).style ("opacity", 0);
      });

    var label = g.append ("g")
      .attr ("class", "pkgstats-node-labels")
      .selectAll ("text")
      .data (nodes)
      .join ("text")
      .attr ("class", "pkgstats-node-label")
      .text (function (d) {
        return d.label;
      });

    simulation.on ("tick", function () {
      link
        .attr ("x1", function (d) {
          return d.source.x;
        })
        .attr ("y1", function (d) {
          return d.source.y;
        })
        .attr ("x2", function (d) {
          return d.target.x;
        })
        .attr ("y2", function (d) {
          return d.target.y;
        });
      node
        .attr ("cx", function (d) {
          return d.x;
        })
        .attr ("cy", function (d) {
          return d.y;
        });
      label
        .attr ("x", function (d) {
          return d.x + radius (d.value) + 3;
        })
        .attr ("y", function (d) {
          return d.y;
        });
    });

    function drag (sim) {
      function dragstarted (event, d) {
        if (!event.active) sim.alphaTarget (0.3).restart ();
        d.fx = d.x;
        d.fy = d.y;
      }
      function dragged (event, d) {
        d.fx = event.x;
        d.fy = event.y;
      }
      function dragended (event, d) {
        if (!event.active) sim.alphaTarget (0);
        d.fx = null;
        d.fy = null;
      }
      return d3.drag ()
        .on ("start", dragstarted)
        .on ("drag", dragged)
        .on ("end", dragended);
    }

    if (opts.legend !== false) {
      var legend = d3.select ("#pkgstats-legend")
        .selectAll (".pkgstats-legend-item")
        .data (groups)
        .join ("div")
        .attr ("class", "pkgstats-legend-item");
      legend.append ("span")
        .attr ("class", "pkgstats-legend-swatch")
        .style ("background-color", function (d) {
          return color (d);
        });
      legend.append ("span").text (function (d) {
        return d;
      });
    }

    return { svg: svg, simulation: simulation };
  }

  window.pkgstatsNetworkPlot = pkgstatsNetworkPlot;
}) ();
