/**
 * This example shows how to load a GEXF graph file (using the dedicated
 * graphology parser), and display it with some basic map features: Zoom in and
 * out buttons, reset zoom button, and a slider to increase or decrease the
 * quantity of labels displayed on screen.
 */

import Sigma from "sigma";
import Graph from "graphology";
import { parse } from "graphology-graphml/browser";
import * as layout from 'graphology-layout';
import forceAtlas2 from 'graphology-layout-forceatlas2';
import FA2Layout from "graphology-layout-forceatlas2/worker";

// Load external GraphML file:
fetch("./graph.xml")
  .then((res) => res.text())
  .then((text) => {
    // Parse GEXF string:
    const graph = parse(Graph, text, {addMissingNodes: true});

    const col : string[] = ['#003f5c', '#58508d', '#bc5090', '#ff6361', '#ffa600'];
    const colmap = new Map<string, string>();
    let coli = 0;

    // filter
    graph.mapNodes((node : string) => {
      const kind = graph.getNodeAttribute(node, "kind");
      if (! colmap.has(kind)) {
        colmap.set(kind, col[coli++]);
      }
      graph.setNodeAttribute(node, "color", colmap.get(kind))
      switch (kind) {
        case "participant":
          graph.dropNode(node);
          break;
        case "event":
          const time = graph.getNodeAttribute(node, "time");
          graph.updateNodeAttribute(node, "label", x => `${x} at ${time}`);
          graph.setNodeAttribute(node, "size", 2)
          break;
        default:
          graph.updateNodeAttribute(node, "label", x => `${x}: ${kind}`);
          graph.setNodeAttribute(node, "size", 4)
      }
    });

    graph.mapEdges((edge) => {
      switch (graph.getEdgeAttribute(edge, "kind")) {
        case "precedes":
          graph.dropEdge(edge);
          break;
        case "hash-link":
          graph.setEdgeAttribute(edge, "weight", 2)
          break;
        case "action":
          graph.setEdgeAttribute(edge, "weight", 1)
          break;
      }
    });

    // Retrieve some useful DOM elements:
    const container = document.getElementById("sigma-container") as HTMLElement;
    const zoomInBtn = document.getElementById("zoom-in") as HTMLButtonElement;
    const zoomOutBtn = document.getElementById("zoom-out") as HTMLButtonElement;
    const zoomResetBtn = document.getElementById("zoom-reset") as HTMLButtonElement;
    const labelsThresholdRange = document.getElementById("labels-threshold") as HTMLInputElement;


    // Layout
    layout.random.assign(graph);
    const sensibleSettings : any = forceAtlas2.inferSettings(graph);
    sensibleSettings["getEdgeWeight"] = "weight";
    const layout_worker = new FA2Layout(graph, {
      getEdgeWeight: "weight",
      settings: sensibleSettings
    });
    // layout_worker.start();
    setTimeout(_ => layout_worker.stop(), 10 * 1000)

    forceAtlas2.assign(graph, {iterations: 300, getEdgeWeight: "weight", settings: sensibleSettings});

    // Instanciate sigma:
    const renderer = new Sigma(graph, container);
    const camera = renderer.getCamera();

    // Bind zoom manipulation buttons
    zoomInBtn.addEventListener("click", () => {
      camera.animatedZoom({ duration: 600 });
    });
    zoomOutBtn.addEventListener("click", () => {
      camera.animatedUnzoom({ duration: 600 });
    });
    zoomResetBtn.addEventListener("click", () => {
      camera.animatedReset({ duration: 600 });
    });

    // Bind labels threshold to range input
    labelsThresholdRange.addEventListener("input", () => {
      renderer.setSetting("labelRenderedSizeThreshold", +labelsThresholdRange.value);
    });

    // Set proper range initial value:
    labelsThresholdRange.value = renderer.getSetting("labelRenderedSizeThreshold") + "";
  });
