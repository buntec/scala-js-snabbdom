import { init, attributesModule, classModule, propsModule, styleModule, eventListenersModule, datasetModule, h } from 'https://cdn.jsdelivr.net/npm/snabbdom/+esm'

const patch = init([
  attributesModule,
  classModule,
  propsModule,
  styleModule,
  eventListenersModule,
  datasetModule
]);

function benchmark1(root) {

  let vnode0 = h("div", {})
  let vnode1 = h("div", {}, [h("span", "1"), h("span", "2"), h("span", "3")]);
  let vnode2 = h("div", {}, [h("span", "2"), h("span", "3")]);
  let vnode3 = h("div", {}, [h("span", "3")]);
  let vnode4 = h("div", {}, [h("span", "2"), h("span", "3")]);
  let vnode5 = h("div", {}, [h("span", "1"), h("span", "2"), h("span", "3")]);
  let vnode6 = h("div", {}, [h("span", "0"), h("span", "1"), h("span", "2"), h("span", "3")]);
  let vnode7 = h("div", {}, [h("span", "0"), h("span", "1"), h("span", "2"), h("span", "3"), h("span", "4")]);

  patch(root, vnode0);
  patch(vnode0, vnode1);
  patch(vnode1, vnode2);
  patch(vnode2, vnode3);
  patch(vnode3, vnode4);
  patch(vnode4, vnode5);
  patch(vnode5, vnode6);
  patch(vnode6, vnode7);

}

function benchmark2(root) {

  let vnode0 = h("div", {})
  let vnode1 = h("div", {}, [h("span", { key: "1" }, "1"), h("span", { key: "2" }, "2"), h("span", { key: "3" }, "3")]);
  let vnode2 = h("div", {}, [h("span", { key: "2" }, "2a"), h("span", { key: "3" }, "3a")]);
  let vnode3 = h("div", {}, [h("span", { key: "3" }, "3b")]);
  let vnode4 = h("div", {}, [h("span", { key: "2" }, "2c"), h("span", { key: "3" }, "3c")]);
  let vnode5 = h("div", {}, [h("span", { key: "1" }, "1d"), h("span", { key: "2" }, "2d"), h("span", { key: "3" }, "3d"), h("span", "b")]);
  let vnode6 = h("div", {}, [h("span", { key: "1" }, "1e"), h("span", { key: "3" }, "3e"), h("span", { key: "2" }, "2e")]);
  let vnode7 = h("div", {}, [h("span", "a"), h("span", "b"), h("span", { key: "1" }, "1f"), h("span", { key: "2f" }, "2"), h("span", { key: "3f" }, "3")]);
  let vnode8 = h("div", {}, [h("span", { key: "1" }, "1g"), h("span", { key: "2" }, "2g"), h("span", { key: "3" }, "3g"), h("span", { key: "4" }, "4g")]);

  patch(root, vnode0);
  patch(vnode0, vnode1);
  patch(vnode1, vnode2);
  patch(vnode2, vnode3);
  patch(vnode3, vnode4);
  patch(vnode4, vnode5);
  patch(vnode5, vnode6);
  patch(vnode6, vnode7);
  patch(vnode7, vnode8);

}

function run(name, fn, nRuns) {

  const container = document.getElementById("container");
  console.log(`running ${name}...`);
  const t0 = performance.now()
  for (let i = 0; i <= nRuns; i++) {
    const root = document.createElement("div");
    container.appendChild(root);
    fn(root);
    container.replaceChildren();
  }
  const t1 = performance.now()
  console.log(`done: ${t1 - t0} ms`)

}

document.addEventListener("DOMContentLoaded", function() {

  const n = 10000 // number of runs

  run("snabbdom - benchmark1", benchmark1, n)
  run("sjs-snabbdom - benchmark1", (root) => SnabbdomBenchmarks.benchmark1(root), n)
  run("snabbdom - benchmark2", benchmark2, n)
  run("sjs-snabbdom - benchmark2", (root) => SnabbdomBenchmarks.benchmark2(root), n)

}

)
