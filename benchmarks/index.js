import { init, classModule, propsModule, styleModule, eventListenersModule, h } from 'https://cdn.jsdelivr.net/npm/snabbdom/+esm'

const patch = init([
  classModule,
  propsModule,
  styleModule,
  eventListenersModule,
]);

document.addEventListener("DOMContentLoaded", function() {

  const container = document.getElementById("container");

  const n = 10000

  for (let j = 0; j < 0; j++) {

    console.log("running js-snabbdom...");
    let t0 = performance.now()
    for (let i = 0; i <= n; i++) {

      container.replaceChildren();
      const root = document.createElement("div");
      container.appendChild(root);

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

    let t1 = performance.now()
    console.log(`done: ${t1 - t0} ms`)

  }


  for (let j = 0; j < 100; j++) {

    console.log("running scala-js-snabbdom...");
    let t0 = performance.now()
    for (let i = 0; i <= n; i++) {

      container.replaceChildren();
      const root = document.createElement("div");
      container.appendChild(root);

      SnabbdomBenchmarks.benchmark1(root);

    }
    let t1 = performance.now()
    console.log(`done: ${t1 - t0} ms`)

  }
}

)
