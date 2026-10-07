// Explorer der logistischen Regression (Website-Version)
// Erwartet window.remissionData (Sitzungen, Remission) und window.logitStart (b0, b1).
(function () {
  const data = window.remissionData, start = window.logitStart;
  const svg = document.getElementById("ex-svg");
  const W = 900, H = 420, m = {l: 95, r: 20, t: 15, b: 60};
  const xMin = 0, xMax = 41, yMin = -0.12, yMax = 1.15;
  const sx = x => m.l + (x - xMin) / (xMax - xMin) * (W - m.l - m.r);
  const sy = y => H - m.b - (y - yMin) / (yMax - yMin) * (H - m.t - m.b);
  const ns = "http://www.w3.org/2000/svg";
  const el = (tag, attrs, parent) => {
    const e = document.createElementNS(ns, tag);
    for (const k in attrs) e.setAttribute(k, attrs[k]);
    (parent || svg).appendChild(e);
    return e;
  };
  const fmt = (v, d) => Number(v).toFixed(d).replace("-", "−");
  const logistic = (b0, b1, x) => 1 / (1 + Math.exp(-(b0 + b1 * x)));

  // Achsen und Gitter
  [0, 0.5, 1].forEach(v => {
    el("line", {x1: sx(xMin), x2: sx(xMax), y1: sy(v), y2: sy(v),
                stroke: "#e3e3e3", "stroke-width": 1.5});
    const t = el("text", {x: m.l - 10, y: sy(v) + 6, "text-anchor": "end",
                          "font-size": 18, fill: "#555"});
    t.textContent = v === 0 ? "0 = nein" : v === 1 ? "1 = ja" : "π = .5";
  });
  [0, 10, 20, 30, 40].forEach(v => {
    el("line", {x1: sx(v), x2: sx(v), y1: sy(yMin), y2: sy(yMax),
                stroke: "#f0f0f0", "stroke-width": 1.5});
    const t = el("text", {x: sx(v), y: H - m.b + 24, "text-anchor": "middle",
                          "font-size": 18, fill: "#555"});
    t.textContent = v;
  });
  const xl = el("text", {x: (m.l + W - m.r) / 2, y: H - 8,
                         "text-anchor": "middle", "font-size": 20});
  xl.textContent = "Anzahl Therapiesitzungen (x)";

  // Datenpunkte (deterministisches Jittern)
  const dots = el("g", {});
  data.forEach((d, i) => {
    const jit = ((i * 37) % 13 - 6) / 6 * 0.03;
    el("circle", {cx: sx(d.sitzungen), cy: sy(d.remission + jit), r: 4,
                  fill: "#595959", "fill-opacity": 0.4}, dots);
  });

  const pathFor = (b0, b1) => {
    let p = "";
    for (let k = 0; k <= 200; k++) {
      const x = xMin + k / 200 * (xMax - xMin);
      p += (k ? "L" : "M") + sx(x).toFixed(1) + "," + sy(logistic(b0, b1, x)).toFixed(1);
    }
    return p;
  };
  const refPath = el("path", {d: pathFor(start.b0, start.b1), fill: "none",
    stroke: "#0072B2", "stroke-width": 3, "stroke-dasharray": "8 6", "stroke-opacity": 0.55});
  const curPath = el("path", {fill: "none", stroke: "#009E73", "stroke-width": 4.5});
  const midLine = el("line", {stroke: "#009E73", "stroke-width": 2, "stroke-dasharray": "4 4"});
  const midText = el("text", {"font-size": 17, fill: "#009E73", stroke: "#fff",
                              "stroke-width": 5, "paint-order": "stroke"});
  const xLine = el("line", {stroke: "#D55E00", "stroke-width": 2});
  const xDot = el("circle", {r: 7, fill: "#fff", stroke: "#D55E00", "stroke-width": 3});

  // Eingaben: Regler und Zahlenfeld sind gekoppelt
  const pairs = {
    b0: [document.getElementById("ex-b0"), document.getElementById("ex-b0-num")],
    b1: [document.getElementById("ex-b1"), document.getElementById("ex-b1-num")],
    x:  [document.getElementById("ex-x"),  document.getElementById("ex-x-num")]
  };
  const val = k => +pairs[k][1].value;
  const showRef = document.getElementById("ex-ref");
  const showData = document.getElementById("ex-data");

  function update() {
    const b0 = val("b0"), b1 = val("b1"), x = val("x");
    if (![b0, b1, x].every(isFinite)) return;
    curPath.setAttribute("d", pathFor(b0, b1));
    refPath.style.display = showRef.checked ? "" : "none";
    dots.style.display = showData.checked ? "" : "none";

    const xm = b1 !== 0 ? -b0 / b1 : NaN;
    if (isFinite(xm) && xm >= xMin && xm <= xMax) {
      midLine.setAttribute("x1", sx(xm)); midLine.setAttribute("x2", sx(xm));
      midLine.setAttribute("y1", sy(0.5)); midLine.setAttribute("y2", sy(yMin));
      let right = b1 > 0;
      if (right && sx(xm) > W - m.r - 250) right = false;
      if (!right && sx(xm) < m.l + 250) right = true;
      midText.setAttribute("text-anchor", right ? "start" : "end");
      midText.setAttribute("x", sx(xm) + (right ? 10 : -10));
      midText.setAttribute("y", sy(0.5) + 24);
      midText.textContent = "π = .5 bei " + fmt(xm, 1) + " Sitzungen";
      midLine.style.display = midText.style.display = "";
    } else {
      midLine.style.display = midText.style.display = "none";
    }

    const lo = b0 + b1 * x, od = Math.exp(lo), p = od / (1 + od);
    xLine.setAttribute("x1", sx(x)); xLine.setAttribute("x2", sx(x));
    xLine.setAttribute("y1", sy(yMin)); xLine.setAttribute("y2", sy(p));
    xDot.setAttribute("cx", sx(x)); xDot.setAttribute("cy", sy(p));

    document.getElementById("ex-step-lo").textContent =
      fmt(b0, 2) + " + " + fmt(b1, 2) + " · " + fmt(x, 0) + " = " + fmt(lo, 2);
    document.getElementById("ex-step-od").textContent =
      "e^(" + fmt(lo, 2) + ") = " + fmt(od, 3);
    document.getElementById("ex-step-pi").textContent =
      fmt(od, 3) + " / (1 + " + fmt(od, 3) + ") = " + fmt(p, 3);
    document.getElementById("ex-x-label").textContent = fmt(x, 0);
  }

  Object.values(pairs).forEach(([range, num]) => {
    range.addEventListener("input", () => { num.value = range.value; update(); });
    num.addEventListener("input", () => { range.value = num.value; update(); });
  });
  [showRef, showData].forEach(e => e.addEventListener("change", update));

  function set(b0, b1) {
    pairs.b0[0].value = pairs.b0[1].value = b0;
    pairs.b1[0].value = pairs.b1[1].value = b1;
    update();
  }
  document.getElementById("ex-reset").addEventListener("click", () => set(start.b0, start.b1));
  set(start.b0, start.b1);
})();
