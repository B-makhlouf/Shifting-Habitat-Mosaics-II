#!/usr/bin/env python3
"""
Build interactive QC maps of portfolio-effect spatial UNITS.

A "unit" at stream order k = a maximal order-k tributary (the order-k reach at
its mouth) PLUS its entire upstream catchment -- the same unit definition used
in PortfolioCV.R. Each boxplot point in CV_by_streamorder is one such unit.

For QC we let the user pick a stream order; every reach then recolours by which
order-k unit it belongs to (units are nested across orders, so a reach belongs
to one unit per order >= its own order). One self-contained Leaflet HTML per
basin, no external data files.
"""
import os, json
import numpy as np
import pandas as pd
import geopandas as gpd

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
OUT  = os.path.join(REPO, "Figures", "PortfolioEffect")
os.makedirs(OUT, exist_ok=True)

BAS = {
    "Kusko": dict(
        topo=f"{REPO}/Data/UpstreamReaches/kusko_upstream_topology.csv",
        shp=f"{REPO}/Data/Spatial Data/AnalysisShapefiles/Kusko_edges_geomorphAdded.shp",
        units=f"{REPO}/Outputs/PortfolioEffect/Kusko_unit_CVs.csv"),
    "Yukon": dict(
        topo=f"{REPO}/Data/UpstreamReaches/yukon_upstream_topology.csv",
        shp=f"{REPO}/Data/Spatial Data/AnalysisShapefiles/Yukon_edges_geomorphAdded.shp",
        units=f"{REPO}/Outputs/PortfolioEffect/Yukon_unit_CVs.csv"),
}
SIMPLIFY_M = 150   # geometry simplification tolerance (Albers metres)

# Spatial restriction: analyse only the parts upstream of the order-K0 tributaries
# so the two basins cover comparable sub-catchments (Kusko order 6, Yukon order 8).
# Set a value to None to map the whole basin.
RESTRICT = {"Kusko": 6, "Yukon": 8}


def build(name, cfg):
    topo = pd.read_csv(cfg["topo"])
    topo["reachid"] = topo.reachid.round().astype(int)
    topo["down_reachid"] = topo.down_reachid.round().astype(int)

    order_by = dict(zip(topo.reachid, topo.strahler))
    nup_by   = dict(zip(topo.reachid, topo.n_upstream))
    isout    = dict(zip(topo.reachid, topo.is_outlet))
    reach_set = set(topo.reachid)
    down_map = dict(zip(topo.reachid, topo.down_reachid))

    # children: parent reachid -> list of upstream child reachids
    children = {}
    for r, d in zip(topo.reachid.values, topo.down_reachid.values):
        children.setdefault(d, []).append(r)

    def is_mouth(r):
        if isout.get(r, 0) == 1:
            return True
        d = down_map[r]
        if d not in reach_set:
            return True
        return order_by[d] > order_by[r]

    # restriction domain: all reaches upstream of (and including) an order-K0
    # tributary mouth. If K0 is None, the whole basin is the domain.
    K0 = RESTRICT.get(name)
    if K0 is None:
        domain = set(topo.reachid)
    else:
        domain = set()
        for m in (r for r in topo.reachid if order_by[r] == K0 and is_mouth(r)):
            stack = [int(m)]
            while stack:
                r = stack.pop()
                domain.add(r)
                for c in children.get(r, []):
                    stack.append(c)

    # STRICT NESTING: keep a unit only if its containing unit (the next mouth
    # downstream) is exactly one order higher, recursively up to the order-K0
    # sub-basin. Drops "skip" tributaries; preserves the branching tree (an
    # order-6 unit still holds several nested order-5 units, etc.).
    mouth_set = set(int(r) for r in topo.reachid if is_mouth(r))
    K0_eff = int(topo.strahler.max()) if K0 is None else K0
    def containing(m):
        r = down_map[m]
        while r in reach_set:
            if r in mouth_set:
                return r
            r = down_map[r]
        return None
    _nest = {}
    def is_nested(m):
        if m in _nest:
            return _nest[m]
        k = order_by[m]
        if k >= K0_eff:
            _nest[m] = (m in domain)
            return _nest[m]
        p = containing(m)
        _nest[m] = (p is not None) and (order_by[p] == k + 1) and is_nested(p)
        return _nest[m]
    nested_set = set(m for m in mouth_set if m in domain and is_nested(m))

    # orders shown = orders with at least one ESTIMABLE unit (CV not NA) that
    # lies inside the domain -- read from the same per-unit table the boxplot
    # uses (restricted to the domain) so map and boxplot stay consistent.
    uc = pd.read_csv(cfg["units"])
    uc = uc[uc.reachid.notna()].copy()
    uc["reachid"] = uc.reachid.round().astype(int)
    uc = uc[uc.reachid.isin(nested_set)]
    orders = sorted(int(o) for o in uc.loc[uc["cv"].notna(), "stream_order"].unique())
    n_estimable = uc[uc["cv"].notna()].groupby("stream_order").size().to_dict()

    # per-reach unit assignment: unit_idx[reach][k] = colour index, mouth[reach][k]
    unit_idx = {r: {} for r in topo.reachid}
    unit_mouth = {r: {} for r in topo.reachid}
    mouth_size = {}
    n_drawn = {}
    for k in orders:
        mouths_k = sorted(int(r) for r in topo.reachid
                          if order_by[r] == k and is_mouth(r) and int(r) in nested_set)
        n_drawn[k] = len(mouths_k)
        for ci, m in enumerate(mouths_k):
            m = int(m)
            mouth_size[m] = int(nup_by[m]) + 1
            stack = [m]                      # BFS upstream subtree of mouth m
            while stack:
                r = stack.pop()
                unit_idx[r][k] = ci
                unit_mouth[r][k] = m
                for c in children.get(r, []):
                    stack.append(c)

    # geometry
    g = gpd.read_file(cfg["shp"])[["reachid", "geometry"]].copy()
    g["reachid"] = g.reachid.round().astype(int)
    g["geometry"] = g.geometry.simplify(SIMPLIFY_M, preserve_topology=False)
    g = g.to_crs(4326)

    feats = []
    bounds = [180, 90, -180, -90]
    for rid, geom in zip(g.reachid.values, g.geometry.values):
        if geom is None or geom.is_empty or int(rid) not in domain:
            continue
        coords = [[round(x, 5), round(y, 5)] for x, y in geom.coords]
        for x, y in coords:
            bounds[0] = min(bounds[0], x); bounds[1] = min(bounds[1], y)
            bounds[2] = max(bounds[2], x); bounds[3] = max(bounds[3], y)
        uk = [int(unit_idx[rid].get(k, -1)) for k in orders]
        mk = [int(unit_mouth[rid].get(k, 0)) for k in orders]
        feats.append([int(rid), int(order_by[rid]), coords, uk, mk])

    data = dict(
        basin=name, orders=orders, restrict=(None if K0 is None else int(K0)),
        n_units={str(k): int(n_drawn[k]) for k in orders},
        n_estimable={str(k): int(n_estimable.get(k, 0)) for k in orders},
        mouth_size=mouth_size, bounds=bounds, feats=feats,
    )
    html = HTML.replace("__DATA__", json.dumps(data, separators=(",", ":")))
    suffix = "_subbasin" if K0 is not None else ""
    path = os.path.join(OUT, f"{name}_unit_QC_map{suffix}.html")
    with open(path, "w") as f:
        f.write(html)
    print(f"{name}: {len(feats)} reaches, orders {orders} -> {path} "
          f"({os.path.getsize(path)/1e6:.1f} MB)")


HTML = r"""<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Unit QC map</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<style>
  html,body{margin:0;height:100%;font-family:system-ui,Arial,sans-serif}
  #map{position:absolute;top:0;bottom:0;left:0;right:0}
  #panel{position:absolute;top:10px;right:10px;z-index:1000;background:rgba(255,255,255,.94);
    padding:12px 14px;border-radius:8px;box-shadow:0 1px 6px rgba(0,0,0,.3);max-width:260px;font-size:13px}
  #panel h3{margin:0 0 6px;font-size:14px}
  #panel .sub{color:#555;font-size:11.5px;margin-bottom:8px;line-height:1.35}
  .ord{display:inline-block;margin:2px 3px;padding:4px 9px;border:1px solid #bbb;border-radius:5px;
    cursor:pointer;background:#fff}
  .ord.sel{background:#222;color:#fff;border-color:#222}
  #info{margin-top:8px;font-size:12px;color:#222;min-height:34px;border-top:1px solid #ddd;padding-top:6px}
  .swatch{display:inline-block;width:11px;height:11px;border-radius:2px;margin-right:5px;vertical-align:-1px}
</style></head>
<body>
<div id="map"></div>
<div id="panel">
  <h3 id="ttl"></h3>
  <div class="sub">Pick a stream order. Each <b>maximal order-k tributary + its upstream
   catchment</b> (one boxplot point) gets its own colour. Grey = reaches not contained in
   any order-k unit (they drain past order k). Button shows units drawn; some have
   no fish production and so are dropped from the boxplot. Hover a reach for details.</div>
  <div id="orders"></div>
  <div id="info">Hover over a reach&hellip;</div>
</div>
<script>
const D = __DATA__;
const PAL = ["#e6194B","#3cb44b","#4363d8","#f58231","#911eb4","#42d4f4","#f032e6",
 "#bfef45","#fabed4","#469990","#dcbeff","#9A6324","#800000","#aaffc3","#808000",
 "#ffd8b1","#000075","#a9a9a9","#ff6db6","#006ddb","#b66dff","#22cf22","#990000",
 "#004949","#924900"];
const map = L.map('map',{preferCanvas:true});
L.tileLayer('https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
  {attribution:'&copy; OpenStreetMap, &copy; CARTO',maxZoom:14,subdomains:'abcd'}).addTo(map);
map.fitBounds([[D.bounds[1],D.bounds[0]],[D.bounds[3],D.bounds[2]]]);

let curOrder = D.orders[0];
const oi = k => D.orders.indexOf(k);
function colorFor(f){
  const idx = f[3][oi(curOrder)];
  if(idx < 0) return null;
  return PAL[idx % PAL.length];
}
const layers = [];
for(const f of D.feats){
  const latlngs = f[2].map(c=>[c[1],c[0]]);
  const pl = L.polyline(latlngs,{weight:1.4,opacity:0.95}).addTo(map);
  pl._f = f;
  pl.on('mouseover',e=>{
    const f=e.target._f, idx=f[3][oi(curOrder)], m=f[4][oi(curOrder)];
    e.target.setStyle({weight:4});
    let html = `reach <b>${f[0]}</b> &middot; Strahler order ${f[1]}<br>`;
    if(idx<0){ html += `<span style="color:#777">not in any order-${curOrder} unit</span>`; }
    else { const sz=D.mouth_size[m]||'?';
      html += `<span class="swatch" style="background:${PAL[idx%PAL.length]}"></span>`+
              `order-${curOrder} unit: mouth <b>${m}</b><br>unit size: ${sz} reaches`; }
    document.getElementById('info').innerHTML = html;
  });
  pl.on('mouseout',e=>{restyle(e.target);});
  layers.push(pl);
}
function restyle(pl){
  const c = colorFor(pl._f);
  if(c===null) pl.setStyle({color:'#d9d9d9',weight:0.7,opacity:0.6});
  else pl.setStyle({color:c,weight:1.4,opacity:0.95});
}
function redraw(){ for(const pl of layers) restyle(pl); }
const ob = document.getElementById('orders');
D.orders.forEach(k=>{
  const b=document.createElement('span'); b.className='ord'; b.dataset.k=k;
  b.textContent = `${k}  (${D.n_units[k]})`;
  b.title = `${D.n_units[k]} order-${k} units drawn; ${D.n_estimable[k]} have production (= boxplot points)`;
  b.onclick=()=>{curOrder=k; document.querySelectorAll('.ord').forEach(x=>x.classList.remove('sel'));
    b.classList.add('sel'); redraw();};
  ob.appendChild(b);
});
document.querySelector('.ord').classList.add('sel');
document.getElementById('ttl').textContent = D.restrict
  ? `${D.basin}: units upstream of order-${D.restrict} tributaries`
  : `${D.basin}: portfolio units QC`;
document.title = `${D.basin} unit QC map`;
redraw();
</script>
</body></html>"""

for nm, cfg in BAS.items():
    build(nm, cfg)
