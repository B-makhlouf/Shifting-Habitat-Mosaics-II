#!/usr/bin/env python3
"""Assemble one multi-panel figure per basin from the per-year PNGs.

Layout (one row per year):   [ YEAR ] [ production map ] [ density contour ]

Inputs are the PNGs written by:
  - 01_FullBasinRelativeProdMaps.R  -> Figures/01_ProdMaps/{Kusko,Yukon}/*_relprod.png
                                       (title-less maps with a large shared legend)
  - 02_ContourThreshnew.R           -> Figures/02_Contours/*_thresh0.7.png

Panels are used unmodified; only exterior whitespace is trimmed and each row is
scaled to a common height. A large bold year is drawn in the left margin so the
map side is labelled once the per-map titles have been removed.

USAGE (from project root):
  python3 Code/Analysis/combine_panels.py
Output: Figures/00_PubFigures/{Kuskokwim,Yukon}_MapsContours.{png,pdf}
"""
import os, glob
from PIL import Image, ImageDraw, ImageFont

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
FIG  = os.path.join(ROOT, "Figures")
OUT  = os.path.join(FIG, "00_PubFigures")
os.makedirs(OUT, exist_ok=True)

BASINS = {
    "Kuskokwim": [(yr, f"{FIG}/01_ProdMaps/Kusko/Kusko_{yr}_relprod.png",
                       f"{FIG}/02_Contours/Kusko_{yr}_thresh0.7.png")
                  for yr in (2017, 2018, 2019, 2020, 2021, 2022)],
    "Yukon":     [(yr, f"{FIG}/01_ProdMaps/Yukon/Yukon_Full_{yr}_relprod.png",
                       f"{FIG}/02_Contours/Yukon_{yr}_thresh0.7.png")
                  for yr in (2015, 2016, 2021)],
}

BG=(255,255,255); ROW_H=1000; MARGIN=70; YEAR_COL=230; COL_GUTTER=40
ROW_GUTTER=46; RULE_COLOR=(223,223,223); RULE_H=2; YEAR_COLOR=(26,26,26)

def _font(size):
    for p in ["/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf"] + \
             glob.glob("/usr/**/DejaVuSans-Bold.ttf", recursive=True):
        if os.path.exists(p):
            return ImageFont.truetype(p, size)
    try:                       # fall back to matplotlib's bundled font
        import matplotlib
        return ImageFont.truetype(os.path.join(
            matplotlib.get_data_path(), "fonts/ttf/DejaVuSans-Bold.ttf"), size)
    except Exception:
        return ImageFont.load_default()

def trim(im, thresh=248, pad=12):
    im=im.convert("RGB")
    mask=im.convert("L").point(lambda p:255 if p<thresh else 0)
    bbox=mask.getbbox()
    if bbox is None: return im
    l,t,r,b=bbox
    return im.crop((max(0,l-pad),max(0,t-pad),min(im.width,r+pad),min(im.height,b+pad)))

def scale_to_h(im,h):
    return im.resize((round(im.width*h/im.height),h), Image.LANCZOS)

def build(name, rows):
    panels=[(yr, scale_to_h(trim(Image.open(mp)),ROW_H),
                 scale_to_h(trim(Image.open(cp)),ROW_H)) for yr,mp,cp in rows]
    col1=max(m.width for _,m,c in panels); col2=max(c.width for _,m,c in panels)
    W=MARGIN*2+YEAR_COL+col1+COL_GUTTER+col2
    H=MARGIN*2+len(panels)*ROW_H+(len(panels)-1)*ROW_GUTTER
    canvas=Image.new("RGB",(W,H),BG); d=ImageDraw.Draw(canvas); yf=_font(96)
    y=MARGIN
    for i,(yr,m,c) in enumerate(panels):
        d.text((MARGIN+YEAR_COL//2, y+ROW_H//2), str(yr), font=yf,
               fill=YEAR_COLOR, anchor="mm")
        mx=MARGIN+YEAR_COL+(col1-m.width)//2
        cx=MARGIN+YEAR_COL+col1+COL_GUTTER+(col2-c.width)//2
        canvas.paste(m,(mx,y)); canvas.paste(c,(cx,y))
        y+=ROW_H
        if i<len(panels)-1:
            ry=y+ROW_GUTTER//2
            d.rectangle([MARGIN,ry,W-MARGIN,ry+RULE_H],fill=RULE_COLOR)
            y+=ROW_GUTTER
    png=f"{OUT}/{name}_MapsContours.png"; pdf=f"{OUT}/{name}_MapsContours.pdf"
    canvas.save(png,dpi=(200,200)); canvas.save(pdf,"PDF",resolution=200.0)
    print(f"{name}: {W}x{H} -> {png}")

if __name__ == "__main__":
    for n,r in BASINS.items(): build(n,r)
