from rasterio.merge import merge
import rasterio
import pandas as pd
import pystac_client
import geopandas as gpd
import requests
import os
from pathlib import Path

# 1. Setup download directory
# Change to your preferred location
download_dir = Path("/Users/benjaminmakhlouf/Spatial Data/DEMS/YukonARCTICDEM")
download_dir.mkdir(exist_ok=True)

# 2. Load your Yukon River Basin shapefile
yukon_shapefile_path = "/Users/benjaminmakhlouf/Desktop/Research/isoscapes_new/Yukon/For_Sean/Yuk_Mrg_alb.shp"
yukon_gdf = gpd.read_file(yukon_shapefile_path)
yukon_gdf = yukon_gdf.to_crs("EPSG:4326")

bbox = yukon_gdf.total_bounds
print(f"Bounding box: {bbox}")

# 3. Connect to PGC STAC API
cat = pystac_client.Client.open("https://stac.pgc.umn.edu/api/v1/")

# 4. Search for ArcticDEM 10m mosaics
mosaic_search = cat.search(
    collections=["arcticdem-mosaics-v4.1-10m"],
    bbox=bbox
)

mosaic_items = list(mosaic_search.items())
print(f"Found {len(mosaic_items)} mosaic tiles to download")

# 5. Create a list of download URLs and save metadata
tile_info = []
for item in mosaic_items:
    tile_id = item.properties.get('pgc:tile', item.id)
    dem_url = item.assets['dem'].href  # Direct link to the DEM GeoTIFF

    tile_info.append({
        'tile_id': tile_id,
        'dem_url': dem_url,
        'filename': f"{tile_id}_dem.tif"
    })
    print(f"Tile: {tile_id}")
    print(f"  URL: {dem_url}")

# 6. Save the download list to CSV (useful for tracking)
df = pd.DataFrame(tile_info)
df.to_csv(download_dir / "tile_download_list.csv", index=False)
print(f"\nSaved download list to: {download_dir / 'tile_download_list.csv'}")

# 7. Download function with error handling


def download_file(url, output_path):
    """Download file with progress tracking"""
    try:
        response = requests.get(url, stream=True)
        response.raise_for_status()

        total_size = int(response.headers.get('content-length', 0))

        with open(output_path, 'wb') as f:
            if total_size == 0:
                f.write(response.content)
            else:
                downloaded = 0
                for chunk in response.iter_content(chunk_size=8192):
                    f.write(chunk)
                    downloaded += len(chunk)
                    # Simple progress indicator
                    progress = (downloaded / total_size) * 100
                    print(f"\r  Progress: {progress:.1f}%", end='')
        print()  # New line after download
        return True
    except Exception as e:
        print(f"\n  ERROR: {e}")
        return False


# 8. Download all tiles
print(f"\nStarting download of {len(tile_info)} tiles...")
successful = 0
failed = []

for i, tile in enumerate(tile_info, 1):
    output_path = download_dir / tile['filename']

    # Skip if already downloaded
    if output_path.exists():
        print(f"[{i}/{len(tile_info)}] Skipping {tile['tile_id']} (already exists)")
        successful += 1
        continue

    print(f"[{i}/{len(tile_info)}] Downloading {tile['tile_id']}...")

    if download_file(tile['dem_url'], output_path):
        successful += 1
        print(f"  ✓ Saved to: {output_path}")
    else:
        failed.append(tile['tile_id'])
        print(f"  ✗ Failed to download {tile['tile_id']}")

# ... (after your download section completes) ...

# 10. Mosaic the downloaded tiles
print("\nMosaicking tiles...")

dem_files = list(download_dir.glob("*_dem.tif"))
src_files = [rasterio.open(fp) for fp in dem_files]

mosaic, out_trans = merge(src_files)

out_meta = src_files[0].meta.copy()
out_meta.update({
    "driver": "GTiff",
    "height": mosaic.shape[1],
    "width": mosaic.shape[2],
    "transform": out_trans,
    "compress": "lzw"
})

output_mosaic = download_dir / "yukon_basin_mosaic_10m.tif"
with rasterio.open(output_mosaic, "w", **out_meta) as dest:
    dest.write(mosaic)

for src in src_files:
    src.close()

print(f"✓ Mosaic saved to: {output_mosaic}")
