import os
import argparse
from matplotlib import rcParams
import matplotlib.font_manager as fm
from osgeo import gdal
import datetime
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
from matplotlib.collections import PatchCollection
import numpy as np
from geopy import distance
import sys
from adjustText import adjust_text
from osgeo import ogr
from osgeo import osr
parser=argparse.ArgumentParser()
parser.add_argument('--StormName',type=str,required=True)
parser.add_argument('--EventID',type=str,required=True)
parser.add_argument('--Landfall',type=str,required=True)
args=parser.parse_args()

spath = 'D:/03_JapanAER/IN_SEASON_ANALYSIS/'
trackpath = 'D:/03_JapanAER/IN_SEASON_ANALYSIS/Tableau/'

os.chdir(spath)
rcParams['font.sans-serif'] = "Arial"
rcParams['font.family'] = "sans-serif"
path = os.path.join('fonts', 'HelveticaNowText.ttf')
prop = fm.FontProperties(fname=path)
path_bold = os.path.join('fonts', 'HelveticaNowTextBold.ttf')
prop_bold = fm.FontProperties(fname=path_bold)

ds = gdal.Open('raster/AMEDAS202214_gust.tif')
proj=ds.GetProjection()
source = osr.SpatialReference()
source.ImportFromWkt(ds.GetProjection())
target = osr.SpatialReference()
target.ImportFromEPSG(4326)
transform = osr.CoordinateTransformation(source, target)
# transform.TransformPoint(gtrs[0], gtrs[3])
dsReprj = gdal.Warp('AMEDAS202214_gust.tif', ds, dstSRS="EPSG:4326") 
data = np.flipud(dsReprj.ReadAsArray().astype(float))
data[data < 20] = np.nan
gtrs = ds.GetGeoTransform()
gtrss=dsReprj.GetGeoTransform()
xsize = ds.RasterXSize
ysize = ds.RasterYSize
xxsize = dsReprj.RasterXSize
yysize = dsReprj.RasterYSize

trackdata = os.path.join(trackpath, 'trackfile.txt')
with open(trackdata) as trk:
    best = trk.readlines()
    trk.close()

 
del best[-1]
btkx = [float((best[t].split(' ')[5]).strip('E')) for t in range(1, len(best))]
btky = [float((best[t].split(' ')[4]).strip('N')) for t in range(1, len(best))]
yy = [y[2:] for y in [best[t].split(' ')[2] for t in range(1, len(best))]]
hh = [h[0:2] for h in [best[t].split(' ')[3] for t in range(1, len(best))]]
ss = ["{}{}".format(yy_, hh_) for yy_, hh_ in zip(yy, hh)]
dtimes = [datetime.datetime.strptime(str(ss[j]), '%m%d%H') for j in range(len(ss))]
mon = [datetime.datetime.strptime(str(ss[j]), '%m%d%H').strftime("%b").ljust(4,' ') for j in range(len(ss))]
dd = [d[2:].ljust(2,' ') for d in yy]
diffs = ["{}{}{}".format(dd_,mon_,hh_)+"Z"  for dd_, mon_, hh_ in zip(dd,mon,hh)]
idx = ss.index(args.Landfall)
idx_min, idx_max = [idx-6, idx+3]
latmin = btky[idx] + gtrss[5]*(yysize/40)
latmax = btky[idx] - gtrss[5]*(yysize/4)
lonmin = btkx[idx] - gtrss[1]*(xxsize/30)
lonmax = btkx[idx] + gtrss[1]*(xxsize/4)
latavg = (latmax + latmin) / 2
lonavg = (lonmax + lonmin) / 2
map_width = distance.distance((latavg, lonmin), (latavg, lonmax)).meters * 1.2
map_height = distance.distance((latmin, lonavg), (latmax, lonavg)).meters * 1.2
map_ratio = map_height / map_width
if map_ratio < 0.85:
    map_height = map_width * 0.85    


if map_ratio > 1.17647:
    map_width = map_height * 0.85


if max([map_width, map_height]) < 500000:
    map_width, map_height = [500000, 500000]

m = Basemap(width=map_width, height=map_height, resolution='h', area_thresh=100,
                projection='lcc', lat_1=latmin, lat_2=latmax, lat_0=latavg, lon_0=lonavg)
                
lon = np.arange(gtrss[0] + gtrss[1] / 2, gtrss[0] + gtrss[1] * xxsize, gtrss[1])
lat = np.arange(gtrss[3] + gtrss[5] * yysize - gtrss[5] / 2, gtrss[3], -gtrss[5])
lon2d, lat2d = np.meshgrid(lon, lat)
x, y = m(lon2d, lat2d)

figwidth = 6.2
fig = plt.figure(clear=True, figsize=(figwidth, figwidth * 1.1 * m.aspect))
ax = fig.add_axes([0, 0.09, 1, 0.91])
ax.axis('off')
m.fillcontinents(color='#e6e6e6', lake_color='#f2f2f2', zorder=1)
intervals = [20, 25, 30, 35, 40, 50, 60, 70]
palette = ['#FFE7B9', '#FFD27C', '#FFA600', '#F25D00', '#EA2238', '#D10058', '#A70070']                
cs = m.contourf(x, y, data, colors=palette, levels=intervals, extend='both', zorder=2, vmin=45, vmax=200)
states = m.readshapefile(r"Geodata\gadm36_JPN_1", 'boundaries', zorder=5, color='Gray', linewidth=0.5)
pref_keys = [item['NAME_1'] for item in m.boundaries_info]
if m.xmax < 1000000:
    m.drawcoastlines(linewidth=0.25, color='black')


bestx, besty = m(btkx, btky)
ref_lat, ref_lon = [btky[idx], btkx[idx]]
kxrv, kyrv, txrv, tyrv = [btkx[idx_min:idx_max], btky[idx_min:idx_max], bestx[idx_min:idx_max], besty[idx_min:idx_max]]
plt.plot(txrv, tyrv, '--', linewidth=1.5, markersize=0, zorder=8, color='#262836')
best_points = []
for n in range(len(kxrv)):
    best_points.append(plt.plot(txrv[n], tyrv[n], 'o', markersize=6, zorder=8, color='black')[0])


ofcl_labels = []
for q in range(len(diffs[idx_min:idx_max])):
    if m.lonmin < kxrv[q] < m.lonmax:
        if m.latmin < kyrv[q] < m.latmax:
            ofcl_labels.append(
                plt.text(txrv[q] + m.xmax / 60, tyrv[q], diffs[idx_min:idx_max][q], zorder=11, fontproperties=prop_bold, size=9.5,
                             color='black', verticalalignment='center', horizontalalignment='left'))
                             

adjust_text(ofcl_labels, autoalign='x', va='center', add_objects=best_points)    
cax = fig.add_axes([0.25, 0.05, 0.5, 0.015])
cax.axis('off')
cbar = plt.colorbar(cs, cax=cax, orientation='horizontal', spacing='proportional')
cbar.outline.set_linewidth(0.2)
cax.axis('on')
cbar.set_label('3-sec Wind gust (m/s)', fontproperties=prop, fontsize=9, fontname="Roboto", labelpad=8)
cbar.ax.tick_params(labelsize=9, pad=5, length=2, width=0.2)
fname = os.path.join('maps', args.StormName, left(args.EventID,4),'_footprint.png')
plt.savefig(spath+fname, dpi=150, bbox_inches='tight', pad_inches=0.0)
