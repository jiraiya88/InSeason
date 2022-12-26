import os
os.environ["PROJ_LIB"] = "C:\\Miniconda3\\Library\\share\\basemap"
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.font_manager as fm
import datetime
import pandas as pd
import sys
from geopy import distance
from matplotlib import patches
from datetime import timedelta

# use in command line:
# > python Plot_Track.py Cmbd_2018_JEBI_2018083000.csv ../fonts/

# inputs are 
# sys.argv[0]: name of the script
# sys.argv[1]: name (and path) of the text file with data to be plotted
# sys.argv[2]: path to a folder with font information

# classic pallette
cat_colors = {0: [77 / 255, 168 / 255, 195 / 255],
              1: [178 / 255, 178 / 255, 178 / 255],
              2: [240 / 255, 170 / 255, 0 / 255],
              3: [225 / 255, 27 / 255, 34 / 255],
              4: [110 / 255, 38 / 255, 123 / 255],
              5: [51 / 255, 51 / 255, 51 / 255]}

# 2021 rebranded pallette
cat_colors = {0: "#CDDBDE",
              1: "#ACC0C3",
              2: "#82939A",
              3: "#0055A8",
              4: "#EB0017",
              5: "#6E027F"}

def conv_Lon_positive(Longitude):
    if Longitude<0:
        Longitude = 360 + Longitude
    return(Longitude)

def track_map(filename, fsttime,ofilename):
    """
    creates a map of HRES and ENS tracks from csv input
    :param filename: csv input, columns: ['EventID', 'Snap', 'ISO_time', 'Longitude', 'Latitude', 'CP_hPa', 'Cat']
    :param fsttime: currently in a '2020-09-03 00:00:00' format
    :return: saves as png (name based on csv input filename)
    """

    df = pd.read_csv(filename)
    forecast_time = datetime.datetime.strptime(fsttime, '%Y-%m-%d %H:%M:%S')
    
    # adapt negative longitudes to format exceeding 180
    df.loc[df['lon']<0,'lon'] = 360 + df.loc[df['lon']<0,'lon']
    
    # define storm category based on MSLP

    Cat = []
    for pres in df['mslp']:
        if pres >= 990:
            cat = '0'
        elif pres >= 975:
            cat = '1'
        elif pres >= 960:
            cat = '2'
        elif pres >= 945:
            cat = '3'
        elif pres >= 925:
            cat = '4'
        else:
            cat = '5'
        Cat.append(cat)

    df['Cat'] = Cat

    # define map extent

    # center at forecast only
    #latmin = min(df[pd.to_datetime(df['DateTimeUTC'],format='%Y-%m-%d %H:%M:%S') + timedelta(hours=12,minutes=0)>=forecast_time]['lat'])
    latmin = min(df[pd.to_datetime(df['DateTimeUTC'],format='%Y-%m-%dT%H:%M:%SZ') + timedelta(hours=12,minutes=0)>=forecast_time]['lat'])
    # show range at least at latitude of Tokyo
    #latmax = max(36,max(df[pd.to_datetime(df['DateTimeUTC'],format='%Y-%m-%d %H:%M:%S') + timedelta(hours=12)>=forecast_time]['lat']))
    latmax = max(36,max(df[pd.to_datetime(df['DateTimeUTC'],format='%Y-%m-%dT%H:%M:%SZ') + timedelta(hours=12)>=forecast_time]['lat']))
    # show range at least at llongitude of Osaka
    #lonmin = min(135,min(df[pd.to_datetime(df['DateTimeUTC'],format='%Y-%m-%d %H:%M:%S') + timedelta(hours=12)>=forecast_time]['lon']))
    lonmin = min(135,min(df[pd.to_datetime(df['DateTimeUTC'],format='%Y-%m-%dT%H:%M:%SZ') + timedelta(hours=12)>=forecast_time]['lon']))
    #lonmax = max(df[pd.to_datetime(df['DateTimeUTC'],format='%Y-%m-%d %H:%M:%S') + timedelta(hours=12)>=forecast_time]['lon'])
    lonmax = max(df[pd.to_datetime(df['DateTimeUTC'],format='%Y-%m-%dT%H:%M:%SZ') + timedelta(hours=12)>=forecast_time]['lon'])

    # convert Longitudes to positive numbers in case it is negative
    # lonmin = conv_Lon_positive(lonmin)
    # lonmax = conv_Lon_positive(lonmax)

    # stretch the image in case there is no forecast to 5 degress in each direction from last point
    if latmin==latmax:
        latmin=latmin-5
        latmax=latmax+5
    if lonmin==lonmax:
        lonmin=lonmin-5
        lonmax=lonmax+5  
 
    latmin = latmin - (latmax - latmin) * 0.1  # space for legend at the bottom

    latavg = (latmax + latmin) / 2
    lonavg = (lonmax + lonmin) / 2

    # adjust proportions so that the map is not too long nor too wide

    if (latmax - latmin) > (lonmax - lonmin):
        lon_adjust = ((latmax - latmin) - (lonmax - lonmin)) / 2
        lat_adjust = 0
    else:
        lat_adjust = ((lonmax - lonmin) - (latmax - latmin)) / 2
        lon_adjust = 0

    latmin1 = max(latmin - lat_adjust,-90)
    latmax1 = min(latmax + lat_adjust,90)
    lonmin1 = lonmin - lon_adjust
    lonmax1 = lonmax + lon_adjust

    map_width = distance.distance((latmin1, lonmin1), (latmin1, lonmax1)).meters * 1.05
    map_height = distance.distance((latmin1, lonavg), (latmax1, lonavg)).meters * 1.05

    m = Basemap(width=map_width, height=map_height, resolution='h', area_thresh=100,
                projection='lcc', lat_1=latmin, lat_2=latmax, lat_0=latavg, lon_0=lonavg)

    # set up figure

    figwidth = 6.2
    fig = plt.figure(clear=True, figsize=(figwidth, figwidth * m.aspect))
    ax = fig.add_axes([0, 0, 1, 1])
    ax.axis('off')

    m.shadedrelief(alpha=0.4, zorder=1)
    #m.fillcontinents(color='0.5', lake_color=None, ax=None, zorder=1, alpha=1)
    m.drawcountries(linewidth=0.5, zorder=2, color='dimgray')
    
    # plot tracks

    for storm in df['EventID'].unique():

        storm_df = df.loc[df['EventID'] == storm]

        lons = np.array(storm_df['lon'])
        lats = np.array(storm_df['lat'])
        cats = np.array(storm_df['Cat'])

        tx, ty = m(lons, lats)

        if storm < 51:
            # plot ENSEMBLE tracks
            for j in range(len(storm_df) - 1):
                plt.plot(tx[j:j + 2], ty[j:j + 2], linewidth=0.4,
                         color=cat_colors[int(cats[j])],
                         alpha=0.6, zorder=3, solid_capstyle="butt")
        else:
            # plot HRES track
            for j in range(len(storm_df) - 1):
                plt.plot(tx[j:j + 2], ty[j:j + 2], linewidth=2.25, marker='o', markersize=4.5,
                         color=cat_colors[int(cats[j])],
                         alpha=1, zorder=4, solid_capstyle="butt")

            iso_times = list(storm_df['DateTimeUTC'])
            #iso_datetimes = [datetime.datetime.strptime(iso_time, '%Y-%m-%d %H:%M:%S') for iso_time in iso_times]
            iso_datetimes = [datetime.datetime.strptime(iso_time, '%Y-%m-%dT%H:%M:%SZ') for iso_time in iso_times]
            diffs = [int((iso_datetime - forecast_time).total_seconds() / 60 / 60) for iso_datetime in
                     iso_datetimes]

            # HRES track labels

            for k in range(len(diffs)):
                if diffs[k] <= 0:
                    diffs[k] = ''
                else:
                    if diffs[k] in [12, 24, 48, 72, 96, 120, 144, 168, 192]:
                        diffs[k] = '  +' + str(diffs[k]) + 'h'
                    else:
                        diffs[k] = ''

            trk_labels = []
            for p in range(len(diffs)):
                trk_labels.append(
                    plt.text(tx[p], ty[p], diffs[p], verticalalignment='center', horizontalalignment='left',
                             fontproperties=roboto_bold, color='black', zorder=5))

    # legend

    s = m.xmax
    for c in range(6):
        rect = patches.Rectangle((s * 0.2 + c * s * 0.1, s * 0.03), s * 0.02, s * 0.02, linewidth=0,
                                 facecolor=cat_colors[c], zorder=6)
        ax.add_patch(rect)
        plt.text(s * 0.23 + c * s * 0.1, s * 0.0325, 'Cat ' + str(c), fontproperties=roboto, fontsize=9, zorder=6)

    # save figure
    plt.savefig(ofilename, dpi=300, bbox_inches='tight', pad_inches=0.0)


if __name__ == '__main__':

    ##ROOT_DIR = os.path.realpath(os.path.join(os.path.dirname(__file__), '..', '..'))
    ##print(ROOT_DIR)
    ##CT_PATH = os.path.join(ROOT_DIR, 'TRACKS', 'Combined')
    ##print(CT_PATH)
    ##FONT_PATH=os.path.join(ROOT_DIR, 'Plotting', 'fonts')
    
    ##fpath = os.path.join(FONT_PATH,"HelveticaNowText.ttf")
    ##fpath1 = os.path.join(FONT_PATH,"HelveticaNowTextBold.ttf")
    
    # Get the combined track file name passed as command argument
    ctFileName = sys.argv[1]
    
    # sort out source of font information
    if len(sys.argv) > 2:
        # if provided as second passed argument in command line then use
        fpath = os.path.join(sys.argv[2],"HelveticaNowText.ttf")
        fpath1 = os.path.join(sys.argv[2],"HelveticaNowTextBold.ttf")
    else:
        # otherwise fallback to absolute path
        fpath = r"D:\03_JapanAER\SCRIPTS\Plotting\fonts\HelveticaNowText.ttf"
        fpath1 = r"D:\03_JapanAER\SCRIPTS\Plotting\fonts\HelveticaNowTextBold.ttf"
    
    # check if fonts really exist
    if os.path.isfile(fpath) and os.path.isfile(fpath1):
        roboto = fm.FontProperties(fname=fpath)
        roboto_bold = fm.FontProperties(fname=fpath1)
    else:
        print('Warning: Default font was used, because there is no font at the end of provided path.')
        roboto = fm.FontProperties(family=None, style=None, variant=None, weight=None, stretch=None, size=None, fname=None, math_fontfamily=None)
        roboto_bold = fm.FontProperties(family=None, style=None, variant=None, weight=None, stretch=None, size=None, fname=None, math_fontfamily=None)
    
    #trackFile = os.path.join(CT_PATH, ctFileName)
    #print(trackFile)
    fileName = os.path.basename(ctFileName)
    print(fileName)
    
    #fsttime = sys.argv[1].split('_')[3]
    fsttime = fileName.split('_')[3]
    fsttime = fsttime.split('.')[0]
    fsttime = fsttime[0:4] + '-' + fsttime[4:6] + '-' + fsttime[6:8] + ' ' + fsttime[8:10] + ':00:00'
    track_map(ctFileName,fsttime,ctFileName.replace('.csv','.png'))
