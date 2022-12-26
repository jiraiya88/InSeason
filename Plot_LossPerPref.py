import sys
import os
from datetime import datetime
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import matplotlib as mp
import numpy as np
import math
import pandas as pd

# use in command line:
# > python Plot_LossPerPref.py aer_jpty_pref_loss_top10.csv aer_jpty_pref_claim_top10.csv ../fonts/

# inputs are 
# sys.argv[0]: name of the script
# sys.argv[1]: name (and path) of the text file with data to be plotted - LOSSES
# sys.argv[2]: name (and path) of the text file with data to be plotted - NUMBER OF CLAIMS
# sys.argv[3]: path to a folder with font information

def plotprefbar(filename,mode,outname):

    data = pd.read_csv(filename)

    # set chart units
    datamax = max(max(data['PCT90']), max(data['HRES']))

    if mode=='loss':    
        if datamax > 1000000000:
            ulabel = 'B'
            divide = 1000000000
        else:
            divide = 1000000
            ulabel = 'M'
    else:
        if datamax > 1000000:
            ulabel = 'MILLIONS'
            divide = 1000000
        else:
            divide = 1000
            ulabel = 'THOUSANDS'

    # maximum rounded data value
    ceil=math.ceil(datamax/divide)

    # parameter setup
    barh=.5
    bhfac=1.2
    hrespct=0.01

    # sort from largest as per ENS Median
    data = data.sort_values(['MEDIAN','PCT90','PCT10','HRES'], ascending=[True,True,True,True])
    data = data.head(n=10)

    # COLOR PALLETTE
    axistextcol = "#808080"
    threshcol = '#4d4f53'
    nhccolor = "#007585" 
    ofclcolor = "#D14900"
    span1090col = "#EEF6F7"
    span1090col_text = "#CDDBDE"
    span2575col = "#CDDBDE"
    span2575col_text = "#ACC0C4"

    # loading data
    pref = np.array(data['Prefecture'],dtype='str')
    pref = np.char.upper(pref)                          # make upper case
    pct10 = np.array(data['PCT10']) / divide
    pct25 = np.array(data['PCT25']) / divide
    pct50 = np.array(data['MEDIAN']) / divide
    pct75 = np.array(data['PCT75']) / divide
    pct90 = np.array(data['PCT90']) / divide
    hres = np.array(data['HRES']) / divide
    mwidth = hrespct*ceil                               # width of marker relative to plot range

    # decode on x axis label
    if mode=='loss':
        mxlabel='LOSS (' + ulabel + '¥)'
    else:
        mxlabel='NUMBER OF CLAIMS (' + ulabel + ')'

    # plotting        
    fig = plt.figure(figsize=(6.25, 3.5), dpi=96, facecolor='w', edgecolor='k')
    ax = fig.add_subplot(111,xlim=(0,ceil))
    ax.xaxis.set_label_position('top')
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    ax.spines['bottom'].set_color(axistextcol)
    ax.spines['left'].set_color(axistextcol)
    ax.spines['top'].set_color(axistextcol)
    ax.spines['right'].set_color('#000000')
    ax.spines['left'].set_linewidth(0.5)
    ax.spines['top'].set_linewidth(0.5)
    ax.tick_params(axis='x', colors=axistextcol,labeltop=True,top=True,labelbottom=False,bottom=False,labelsize=9)
    ax.tick_params(axis='y', colors=axistextcol,labelsize=9,labelleft=False)
    plt.xticks(fontproperties=roboto)
    plt.yticks(fontproperties=roboto)

    ax.barh(pref, pct90-pct10, align='center', height=barh, left=pct10, color=span1090col,label='range pct10 to pct90',zorder=1)
    ax.barh(pref, pct75-pct25, align='center', height=barh, left=pct25, color=span2575col,label='range pct25 to pct75',zorder=3)
    ax.barh(pref, mwidth, align='center', height=barh, left=np.maximum(0,pct50-mwidth/2), color=ofclcolor,label='ENS Median',zorder=3)
    ax.barh(pref, mwidth, align='center', height=bhfac*barh, left=np.maximum(0,hres-mwidth/2), color=nhccolor,label='HRES',zorder=3)

    ylocs = ax.get_yticks()
    for k,npref in enumerate(pref):
        ax.text(-ceil/5.3, ylocs[k], npref, horizontalalignment='left', verticalalignment='center',color=axistextcol,fontproperties=roboto,fontsize=9)

    plt.xlabel(mxlabel, horizontalalignment='right', x=1.0,color=axistextcol,fontproperties=roboto,fontsize=9)

    ax.text(plt.xlim()[1], plt.ylim()[1]*0.0, "ENSEMBLE 10$^{TH}$ TO 90$^{TH}$ PERCENTILE", fontproperties=roboto_bold, fontsize=9,
            color=span1090col_text,verticalalignment='center', horizontalalignment='right',zorder=10)
    ax.text(plt.xlim()[1], plt.ylim()[1]*0.06, "ENSEMBLE 25$^{TH}$ TO 75$^{TH}$ PERCENTILE", fontproperties=roboto_bold, fontsize=9,
            color=span2575col_text,verticalalignment='center', horizontalalignment='right')
    ax.text(plt.xlim()[1], plt.ylim()[1] * 0.12, "ENSEMBLE MEDIAN", fontproperties=roboto_bold, fontsize=9,
            color=ofclcolor,verticalalignment='center', horizontalalignment='right')
    ax.text(plt.xlim()[1], plt.ylim()[1] * 0.18, "HRES FORECAST", fontproperties=roboto_bold, fontsize=9,
            color=nhccolor, verticalalignment='center', horizontalalignment='right')

    # printing
    plt.subplots_adjust(left=0.18,bottom=0.1,right=0.92,top=0.86)
    plt.savefig(outname,dpi=600,pad_inches=0.35, transparent=True)

# sort out source of font information
if len(sys.argv) > 3:
    # if provided as second passed argument in command line then use
    fpath = os.path.join(sys.argv[3],"HelveticaNowText.ttf")
    fpath1 = os.path.join(sys.argv[3],"HelveticaNowTextBold.ttf")
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

# Main function
# plot losses per Prefecture
filename=sys.argv[1]
pathAndFile=os.path.split(filename)
outname=os.path.join(pathAndFile[0],"PrefectureLoss.png")
plotprefbar(filename,'loss',outname)

if sys.argv[4] == 'true':
    # plot claims per Prefecture
    filename=sys.argv[2]
    pathAndFile=os.path.split(filename)
    outname=os.path.join(pathAndFile[0],"PrefectureClaim.png")
    plotprefbar(filename,'claim',outname)
