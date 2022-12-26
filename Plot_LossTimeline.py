import pandas as pd
from matplotlib import pyplot as plt
import matplotlib as mp
import matplotlib.font_manager as fm
import numpy as np
from datetime import datetime
from datetime import timedelta
import sys
import os

# use in command line:
# > python Plot_LossTimeline.py aer_LossTimeline_data.csv ../fonts/

# inputs are 
# sys.argv[0]: name of the script
# sys.argv[1]: name (and path) of the text file with data to be plotted
# sys.argv[2]: path to a folder with font information

def get_ylabels(y1, y2, y3, y4, mindis):
    """
    :param y1-4: y positions of data
    :param mindis: minimum distance between labels
    :return: y positions for 4 data points
    """

    a = [y1, y2, y3, y4]

    positions = [0, 0, 0, 0]

    p = 0
    while p < 4:
        ind = a.index(min(a))
        if a[ind] < mindis:
            if sum(positions) == 0:
                positions[ind] = max(positions) + mindis / 2
            else:
                positions[ind] = max(positions) + mindis
        else:
            if a[ind] - max(positions) < mindis:
                positions[ind] = max(positions) + mindis
            else:
                positions[ind] = a[ind]
        a[ind] = 9999
        p = p + 1

    return positions[0], positions[1], positions[2], positions[3]


def plot_data(file, client_threshold, filename):
    """
    :param file: data for plotting in csv - columns: Date,HRES,MEDIAN,PCT90,PCT10
    :param client_threshold: threshold for sending emails
    :return:
    """

    data = pd.read_csv(file)
    plotndays = 5   # 5 last days will be plotted, everything before that is ignored
    
    utcdates = np.array(data['Date'])
    edtdates = [datetime.strptime(str(utcdate), '%Y%m%d%H') + timedelta(hours=9) for utcdate in utcdates]
    dates = [datetime.strftime(edtdate, '%Y%m%d%I%p') for edtdate in edtdates]
    
    # only plot last plotndays
    initdate = max(edtdates[0],edtdates[-1]-timedelta(days=plotndays))
    print('Only last ' + str(plotndays) + ' days is plotted')
    print('Date of the first considered data point: ' + str(initdate - timedelta(hours=9)) + ' UTC')
    
    data = data[np.array(edtdates)>=initdate]
    
    # set chart units

    datamax = max(max(data['PCT90']), max(data['HRES']))
    if datamax > 1000000000:
        units = 'B'
        unitlabel = 'B'
        divide = 1000000000
    else:
        units = 'M'
        divide = 1000000
        unitlabel = 'M'
    
    utcdates = np.array(data['Date'])
    edtdates = [datetime.strptime(str(utcdate), '%Y%m%d%H') + timedelta(hours=9) for utcdate in utcdates]
    dates = [datetime.strftime(edtdate, '%Y%m%d%I%p') for edtdate in edtdates]
    
    # extract data
    pct90 = np.array(data['PCT90']) / divide
    pct10 = np.array(data['PCT10']) / divide
    pct75 = np.array(data['PCT75']) / divide
    pct25 = np.array(data['PCT25']) / divide
    median = np.array(data['MEDIAN']) / divide
    ofcl = np.array(data['HRES']) / divide
    threshold = int(client_threshold) / divide

    utcdates = np.array(data['Date'])
    edtdates = [datetime.strptime(str(utcdate), '%Y%m%d%H') + timedelta(hours=9) for utcdate in utcdates]
    dates = [datetime.strftime(edtdate, '%Y%m%d%I%p') for edtdate in edtdates]

    # COLOR PALLETTE
    axistextcol = "#808080"
    threshcol = '#4d4f53'
    nhccolor = "#007585" 
    ofclcolor = "#D14900"
    span1090col = "#EEF6F7"
    span1090col_text = "#C1D5D9"
    span2575col = "#CDDBDE"
    span2575col_text = "#ACC0C4"

    # set up figure

    fig = plt.figure(figsize=(6.25, 3.5), dpi=96, facecolor='w', edgecolor='k')
    ax = plt.axes()
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.spines['left'].set_color(axistextcol)
    ax.spines['bottom'].set_color(axistextcol)
    ax.spines['left'].set_linewidth(0.5)
    ax.spines['bottom'].set_linewidth(0.5)

    # Set axes limits

    all_max = max(max(pct90), max(ofcl), threshold)
    if all_max > 100:
        yaxis_max = (all_max / 0.85 + 24) // 25 * 25
    elif all_max < 8.5:
        yaxis_max = (all_max / 0.85 + 0.4) // 0.5 * 0.5
    elif all_max < 24:
        yaxis_max = (all_max / 0.85 + 4) // 5 * 5
    else:
        yaxis_max = (all_max / 0.85 + 9) // 10 * 10

    plt.ylim(bottom=0)
    plt.ylim(top=yaxis_max)

    if len(data) == 1:
        plt.xlim(left=0)
        plt.xlim(right=1.4)
    else:
        plt.xlim(left=edtdates[0])
        plt.xlim(right=edtdates[-1] + (edtdates[-1] - edtdates[0]) * 0.55)

   # get label positions

    minimal_distance = yaxis_max * 0.05
    med_y, ofcl_y, pct10_y, pct90_y = get_ylabels(median[-1], ofcl[-1], pct10[-1], pct90[-1], minimal_distance)

    if median[-1] < 10:
        med_label = "¥{:.1f}".format(median[-1]) + unitlabel
    else:
        med_label = "¥{:.0f}".format(median[-1]) + unitlabel
    if ofcl[-1] < 10:
        ofcl_label = "¥{:.1f}".format(ofcl[-1]) + unitlabel
    else:
        ofcl_label = "¥{:.0f}".format(ofcl[-1]) + unitlabel
    if pct90[-1] < 10:
        pct90_label = "¥{:.1f}".format(pct90[-1]) + unitlabel
    else:
        pct90_label = "¥{:.0f}".format(pct90[-1]) + unitlabel
    if pct75[-1] < 10:
        pct75_label = "¥{:.1f}".format(pct75[-1]) + unitlabel
    else:
        pct75_label = "¥{:.0f}".format(pct75[-1]) + unitlabel
    if pct25[-1] < 10:
        pct25_label = "¥{:.1f}".format(pct25[-1]) + unitlabel
    else:
        pct25_label = "¥{:.0f}".format(pct25[-1]) + unitlabel
    if pct10[-1] < 10:
        pct10_label = "¥{:.1f}".format(pct10[-1]) + unitlabel
    else:
        pct10_label = "¥{:.0f}".format(pct10[-1]) + unitlabel

    # plot data
    
    if len(data) == 1:
        ax.plot(0.5, median[-1], color=ofclcolor, marker='o', markersize=7)
        ax.plot(0.5, ofcl[-1], color=nhccolor, marker='o', markersize=7)
        ax.plot(0.5, pct90[-1], color=span1090col, marker='o', markersize=7)
        ax.plot(0.5, pct10[-1], color=span1090col, marker='o', markersize=7)
        ax.plot(0.5, pct75[-1], color=span2575col, marker='o', markersize=7)
        ax.plot(0.5, pct25[-1], color=span2575col, marker='o', markersize=7)
        
        ax.text(0.53, med_y, med_label, fontproperties=roboto_bold, fontsize=9,
                color=ofclcolor, verticalalignment='center')
        ax.text(0.53, ofcl_y, ofcl_label, fontproperties=roboto_bold,
                fontsize=9, color=nhccolor, verticalalignment='center')
        ax.text(0.53, pct90_y, pct90_label, fontproperties=roboto_bold,
                fontsize=9, color=span1090col_text, verticalalignment='center')
        #ax.text(0.53, pct75_y, pct75_label, fontproperties=roboto_bold,
        #        fontsize=9, color=span2575col_text, verticalalignment='center')
        #ax.text(0.53, pct25_y, pct25_label, fontproperties=roboto_bold,
        #        fontsize=9, color=span2575col_text, verticalalignment='center')
        ax.text(0.53, pct10_y, pct10_label, fontproperties=roboto_bold,
                fontsize=9, color=span1090col_text, verticalalignment='center')
        plt.axhline(y=threshold, xmax=plt.xlim()[1], color=threshcol, linewidth=1.0, linestyle='--')

    else:
        plt.fill_between(edtdates, pct90, pct10, color=span1090col)
        plt.fill_between(edtdates, pct75, pct25, color=span2575col)
        plt.plot(edtdates, median, linewidth=2.0, color=ofclcolor)
        plt.plot(edtdates, ofcl, linewidth=2.0, color=nhccolor)
        plt.axhline(y=threshold, xmax=plt.xlim()[1], color=threshcol, linewidth=1.0, linestyle='--')

        ax.plot(edtdates[-1], median[-1], color=ofclcolor, marker='o', markersize=5)
        ax.plot(edtdates[-1], ofcl[-1], color=nhccolor, marker='o', markersize=5)

        ax.text(edtdates[-1] + (edtdates[-1]-edtdates[0])/45, med_y, med_label, fontproperties=roboto_bold,
                fontsize=9,
                color=ofclcolor, verticalalignment='center')
        ax.text(edtdates[-1] + (edtdates[-1]-edtdates[0])/45, ofcl_y, ofcl_label, fontproperties=roboto_bold,
                fontsize=9,
                color=nhccolor, verticalalignment='center')
        ax.text(edtdates[-1] + (edtdates[-1]-edtdates[0])/45, pct10_y, pct10_label, fontproperties=roboto_bold,
                fontsize=9,
                color=span1090col_text, verticalalignment='center')
        #ax.text(edtdates[-1] + (edtdates[-1]-edtdates[0])/45, pct25_y, pct25_label, fontproperties=roboto_bold,
        #        fontsize=9,
        #        color=span2575col_text, verticalalignment='center')
        #ax.text(edtdates[-1] + (edtdates[-1]-edtdates[0])/45, pct75_y, pct75_label, fontproperties=roboto_bold,
        #        fontsize=9,
        #        color=span2575col_text, verticalalignment='center')
        ax.text(edtdates[-1] + (edtdates[-1]-edtdates[0])/45, pct90_y, pct90_label,
                fontproperties=roboto_bold,
                fontsize=9,
                color=span1090col_text, verticalalignment='center')
                               
    # plot legend

    if len(data) == 1:
        ax.text(0.025, plt.ylim()[1], 'LOSS (' + units + '¥)', fontproperties=roboto, fontsize=9,
                color=axistextcol,
                verticalalignment='center', horizontalalignment='left')
    else:
        ax.text(edtdates[0] + (edtdates[-1] - edtdates[0]) / 45, plt.ylim()[1], 'LOSS (' + units + '¥)', fontproperties=roboto, fontsize=9,
                color=axistextcol,
                verticalalignment='center', horizontalalignment='left')
                
    ax.text(plt.xlim()[1], threshold, "CLIENT THRESHOLD", fontproperties=roboto, fontsize=9, color=threshcol,
            verticalalignment='bottom', horizontalalignment='right')
    ax.text(plt.xlim()[1], plt.ylim()[1], "ENSEMBLE 10$^{TH}$ TO 90$^{TH}$ PERCENTILE", fontproperties=roboto_bold, fontsize=9,
            color=span1090col_text,
            verticalalignment='center', horizontalalignment='right')
    ax.text(plt.xlim()[1], plt.ylim()[1] * 0.94, "ENSEMBLE 25$^{TH}$ TO 75$^{TH}$ PERCENTILE", fontproperties=roboto_bold, fontsize=9,
            color=span2575col_text,
            verticalalignment='center', horizontalalignment='right')
    ax.text(plt.xlim()[1], plt.ylim()[1] * 0.88, "ENSEMBLE MEDIAN", fontproperties=roboto_bold, fontsize=9,
            color=ofclcolor, verticalalignment='center', horizontalalignment='right')
    ax.text(plt.xlim()[1], plt.ylim()[1] * 0.82, "HRES FORECAST", fontproperties=roboto_bold, fontsize=9,
            color=nhccolor, verticalalignment='center', horizontalalignment='right')

    # Set x-axis Labels

    xlabels = [str(date)[6:8] + '/' +
            str(date)[4:6] + '\n' +
            str(date)[-4:] for date in dates]

    if len(xlabels) < 8:
        xmask = np.arange(0, len(xlabels))
    elif len(xlabels) < 15:
        if edtdates[-1].hour in [3, 15]:
            xmask = [edtdate.hour in [3, 15] for edtdate in edtdates]
        elif edtdates[-1].hour in [9, 21]:
            xmask = [edtdate.hour in [9, 21] for edtdate in edtdates]
        else:
            edthour = edtdates[-3].hour
            if edthour in [3, 15]:
                xmask = [edtdate.hour in [3, 15] for edtdate in edtdates]
            else:
                xmask = [edtdate.hour in [9, 21] for edtdate in edtdates]
            xmask[-1] = True
    else:
        if edtdates[-1].hour in [3, 9, 15, 21]:
            edthour = edtdates[-1].hour
            xmask = [edtdate.hour == edthour for edtdate in edtdates]
        else:
            edthour = edtdates[-4].hour
            xmask = [edtdate.hour == edthour for edtdate in edtdates]
            xmask[-1] = True

    xlabs = np.array(xlabels)[xmask]
    edtx = np.array(edtdates)[xmask]

    if len(xlabs) == 1:
        plt.xticks([0.5], xlabs, fontproperties=roboto, fontsize=9, color=axistextcol)
    else:
        plt.xticks(edtx, xlabs, fontproperties=roboto, fontsize=9, color=axistextcol)

    plt.yticks(fontsize=9, color=axistextcol)
    ax.tick_params(direction='out', length=2.5, width=0.5, colors=axistextcol)

    ax.text(plt.xlim()[1], 0 - plt.ylim()[1] / 35, "FORECAST\nUPDATE (JST)", fontproperties=roboto, fontsize=9,
            color=axistextcol, verticalalignment='top', horizontalalignment='right')

    plt.tight_layout()
    plt.savefig(filename, dpi=600, bbox_inches='tight', pad_inches=0.35, transparent=True)

if __name__ == '__main__':

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

    if len(sys.argv) > 3:
        datfile = sys.argv[1]
        client_threshold = sys.argv[2]
        outfile = datfile.replace('.csv', '.png')
        plot_data(datfile, client_threshold, outfile)
    else:
        print('Some of the mandatory parameters are missing (file with data or threshold value).')
