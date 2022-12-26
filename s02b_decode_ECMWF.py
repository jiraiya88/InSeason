###################################################
### Following changes have been done in this script.
###
### 1. Changed the name of folder from track to TRACKS
### 2. Modified the code to assign folder paths to the following instead of hardcoding
###    foldertmp =  
###    folder =  
###    filename =
###################################################

import os
from ftplib import FTP
import fnmatch
import sys
from pybufrkit.decoder import Decoder
from pybufrkit.renderer import FlatJsonRenderer
import glob

def xstr(s):
    return '' if s is None else str(s)

def ecmwf_extract(file, year, ensemble, forecast_time, datafolder, storm):
    """
    Extracts data from the ECMWF BUFR file and converts it to A-deck format
    :param file: BUFR file
    :param noyr:
    :param year:
    :param ensemble: True if we want 50-member ensemble
    :return:
    """
    ep = 101100 # environmental pressure in hPa
    
    if ensemble:
        mem_num = 50
    else:
        mem_num = 1
    print("   ... Number of members: " + str(mem_num))
        
    #print(ensemble)
    if ensemble==1:
        trk = open(os.path.join(datafolder, 'ECMWF_ENS_' + storm + '_' + forecast_time + '.csv'),'w')
        print("   ... Creating file: " + xstr(os.path.join(datafolder, 'ECMWF_ENS_' + storm + '_' + forecast_time + '.csv')))
    else:
        trk = open(os.path.join(datafolder, 'ECMWF_HRES_' + storm + '_' + forecast_time + '.csv'),'w')
        print("   ... Creating file: " + xstr(os.path.join(datafolder, 'ECMWF_HRES_' + storm + '_' + forecast_time + '.csv')))

    columns = ["basin", "TC_num", "forecast_date", "technum", "tech", "forecast_hours","lat", "lon", "vmax", "mslp", "cat", "radii", "quadrant","rad1", "rad2", "rad3", "rad4"]
    trk.write(",".join(map(str, columns)) + "\n")

    print("File...")
    print(file)
    if not (file==''):      # handle cases if no file has been found
        decoder = Decoder()
        with open(file, 'rb') as ins:
            bufr_message = decoder.process(ins.read())
        json_data = FlatJsonRenderer().render(bufr_message)
        data = json_data[4][2]

        noyr=data[0][3].decode('ascii')[:2]

        for i in range(mem_num):
            if len(data[i])>65:
                f_range = int((len(data[i]) - 66) / 49) + 1
                xs = [data[i][68 + j * 49] for j in range(f_range)]  # [data[i][18]] +
                ys = [data[i][67 + j * 49] for j in range(f_range)]  # [data[i][17]]
                pressure = [data[i][69 + j * 49] for j in range(f_range)]
                speed = [data[i][73 + j * 49] for j in range(f_range)]
                forecast = [data[i][65 + j * 49] for j in range(f_range)]

                # 2 missing positions are allowed:
                break_none = 0
                for t in range(len(xs)):
                    if xs[t] is None:
                        break_none = break_none + 1
                        if break_none == 3:
                            truerange = t - 1
                            break
                    else:
                        truerange = t + 1

                wr34 = []  # [list(round(data[i][j] / 1852, 2) for j in [27, 30, 33, 36])]
                for k in range(truerange):
                    wrs = list(data[i][j] for j in [77 + k * 49, 80 + k * 49, 83 + k * 49, 86 + k * 49])
                    for w in range(len(wrs)):
                        if wrs[w] is None:
                            wrs[w] = 0.0
                    wrs = [round(wr / 1852, 2) for wr in wrs]
                    wr34.append(wrs)

                wr50 = []  # [list(round(data[i][j] / 1852, 2) for j in [40, 43, 43, 49])]
                for k in range(truerange):
                    wrs = list(data[i][j] for j in [90 + k * 49, 93 + k * 49, 96 + k * 49, 99 + k * 49])
                    for w in range(len(wrs)):
                        if wrs[w] is None:
                            wrs[w] = 0.0
                    wrs = [round(wr / 1852, 2) for wr in wrs]
                    wr50.append(wrs)

                wr64 = []  # [list(round(data[i][j] / 1852, 2) for j in [53, 56, 59, 62])]
                for k in range(truerange):
                    wrs = list(data[i][j] for j in [103 + k * 49, 106 + k * 49, 109 + k * 49, 112 + k * 49])
                    for w in range(len(wrs)):
                        if wrs[w] is None:
                            wrs[w] = 0.0
                    wrs = [round(wr / 1852, 2) for wr in wrs]
                    wr64.append(wrs)

                mps = speed[0:truerange]

                pascal = pressure[0:truerange]
                pascal=[ep if pascal is None else pascal for pascal in pascal]

                for s in range(truerange):
                    if mps[s] is None:
                        continue
                    basin = data[i][3][-1:].decode('ascii')
                    st_num = data[i][3].decode('ascii')[:2]
                    f_cycle = str(data[i][8]) + str(data[i][9]).zfill(2) + str(data[i][10]).zfill(2) + str(data[i][11]).zfill(2)
                    if ensemble:
                        tech = "ENS" + str(i + 1).zfill(2)
                    else:
                        tech = "HRES"
                    for_hour = forecast[s]
                    lat = str(ys[s])
                    lon = str(xs[s])
                    kn = str(round(mps[s] / 0.514444, 2))
                    mb = round(pascal[s] / 100,1)
                    a2 = ""

                    fstrow = [basin, st_num, f_cycle, noyr, tech, for_hour, lat, lon, kn, mb, a2]

                    fst34 = fstrow + ["34", "NEQ"] + (wr34[s])
                    fst50 = fstrow + ["50", "NEQ"] + (wr50[s])
                    fst64 = fstrow + ["64", "NEQ"] + (wr64[s])

                    if fst34[5] <= 120:
                        trk.write(",".join(map(str, fst34)) + "\n")
                    if sum(fst50[-4:]) != 0 and fst50[5] <= 120:
                        trk.write(",".join(map(str, fst50)) + "\n")
                    if sum(fst64[-4:]) != 0 and fst64[5] <= 120:
                        trk.write(",".join(map(str, fst64)) + "\n")

    trk.close()
    print("   Data decoded")
        
if __name__ == '__main__':

    #folderTemp = sys.argv[1]
    #print(folderTemp);
    #folderECMWF = sys.argv[2]
    #print(folderECMWF);
    #filename = sys.argv[3]
    #print(filename);
    
    dirname = os.path.dirname(__file__)
    folderTemp = os.path.join(dirname, 'temp')  
    filedir = os.path.join(dirname, 'TRACKS')  
    folderECMWF = os.path.join(filedir, 'ECMWF')  
    filename = os.path.join(filedir, 'Track_Dashboard.csv')
    # open dashboard file with observed tracks, look which events are active and search their forecast by name
    with open(filename, "r") as dsh:
        for line in dsh:
            l = line.split(',')
            if l[9] == '1\n':
                forecast = l[6][:13].replace('T', '').replace('-', '')
                storm = l[2]
                year = l[6][:4]
                print("Searching ECMWF forecast: Tropical cyclone: " + str(storm) + ', Forecast time: ' + str(forecast))
                ens_outfile = glob.glob(folderTemp + "/*" + "ECEP" + "*" + storm + "*.bin")
                print(ens_outfile)
                if ens_outfile==[]:
                   ens_outfile = ''
                else:
                    ens_outfile = ens_outfile[0]
                print("   Decoding ENS files")
                ensemble = 1
                ecmwf_extract(ens_outfile, year, ensemble, forecast, folderECMWF, storm)
                hres_outfile = glob.glob(folderTemp + "\\*" + "ECMF" + "*" + storm + "*.bin")
                print(hres_outfile)
                if hres_outfile==[]:
                   hres_outfile = ''
                else:
                    hres_outfile = hres_outfile[0]
                print("   Decoding HRES files")
                ensemble = 0
                ecmwf_extract(hres_outfile, year, ensemble, forecast, folderECMWF, storm)

    dsh.close()

