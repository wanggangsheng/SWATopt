# This program reformats Daymet single point extraction data to SWAT format (written for Latha Baskaran, ORNL)
# Michele Thornton, ORNL
# February 4, 2013

import os
import numpy

dir_cur = os.getcwd()  # "C:\\WGS\\TRB\\Data\\aData\\daymet\\"
dir_cur = dir_cur + "/"
print(dir_cur + "\n")
# no. of rows to skip while reading daymet data
nrow_skip = 8
ncol_dt = 2  #year, julian day
# Swap out SWATlist with SWATlist_one.txt to get data read for one
# station.  ##list of SWAT station created with dos dir /B > SWATlist.txt
SWAT_list = open(dir_cur + "SWATlist.txt", 'r')

SWAT_filename = SWAT_list.readlines()  # read all station file names
SWAT_list.close()
SWAT_file0 = SWAT_filename[1].rstrip()
filename = dir_cur + "input/" + SWAT_file0
data = open(filename, 'r+')
# Read in (skip) first 8 lines of header info
for i in range(nrow_skip):
    header_line = data.readline()
data_lines = data.readlines()

ngage = len(SWAT_filename)
nday = len(data_lines)
print("ngage="+str(ngage)+"; nday="+str(nday)+"\n")

# year_all = numpy.zeros((nday))
# jday_all = numpy.zeros((nday))
prcp_all = numpy.zeros((nday, ngage + ncol_dt))
tmax_all = numpy.zeros((nday, ngage + ncol_dt))
tmin_all = numpy.zeros((nday, ngage + ncol_dt))
srad_all = numpy.zeros((nday, ngage + ncol_dt))


print("\n>>>>>>>>>>>>>>>>>>>>>>>>>>")
print("START DAYMET TO SWAT...")
igage = -1
for SWAT_file in SWAT_filename:
    igage = igage + 1
    SWAT_file0 = SWAT_file.rstrip()
    filename = dir_cur + "input/" + SWAT_file0
    print(filename)
    len1 = len(SWAT_file0)  # length of string
    len2 = len1 - 4  # length of SWAT_file excluding extension
    # print len1, len2
    if len2 > 7:
        SWAT_file1 = SWAT_file0[len2 - 7: len2]
    else:
        SWAT_file1 = SWAT_file0
    # print SWAT_file[4:10]

    print(SWAT_file1)
    data = open(filename, 'r+')

    # Define first line of output file
    # first_line = '19800101'  # DAYMET starts from 01/01/1980
    # # Define name of output file for each varible
    # prcp_file_name = 'P' + SWAT_file1 + '.txt'  # 'P' + SWAT_file[4:10] + 'TXT'
    # temp_file_name = 'T' + SWAT_file1 + '.txt'
    # srad_file_name = 'R' + SWAT_file1 + '.txt'
    # print(prcp_file_name, temp_file_name, srad_file_name)
    # SWAT_prcp_file = open(dir_cur + "output/" + prcp_file_name, 'w')
    # SWAT_temp_file = open(dir_cur + "output/" + temp_file_name, 'w')
    # SWAT_srad_file = open(dir_cur + "output/" + srad_file_name, 'w')

    # SWAT_prcp_file.write(first_line + '\n')
    # SWAT_temp_file.write(first_line + '\n')
    # SWAT_srad_file.write(first_line + '\n')

    # Read in (skip) first 8 lines of header info
    for i in range(nrow_skip):
        header_line = data.readline()

    # Read in the data lines of the file and assign SWAT variables
    data_lines = data.readlines()
    data.close()
    iday = -1
    for line in data_lines:
        iday = iday + 1
        line.rstrip()
        variable = line.split(',')

        year = int(variable[0])
        jday = int(variable[1])
        tmax = variable[2]
        tmin = variable[3]
        prcp = variable[5]
        srad = variable[6]
        dayl = variable[4]

        # convert srad from W/m^2 to MJ/m^2/day
        sradflt = (float(srad))
        daylflt = (float(dayl))
        srad_convert = ((sradflt * daylflt) / 1000000)
        # print "srad_convert", srad_convert
        # print tmax, tmin, prcp, srad, dayl, srad_convert
        if (igage == 0):
            prcp_all[iday, 0] = year
            prcp_all[iday, 1] = jday
            tmax_all[iday, 0] = year
            tmax_all[iday, 1] = jday
            tmin_all[iday, 0] = year
            tmin_all[iday, 1] = jday
            srad_all[iday, 0] = year
            srad_all[iday, 1] = jday


        prcp_all[iday, igage + ncol_dt] = float(prcp)
        tmax_all[iday, igage + ncol_dt] = float(tmax)
        tmin_all[iday, igage + ncol_dt] = float(tmin)
        srad_all[iday, igage + ncol_dt] = srad_convert

        # daymet regards every year as non-leap year,
        # thus for leap year, simply copy the data of day-28 to day-29
        # k = 1
        # if (((year % 4 == 0 and year % 100 != 0) or year % 400 == 0) and jday == 28):
        #     k = 2

        # for j in range(k):
        #     SWAT_prcp_file.writelines(prcp + '\n')
        #     SWAT_temp_file.writelines(tmax + "," + tmin + '\n')
        #     SWAT_srad_file.writelines(str(srad_convert) + '\n')
# end: for SWAT_file in SWAT_filename:
prcp_file_name = dir_cur + "output/" + 'HUC8_PRCP.dat' 
tmax_file_name = dir_cur + "output/" + 'HUC8_TMAX.dat'
tmin_file_name = dir_cur + "output/" + 'HUC8_TMIN.dat'
srad_file_name = dir_cur + "output/" + 'HUC8_SRAD.dat'
# numpy.savetxt(prcp_file_name, (year_all, jday_all, prcp_all[:, 1]))
numpy.savetxt(prcp_file_name, prcp_all, fmt='%10.2f')
# numpy.savetxt(tmax_file_name, (year_all, jday_all, tmax_all))
# numpy.savetxt(tmin_file_name, (year_all, jday_all, tmin_all))
# numpy.savetxt(srad_file_name, (year_all, jday_all, srad_all))

# prcp_file = open(dir_cur + "output/" + prcp_file_name, 'w')
# tmax_file = open(dir_cur + "output/" + tmax_file_name, 'w')
# tmin_file = open(dir_cur + "output/" + tmin_file_name, 'w')
# srad_file = open(dir_cur + "output/" + srad_file_name, 'w')
# for j in range(nday):
#     prcp_file.writelines(str(year_all[j]) + '\t' + str(jday_all[j]) + '\t' + str(prcp_all[j])+'\n')
#     tmax_file.writelines(str(year_all[j]) + '\t' + str(jday_all[j]) + '\t' + str(tmax_all[j])+'\n')
#     tmin_file.writelines(str(year_all[j]) + '\t' + str(jday_all[j]) + '\t' + str(tmin_all[j])+'\n')
#     srad_file.writelines(str(year_all[j]) + '\t' + str(jday_all[j]) + '\t' + str(srad_all[j])+'\n')

# prcp_file.close()
# tmax_file.close()
# tmin_file.close()
# srad_file.close()
print("DONE!")
