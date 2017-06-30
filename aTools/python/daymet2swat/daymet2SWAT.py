# This program reformats Daymet single point extraction data to SWAT format 
# Gangsheng Wang, wangg@ornl.gov
# February 4, 2015

import os

dir_cur = os.getcwd()  # "C:\\WGS\\TRB\\Data\\aData\\daymet\\"
dir_cur = dir_cur + "/"
print(dir_cur + "\n")
# Swap out SWATlist with SWATlist_one.txt to get data read for one
# station.  ##list of SWAT station created with dos dir /B > SWATlist.txt
SWAT_list = open(dir_cur + "SWATlist.txt", 'r')

SWAT_filename = SWAT_list.readlines()  # read all station file names

print("\n>>>>>>>>>>>>>>>>>>>>>>>>>>")
print("START DAYMET TO SWAT...")
for SWAT_file in SWAT_filename:
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
    first_line = '19800101'  # DAYMET starts from 01/01/1980
    # Define name of output file for each varible
    prcp_file_name = 'P' + SWAT_file1 + '.txt'  # 'P' + SWAT_file[4:10] + 'TXT'
    temp_file_name = 'T' + SWAT_file1 + '.txt'
    srad_file_name = 'R' + SWAT_file1 + '.txt'
    print(prcp_file_name, temp_file_name, srad_file_name)
    SWAT_prcp_file = open(dir_cur + "output/" + prcp_file_name, 'w')
    SWAT_temp_file = open(dir_cur + "output/" + temp_file_name, 'w')
    SWAT_srad_file = open(dir_cur + "output/" + srad_file_name, 'w')

    SWAT_prcp_file.write(first_line + '\n')
    SWAT_temp_file.write(first_line + '\n')
    SWAT_srad_file.write(first_line + '\n')

    # Read in (skip) first 8 lines of header info
    for i in range(8):
        header_line = data.readline()

    # Read in the data lines of the file and assign SWAT variables
    data_lines = data.readlines()
    for line in data_lines:
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

        # daymet regards every year as non-leap year,
        # thus for leap year, simply copy the data of day-28 to day-29
        k = 1
        if (((year % 4 == 0 and year % 100 != 0) or year % 400 == 0) and jday == 28):
            k = 2

        for j in range(k):
            SWAT_prcp_file.writelines(prcp + '\n')
            SWAT_temp_file.writelines(tmax + "," + tmin + '\n')
            SWAT_srad_file.writelines(str(srad_convert) + '\n')

    SWAT_prcp_file.close()
    SWAT_temp_file.close()
    SWAT_srad_file.close()
SWAT_list.close()
