# This program reformats RES_OUTFLOW data to SWAT format
# Gangsheng Wang, ORNL
# July 7, 2014

import os

dir_cur = os.getcwd()  
dir_cur = dir_cur + "/"
print(dir_cur + "\n")
RES_list = open(dir_cur + "TRB_Dam.txt", 'r')  ## Swap out SWATlist with SWATlist_one.txt to get data read for one station.  ##list of SWAT station created with dos dir /B > SWATlist.txt

RES_filename = RES_list.readlines() ##read all station file names
len_max = 7

print
print (">>>>>>>>>>>>>>>>>>>>>>>>>>")
print ("START FORMAT DATA FOR SWAT RSERVOIR outflow & storage...")
for RES_file in RES_filename:
    RES_file0 = RES_file.rstrip()
    print (RES_file0)
    print () 
    filename = dir_cur + "input/" + RES_file0+".txt"

    # Define first line of output file
    first_line = '19850101' ##DAYMET starts from 01/01/1980
    # Define name of output file for each varible
    len1 = len(RES_file0)  ##length of string

    #print len1, len2

    if len1 > len_max:

        RES_file1 = RES_file0[0: len_max]

    else:

        RES_file1 = RES_file0   
    RES_file_name = 'O_' + RES_file1 + '.dat'  ##Outflow, len of filename <=13
    VOL_file_name = 'V_' + RES_file1 + '.dat'  ##Volume

##    print (RES_file_name)
    SWAT_RES_file = open(dir_cur + "output/" + RES_file_name, 'w')
    SWAT_VOL_file = open(dir_cur + "output/" + VOL_file_name, 'w')
 
    SWAT_RES_file.write(first_line + '\n')
    SWAT_VOL_file.write(first_line + '\n')
 
    data = open(filename, 'r+')           
    #skip the first n lines
    for i in range(2):  ##i.e., [0 1], first 2 lines
        data_lines_tmp = data.readline()
        
    #Read in the data lines of the file and assign SWAT variables
    data_lines = data.readlines()
    for line in data_lines:
        var0 = line.rstrip()
        var1 = var0.split('\t', 1)  ##1 split, 2 pieces

        outflow = float(var1[1])
        vol_res = float(var1[0])
        SWAT_RES_file.writelines('%8.2f'%outflow + '\n')
        SWAT_VOL_file.writelines('%15.4f'%vol_res + '\n')
               
    SWAT_RES_file.close()
    SWAT_VOL_file.close()
 
RES_list.close()
        
