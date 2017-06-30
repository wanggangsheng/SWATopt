# This program edits the *.res file in SWAT format
# Gangsheng Wang, ORNL
# July 8, 2014

import os
import fileinput

##dir_cur = "~\\Dropbox\\TRB\\Data\\Dam\\data_covert\\"
dir_cur = "/Users/wg4/Dropbox/TRB/Data/Dam/data_covert/"

##def replace_line(file_name, line_num, column_num, text):
##    lines = open(file_name, 'r').readlines()
##    lines[line_num] = text
##    out = open(file_name, 'w')
##    out.writelines(lines)
##    out.close()

RES_list = open(dir_cur + "TRB_RES_par.txt", 'r')  ## Swap out SWATlist with SWATlist_one.txt to get data read for one station.  ##list of SWAT station created with dos dir /B > SWATlist.txt

for i in range(2):  ##i.e., [0 1], first 2 lines
    RES_par_head = RES_list.readline()
    
RES_par = RES_list.readlines() ##read all data
len_max = 7

print
print (">>>>>>>>>>>>>>>>>>>>>>>>>>")
print ("Start Writing RES parameters FOR SWAT RSERVOIR...")
for RES in RES_par:
    RES0 = RES.strip()
    RES1 = RES0.split('\t')
    RES_name0 = RES1[1].strip()     ##reservoir name
    RES_SUB  = RES1[5].strip()
    MORES    = int(RES1[15])
    IYRES    = int(RES1[14])
    RES_ESA  = float(RES1[9])
    RES_EVOL = float(RES1[10])
    RES_PSA  = float(RES1[11])
    RES_PVOL = float(RES1[12])
    IRESCO   = int(RES1[19])
    RES_RR   = float(RES1[13])
    IFLOD1R  = int(RES1[16])
    IFLOD2R  = int(RES1[17])
    NDTARGR  = int(RES1[18])
    RES_VOL  = float(RES1[20])
    WURTNF   = float(RES1[22])
    WURTNF   = float(RES1[23])
    
    len1 = len(RES_name0)  ##length of string

    #print len1, len2
    if len1 > len_max:
        RES_name1 = RES_name0[0: len_max]
    else:
        RES_name1 = RES_name0   
    
    RESDAYO = 'O_' + RES_name1 + '.dat'  ##Outflow, len of filename <=13
    print (RESDAYO)
    file_RES_SUB = RES_SUB.zfill(5)+'0000.res'
    print(file_RES_SUB)
    print ()
##    print (RES_file_name)
    file_RES = dir_cur + "resfile/" + file_RES_SUB
    SWAT_RES_file = open(file_RES, 'r')
    lines = SWAT_RES_file.readlines()
    SWAT_RES_file.close()
    
##    for line in fileinput.input(SWAT_RES_file,inplace = 1):
##        print line
##    for i, line in enumerate(lines):
##        if i == 2:
##            print (line)
##            SWAT_RES_file.write()
    iLine = 3 ##MORES
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16d'%MORES + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])
    
    iLine = 4 ##IYRES
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16d'%IYRES + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])
    
    iLine = 5 ##RES_ESA
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16.1f'%RES_ESA + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])
    
    iLine = 6 ##RES_EVOL
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16.1f'%RES_EVOL + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])

    iLine = 7 ##RES_PSA
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16.1f'%RES_PSA + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])
    
    iLine = 8 ##RES_PVOL
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16.1f'%RES_PVOL + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])

    iLine = 9 ##RES_VOL
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16.1f'%RES_VOL + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])

    iLine = 14 ##IRESCO
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16d'%IRESCO + lines[iLine - 1][16:len1]
    
    iLine = 23 ##RES_RR
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16.1f'%RES_RR + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])
    
    iLine = 25 ##IFLOD1R
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16d'%IFLOD1R + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])
    
    iLine = 26 ##IFLOD2R
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16d'%IFLOD2R + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])
    
    iLine = 27 ##NDTARGR
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%16d'%NDTARGR + lines[iLine - 1][16:len1]
##    print(lines[iLine - 1])
    
    iLine = 32 ##RESDAYO
    len1 = len(lines[iLine - 1])
    lines[iLine - 1] = '%s'%RESDAYO + lines[iLine - 1][13:len1] #align to left, file name <=13 letters
##    print(lines[iLine - 1])

##    SWAT_RES_file.seek(0,0)
##    for line in lines:
##        print(line)

    with open(file_RES,'w') as SWAT_RES_file:
        SWAT_RES_file.writelines(lines)
    
                
    SWAT_RES_file.close()
 
RES_list.close()
        
