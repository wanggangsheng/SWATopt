# This program reads the output.rsv file in SWAT format
# Gangsheng Wang, ORNL
# July 8, 2014

import os
import datetime

dir_cur = os.getcwd()  
dir_cur = dir_cur + "/"
print(dir_cur + "\n")

RES_list = open(dir_cur + "TRB_RES_par.txt", 'r')  ## Swap out SWATlist with SWATlist_one.txt to get data read for one station.  ##list of SWAT station created with dos dir /B > SWATlist.txt
##RSV = open(dir_cur + 'rsv/output.rsv','r') ##SWAT output.rsv
RSV_file = dir_cur + 'rsv/output.rsv'

for i in range(2):  ##i.e., [0 1], first 2 lines
    RES_par_head = RES_list.readline()
    
RES_par = RES_list.readlines() ##read all data
len_max = 7

print
print (">>>>>>>>>>>>>>>>>>>>>>>>>>")
print ("Start Writing RES parameters FOR SWAT RSERVOIR...")
for RESpar in RES_par:
    RES0 = RESpar.strip()
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
    RES_ID   = int(RES1[21])
##    print(RES_ID)
    
    len1 = len(RES_name0)  ##length of string

    #print len1, len2
    if len1 > len_max:
        RES_name1 = RES_name0[0: len_max]
    else:
        RES_name1 = RES_name0   
    
    RESDAYO_sim_fname = 'V_' + RES_name1 + '.txt'  ##Outflow, len of filename <=13
    print (RESDAYO_sim_fname)
    print ()
    RESDAYO_sim_file = dir_cur + 'rsv/' + RESDAYO_sim_fname  ##file to write VOL_sim
    RESDAYO_sim = open(RESDAYO_sim_file,'w')

    RESDAYO_obs_fname = 'V_' + RES_name1 + '.dat'
    RESDAYO_obs_file = dir_cur + 'output/'+ RESDAYO_obs_fname ##observed RESDAYO
    RESDAYO_obs = open(RESDAYO_obs_file, 'r')
    date_begin0 = RESDAYO_obs.readline().strip()  ##first date for observations
##    print(date_begin0)
##    print(date_begin0[0:4],date_begin0[4:6],date_begin0[6:8])
    date_begin = date_begin0[0:4] + '-' + date_begin0[4:6] + '-' + date_begin0[6:8]
    RESDAYO_sim.write(date_begin + '\n')
    RESDAYO_sim.write('%8s\t%8s\n'%('obs','sim'))
    
##    file_RES_SUB = RES_SUB.zfill(5)+'0000.res'
##    print(file_RES_SUB)
##    file_RES = dir_cur + "resfile/" + file_RES_SUB
    SWAT_RSV = open(RSV_file,'r') ##SWAT output.rsv
        #skip the first n lines
    for i in range(9):  ##i.e., [0:8], skip first 9 lines
        line_tmp = SWAT_RSV.readline()
    lines = SWAT_RSV.readlines()
    SWAT_RSV.close()

    for line in lines:
        var0 = line.strip()
        var1 = var0.split()  ##1 split, 2 pieces
##        for var1_data in var1:
##            print(var1_data)
        
        RESid = int(var1[1])
        if RESid == RES_ID:
            VOL_sim = float(var1[3])*1.0E-4  ##convert units from m^3 to 10^4 m^3
            VOL_obs = float(RESDAYO_obs.readline())
            RESDAYO_sim.writelines('%8d'%VOL_obs + '\t' + '%8d'%VOL_sim + '\n')
 
    RESDAYO_sim.close()
    RESDAYO_obs.close()
    


##    SWAT_RES_file.seek(0,0)
##    for line in lines:
##        print(line)

##    with open(file_RES,'w') as SWAT_RES_file:
##        SWAT_RES_file.writelines(lines)

 
RES_list.close()
        
