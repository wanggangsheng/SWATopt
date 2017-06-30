## This program edits the SWAT parameter values in SWAT format
## Gangsheng Wang, ORNL
## Feb 23, 2015
##--------------------------------------------------------------------##
##READ ME:
##The Main Master File Name MUST be: "master.ini", which will be read first
##"master.ini" MUST be put in the same directory (dir_master) as the MAIN python script
##The other 4 Master Files defined in "master.ini" MUST also be put in dir_master
##The other 4 Master Files:
##(1) Line 3: HUC8_PAR: parameter values for each HUC8; these parameters will be modified.
##(2) Line 5: HUC8_SUB: subbasin IDs in each HUC8; all HUC8 & their SWAT SUB (NOT limited to HUC8 in HUC8_PAR) should be included, NO sorting required.
##(3) Line 7: SUB_HRU: # of HRU in each subbasin; all SWAT SUB must be included and Sorted Ascending.
##(4) Line 9: PAR_info: SWAT parameter (NOT limited to PAR in HUC8_PAR) info; all PAR in HUC8_PAR MUST be included.
##The 2 Directories defined in "master.ini":
##(i) Line 11: dir_in: for Original SWAT Files
##(ii)Line 13: dir_out: for Updated SWAT Files
##--------------------------------------------------------------------##
import os
import sys
import time
import shutil
import numpy as np
from numpy import *
#import fileinput
# from func_swat import * #CLASS:swrite_swat_par; FUNCTION: fwrite_swat_par
##--------------------------------------------------------------------##
t_start = time.time()
dir_master = os.path.dirname(sys.argv[0])  ##sys.argv[0] is the python script to be executed
dir_master = dir_master + "/"  ##"/Users/wg4/Dropbox/TRB/python/fmaster"

file_in = dir_master + "output.std"
file_out = dir_master + "crop_yield.out"
fout = open(file_out, 'w')
# SWAT_file = open(file_in,'r',encoding='utf-8',errors='ignore')  
SWAT_file = open(file_in,'r')  
lines = SWAT_file.readlines()  
SWAT_file.close()
nline = len(lines)
# print('nLine = ',nline)

nField = 7      ## "SWCH  Yld =    19.0 BIOM =  17814.8"
nField_HRU_SUB = 4 ## "HRU    1376 SUB  21"
##ATTENTION: text[0:4] = text[0] : text[3]

crop_name = 'SWCH'
print('CROP','SUB','HRU','YEAR','YIELD','BIOMASS')
fout.write("%10s %10s %10s %10s %10s %10s\n"%('CROP','SUB','HRU','YEAR','YIELD','BIOMASS'))
format_out = "%10s %10s %10s %10d %10s %10s\n"

for i in range(nline):
    text = lines[i].split()
    # print(i, len(text),text)
    len_text = len(text)
    # if len_text > 0:
    if len_text > 4 and text[0] == 'HRU' and text[4] == crop_name:
       	nyear = (len_text - nField_HRU_SUB)/nField
       	text2 = lines[i+1].split()
       	len_text2 = len(text2)
       	nyear2 = len_text2/nField
        for j in range(nyear):
        	jbeg = nField_HRU_SUB + j*nField
        	jend = jbeg + nField 
        	newline = crop_name + text[3] + text[1] + str(j + 1) + text[jbeg+3] + text[jbeg+6] + '\n'
        	print(text[0:nField_HRU_SUB], j + 1, text[jbeg:jend])
        	# print(crop_name, text[3], text[1], j + 1, text[jbeg+3],text[jbeg+6])
        	fout.write(format_out%(crop_name, text[3], text[1], j + 1, text[jbeg+3],text[jbeg+6]))
        for j in range(nyear2):
        	jbeg = j*nField
        	jend = jbeg + nField
        	print(text[0:nField_HRU_SUB], nyear + j + 1, text2[jbeg:jend])
        	# print(crop_name, text[3],text[1], nyear + j + 1, text2[jbeg+3],text2[jbeg+6])
        	fout.write(format_out%(crop_name, text[3], text[1], nyear + j + 1, text2[jbeg+3],text2[jbeg+6]))

fout.close()    
        # elif text[0] == crop_name:
        #     print(text)


# file_master_HUC8_PAR = dir_master + lines[2].strip()
# file_master_HUC8_SUB = dir_master + lines[4].strip()  ##SubID in each HUC8, all HUC8 & their SWAT SUB should be included, NO sorting required
# file_master_SUB_HRU = dir_master + lines[6].strip()   ##no. of HRU in each SUB, all SWAT SUB must be included and Sorted Ascending
# file_master_PAR = dir_master + lines[8].strip()

# dir_in = lines[10].strip() ##"/Users/wg4/Dropbox/TRB/SWAT2012opt/" 
# dir_out = lines[12].strip() ##
# if not os.path.exists(dir_out):
#     os.makedirs(dir_out)

# print(">>>This Program Updates SWAT PAR Values in Files...")
# print("imDirectory for Master Files:",dir_master)
# print(">>>Master Files:")
# print(file_master)
# print(file_master_HUC8_PAR)
# print(file_master_HUC8_SUB)
# print(file_master_SUB_HRU)
# print(file_master_PAR)
# print()
# print("Input Dir = ",dir_in)
# print("Output Dir = ",dir_out)
# print()
# dir_in = dir_in + "/"
# dir_out = dir_out + "/"
# shutil.copy2(dir_in+"sub.lag", dir_out)  ##"sub.lag" will be modified continuously

# ##nPAR<=nPAR_all
# ##nHUC8<=nHUC8_all
# ##--------------------------------------------------------------------##
# file = open(file_master_HUC8_PAR,'r')
# lines = file.readlines()
# file.close()
# nHUC8 = len(lines) - 1
# nPAR = len(lines[0].split()) - 1 ##parameter names
# PAR_name = (lines[0].split())[1:]
# HUC8_PAR = np.empty([nHUC8,nPAR+1],dtype=np.object) ##1st column: HUC8
# for i in range(nHUC8):
#     line = lines[i+1].strip()
#     HUC8_PAR[i] = line.split()
# print("# of PAR = ",nPAR)
# print("# of HUC8 = ",nHUC8)
# ##print(nHUC8,nPAR,PAR_name)
# ##print(HUC8_PAR[0,:])
# ##--------------------------------------------------------------------##
# file = open(file_master_PAR,'r')
# lines = file.readlines()
# file.close()
# nPAR_all = len(lines) - 1##1st line includes field names, 
# if(nPAR_all<nPAR):
#     print("ERROR: # of PAR in file_master_PAR = ",nPAR_all, " < # of PAR in file_master_HUC8_PAR = ", nPAR)
# ##PAR_info = np.empty([nPAR_all,7],dtype=np.object)
# PAR_info = np.empty([nPAR,7],dtype=np.object) ##nPAR<=nPAR_all
# k = -1
# for i in range(1,nPAR_all+1):
#     line = lines[i].strip()
# ##    PAR_info[i - 1] = line.split()  ##['1' 'CN2' 'mgt' '11' '16.2' 'HRU' '1']
#     data1 = line.split() 
#     for j in range(nPAR):
#         if(data1[1]==PAR_name[j]):
#             k = k + 1
#             PAR_info[k] = data1
# ##print("PAR_info = ", PAR_info[0,:]) ##PAR_info only includes those PAR in HUC8_PAR

# SWAT_type,nPAR_SWATtype = unique(PAR_info[:,2],return_counts=True)  ##numpy, SWAT file types, e.g., *.mgt, *.gw, *.sol
# nSWAT_type = len(SWAT_type)  ##no. of SWAT file types
# print("SWAT File Type = ",SWAT_type)
# print("# of PAR in each Type = ",nPAR_SWATtype)
# #nPAR_SWATtype = np.zeros(nSWAT_type)  #no. of parameters in each SWAT file type
# SWAT_unit = np.empty(nSWAT_type,dtype=np.object)  ##SUB or HRU
# for i in range(nPAR): ##nPAR_all
#     for j in range(nSWAT_type):
#         if(PAR_info[i,2]==SWAT_type[j]):
#             SWAT_unit[j] = PAR_info[i,5] ##SUB or HRU
# print("Update SUB/HRU = ",SWAT_unit)
# ##--------------------------------------------------------------------##    

# file = open(file_master_HUC8_SUB,'r')
# lines = file.readlines()
# file.close()
# nHUC8_all = len(lines) - 1
# print("# of PAR in file_master_PAR = ",nPAR_all)
# print("# of HUC8 in file_master_HUC8_SUB = ",nHUC8_all)
# ##HUC8_SUB = np.empty([nHUC8_all,3],dtype=np.object) ##HUC8	nSub	SubID
# HUC8_SUB = np.empty([nHUC8,3],dtype=np.object)
# k = -1
# for i in range(nHUC8_all):
#     line = lines[i+1].strip()
#     data1 = line.split('\t') #use "\t" to ensure correctly reading multiple SubID: '3,6,8'
#     for j in range(nHUC8):
#         if(data1[0]==HUC8_PAR[j,0]):
#             k = k + 1
#             HUC8_SUB[k] = data1
# ##print(HUC8_SUB[0,:]) ##HUC8_SUB only includes those HUC8 in HUC8_PAR
# ##--------------------------------------------------------------------##    

# file = open(file_master_SUB_HRU,'r')
# lines = file.readlines()
# file.close()
# nSUB_all = len(lines) - 1
# print("# of SUB in file_master_SUB_HRU = ", nSUB_all)
# SUB_HRU = np.empty([nSUB_all,2],dtype=np.int) ##Subbasin	nHRU
# for i in range(nSUB_all):
#     line = lines[i+1].strip()
#     SUB_HRU[i] = line.split() 
# #print(SUB_HRU)
# ##--------------------------------------------------------------------##
# print()
# print("Ready to Write SWAT PAR...")
# ##print("Press any key to continue...")
# ##a=input()
# sArg = swrite_swat_par()  ## class as Argument in FUNCTION fwrite_swat_par(sArg)
# ##for i in range(2,3): ##ONLY write "sub.lag"
# for i in range(nSWAT_type):  #SWAT file types, e.g., *.gw, *.sol
# ##PAR_info[0,:] = ['1' 'CN2' 'mgt' '11' '16.2' 'HRU' '1']
# ##    print(i,SWAT_type[i])
#     print()
#     t_start1 = time.time()
#     sArg.npar = nPAR_SWATtype[i]  ###sArg
#     iROW_PAR_info = np.empty(sArg.npar,dtype=np.int)
#     iCOL_HUC8_PAR = np.empty(sArg.npar,dtype=np.int)
#     k = -1
#     for j in range(nPAR):
#         if(PAR_info[j,2]==SWAT_type[i]):
#             k = k + 1
#             iROW_PAR_info[k] = j
#     k = -1
#     for j1 in range(sArg.npar):
#         iROW = iROW_PAR_info[j1] 
#         for j2 in range(nPAR):              
#             if(PAR_name[j2]==PAR_info[iROW,1]):
#                 k = k + 1
#                 iCOL_HUC8_PAR[k] = j2 + 1  #1st column in HUC8_PAR is HUC8

#     sArg.option = np.empty(sArg.npar,dtype=np.int)   ###sArg
#     sArg.irow = np.empty(sArg.npar,dtype=np.int)     ###sArg
#     sArg.par_val = np.empty(sArg.npar,dtype=np.float)###sArg
#     sArg.par_len = np.empty(sArg.npar,dtype=np.int)  ###sArg
#     sArg.par_pos0 = np.empty(sArg.npar,dtype=np.int) ###sArg
#     sArg.par_format = np.empty(sArg.npar,dtype=np.object) ###sArg
#     ##PAR_info[0,:] = ['1' 'CN2' 'mgt' '11' '16.2' 'HRU' '1']
#     for j1 in range(sArg.npar):
#         iROW = iROW_PAR_info[j1]
#         sArg.option[j1] = PAR_info[iROW,6]    ###sArg, OK for 'LAG'
#         sArg.irow[j1] = PAR_info[iROW,3]      ###sArg
#         par_format = PAR_info[iROW,4].strip()
#         sArg.par_len[j1] = int(par_format[:2]) ###sArg, OK for 'LAG'
#         sArg.par_pos0[j1] = 1 ##no-use currently ###sArg
#         sArg.par_format[j1] = '%'+ par_format+'f' ###sArg, OK for 'LAG'  
# ##        print(par_format,sArg.par_format[j1])
        
#     for j in range(nHUC8):
#         for j1 in range(sArg.npar):
#             iCOL = iCOL_HUC8_PAR[j1]
#             sArg.par_val[j1] = HUC8_PAR[j,iCOL] ###sArg
                       
#         data1 = HUC8_SUB[j,2].strip()  ##SubID in each HUC8
#         SUBid = data1.split(",")
#         nSUB1 = len(SUBid)
#         for j1 in range(nSUB1):
#             SUBid[j1] = SUBid[j1].strip()
#         #print(nSUB1,SUBid)
#         for k in range(nSUB1):
#             nHRU = SUB_HRU[int(SUBid[k])-1,1]  ##SUBid begins with 1
# ##            print(SWAT_type[i],SWAT_unit[i],HUC8_PAR[j,0],'SubID = ',SUBid[k],'nHRU = ',nHRU)

#             if(SWAT_unit[i]=='SUB'):
#                 fname1 = SUBid[k].zfill(5) + '0000.' + SWAT_type[i]
# ##                print(SWAT_type[i],HUC8_PAR[j,0],fname1)
#                 sArg.file_in = dir_in + fname1  ###sArg
#                 sArg.file_out = dir_out + fname1  ###sArg
# ##                print (sArg.__dict__)
#                 fwrite_swat_par(sArg)
#             elif (SWAT_unit[i]=='LAG'):  ##1 file: "sub.lag"
#                 fname1 = 'sub.lag'
#                 sArg.file_in = dir_out + fname1
#                 sArg.file_out = dir_out + fname1

#                 for j1 in range(sArg.npar):  ##Actually, sArg.npar = 1
# ##                    iROW = iROW_PAR_info[j1]
# ##                    sArg.option[j1] = PAR_info[iROW,6]    ###sArg, OK for 'LAG'
#                     sArg.irow[j1] = int(SUBid[k]) + 1      ###sArg
# ##                    par_format = PAR_info[iROW,4].strip()
# ##                    sArg.par_len[j1] = int(par_format[:2]) ###sArg, OK for 'LAG'
#                     sArg.par_pos0[j1] = 6 ###sArg
#                     fwrite_swat_par(sArg)
# ##                    sArg.par_format[j1] = '%'+ par_format+'f' ###sArg, OK for 'LAG' 
#             else:  ## =='HRU'
#                 for m in range(nHRU):
#                     fname1 = SUBid[k].zfill(5) + str(m+1).zfill(4) + '.' + SWAT_type[i]
# ##                    print(SWAT_type[i],HUC8_PAR[j,0],fname1)
#                     sArg.file_in = dir_in + fname1  ###sArg
#                     sArg.file_out = dir_out + fname1  ###sArg
#                     #print (sArg.__dict__)
#                     fwrite_swat_par(sArg)
#             ##END: if(SWAT_unit[i]=='SUB'):
#         ##END: for k in range(nSUB1):
#     ##END: for j in range(nHUC8):
#     t_end1 = time.time()
#     print(i,SWAT_type[i],SWAT_unit[i],"; Time Cost = ",'%10.2f'%(t_end1-t_start1),"Seconds")
##END: for i in range(nSWAT_type):
                    
t_end = time.time()
print()
print("Mission Completed!!! Running Time = ",'%10.2f'%(t_end-t_start),"Seconds")
        

##--------------------------------------------------------------------##

##--------------------------------------------------------------------##

##--------------------------------------------------------------------##
##Test with a single file
##sArg = swrite_swat_par()
##sArg.file_in = dir_cur + "finput/"+"000010000.rte"
##sArg.file_out = dir_cur + "foutput/"+"000010000.rte"
##sArg.npar = 2
##sArg.irow = [6, 7]
##sArg.option = np.full(sArg.npar, 1, dtype = np.int)
##sArg.par_val = [0.19,97.87]
##sArg.par_len = np.full(sArg.npar, 14, dtype = np.int) # = [16,16,16,16,16]
##sArg.par_pos0 = np.full(sArg.npar, 0, dtype = np.int)
##sArg.par_format = np.full(sArg.npar, '%14.3f', dtype = np.object)
##
####if os.path.isfile(sArg.file_in) and os.access(sArg.file_in, os.R_OK):
####    print ("File exists and is readable")
####else:
####    print ("Either file is missing or is not readable")
##fwrite_swat_par(sArg)
##print("Test Completed!!!")
##print("Press any key to continue")
##a=input()
##--------------------------------------------------------------------##

##option = 1
##file_in = dir_cur + "finput/"+"000010001.gw"
##file_out = dir_cur + "foutput/"+"000010001.gw"
##npar = 5
##irow = [5, 4, 9, 7, 11]
##par_val = [0.756,26.093,0.657,0.15,0.286]
##par_len = np.full(npar, 16, dtype = np.int) # = [16,16,16,16,16]
##par_pos0 = np.full(npar, 0, dtype = np.int)
##par_format = np.full(npar, '%16.4f', dtype = np.object)
###dataformat = ['%16.4f','%16.4f','%16.4f','%16.4f','%16.4f']
##
##fwrite_swat_par(option,file_in,file_out,npar,irow,par_val,par_len,par_format)
##print("*.gw")


##file_in = dir_cur + "finput/"+"000010001.sol"
##file_out = dir_cur + "foutput/"+"000010001.sol"
##npar = 2
##irow = [10, 11]
##option = np.full(npar, 2, dtype = np.int)
##par_val = [0.141,21.465]
##par_len = np.full(npar, 12, dtype = np.int) # = [16,16,16,16,16]
##par_pos0 = np.full(npar, 0, dtype = np.int)
##par_format = np.full(npar, '%12.2f', dtype = np.object)
###dataformat = ['%16.4f','%16.4f','%16.4f','%16.4f','%16.4f']
##
##sArg = swrite_swat_par()
##sArg.option = option
##sArg.file_in = file_in
##sArg.file_out = file_out
##sArg.npar = npar
##sArg.irow = irow
##sArg.par_val = par_val
##sArg.par_len = par_len
##sArg.par_pos0 = par_pos0
##sArg.par_format = par_format
##
##fwrite_swat_par(sArg)

##fwrite_swat_par(option,file_in,file_out,npar,irow,par_val,par_len,par_format)

