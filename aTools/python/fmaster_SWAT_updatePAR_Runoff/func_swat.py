class swrite_swat_par:
    ## class as Argument in FUNCTION fwrite_swat_par(sArg)
    file_in = None
    file_out = None
    npar = None
    option = None
    irow = None
    par_val = None
    par_len = None
    par_pos0 = None
    par_format = None
    
def fwrite_swat_par(swrite_swat_par):
    #file_in: original file
    #file_out: new file with modified parameters
    #npar: # of parameters to be modified in each file
    #Array with size of npar: option,irow,par_val,par_len,par_pos0,par_format
    #option: 1: one par value at 1 line; 2: multiple par values at 1 line (e.g., AWC)
    #irow: row No. of the parameter
    #par_val: new value for the parameter
    #par_len: length of the value, i.e., # of characters including spaces
    #par_pos0: start position (often the leading space) for the parameter value
    #par_format: data format, e.g., "%16.4f", "%16d",'%s'

    import numpy as np

    option = swrite_swat_par.option
    file_in = swrite_swat_par.file_in
    file_out = swrite_swat_par.file_out
    npar = swrite_swat_par.npar
    irow = swrite_swat_par.irow
    par_val = swrite_swat_par.par_val
    par_len = swrite_swat_par.par_len
    par_format = swrite_swat_par.par_format
    par_pos0 = swrite_swat_par.par_pos0
##    print('file_in = ',file_in)

    ##the argument "errors" to avoid decoding problem
    ##e.g., in Line 20 of *.rte, the Greek letter "mu" cannot be decoded
    SWAT_file = open(file_in,'r',encoding='utf-8',errors='ignore')  
    lines = SWAT_file.readlines()  
    SWAT_file.close()
       
    for i in range(npar):
        j = irow[i] - 1
        len1 = len(lines[j])
        if option[i] == 2: ##multiple layers for 1 PAR, e.g., *.sol
##            print ("This PAR has values in multiple layers!!!")
            
            for k in range(len1):
                if(lines[j][k]==":"):
                    par_pos0 = k+1
                    
            par_all0 = lines[j][par_pos0:len1]
            par_all1 = par_all0.strip()
            par_all2 = par_all1.split() #'s':space
            par_all2 = np.asarray(par_all2)  #covert LIST to ARRAY
            par_all2 = par_all2.astype(np.float) #convert to FLOAT data
            len2 = len(par_all2)  ## of layers
##            print("par1 = ", par_all1)
##            print("par2 = ", par_all2)
##            print("len2 = ", len2)
            par_all3 = np.full(len2,0,dtype = np.float)
            par_all_new = np.full(len2,0,dtype = np.float)
            par_all_new[0] = par_val[i]
            if par_all2[0] != 0: 
                for k in range(1,len2):
                    par_all3[k] = par_all2[k]/par_all2[0]  #compute the ratios
                    par_all_new[k] = par_val[i]*par_all3[k]
##                    print(k,par_all2[k],par_all2[0],par_all3[k],par_all_new[k])

            line_new = lines[j][:par_pos0]
            for k in range(len2):
                line_new = line_new+(par_format[i])%(par_all_new[k])
            lines[j] = line_new + "\n"           
            
        else:  ##option[i]<>2, 1 layer for 1 PAR, e.g., *.gw, sub.lag
            line_new = lines[j][:(par_pos0[i]-1)]
            pos1 = (par_pos0[i] - 1 + par_len[i])
            lines[j] = line_new+(par_format[i])%(par_val[i]) + lines[j][pos1:len1]
    ##END: for i in range(npar)
            
    with open(file_out,'w') as SWAT_file:
        SWAT_file.writelines(lines)                  
    SWAT_file.close()
##END: def fwrite_swat_par(swrite_swat_par)
