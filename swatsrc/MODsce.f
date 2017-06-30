      MODULE scemod
    ! File:   SCEstruct.F90
    ! Author: GANGSHENG WANG - ORNL, MARCH 2015
    !
    ! Created on March 5, 2013, 4:06 PM
    !-----------------------------------------------------------------------------!   
    !SCE-UA Algorithm
        TYPE sSCE_PAR
            INTEGER npar                                    !# of parameters
            INTEGER nopt                                    !# of optimized parameters
            INTEGER nSCE                                    !-> sGLB%iModel: 0-run SWAT; >0: SCEUA optimization, # of SCE runs  
            INTEGER maxn                                    !max no. of trials allowed before optimization is terminated
            INTEGER kstop                                   !number of shuffling loops in which the criterion value must change by the given percentage before optimization is terminated
            INTEGER ngs                                     !number of complexes in the initial population
            INTEGER npt                                     !total number of points in initial population (npt=ngs*npg)
            INTEGER npg                                     !number of points in each complex
            INTEGER nps                                     !number of points in a sub-complex
            INTEGER nspl                                    !number of evolution steps allowed for each complex before complex shuffling
            INTEGER mings                                   !minimum number of complexes required, if the number of complexes is allowed to reduce as the optimization proceeds
            INTEGER ideflt                                  !IF ideflt IS EQUAL TO 0, SET THE SCE CONTROL PARAMETERS TO THE DEFAULT VALUES
            INTEGER iniflg                                  !flag on whether to include the initial point in population; = 0, not included;  = 1, included
            INTEGER ifprint                                 !flag for controlling print-out after each shuffling loop;= 0, print information on the best point of the population; = 1, print information on every point of the population
            INTEGER iFO1                                    !output file1: summarize the SCEUA & Model parameters
            INTEGER iFO2                                    !output file2: summarize optimal parameter values from nRun
            INTEGER iFO3                                    !output file3: other text output, e.g., all parameter sets tested by SCE
            REAL pcento                                     !percentage by which the criterion value must change in given number of shuffling loops
            REAL objOPT                                     !best objective function value
            INTEGER, ALLOCATABLE:: iOpt(:)                  !index of optimized parameters
            REAL, ALLOCATABLE:: parINI(:)                   !initial values
            REAL, ALLOCATABLE:: parMIN(:)                   !lower bound values
            REAL, ALLOCATABLE:: parMAX(:)                   !upper bound values
            REAL, ALLOCATABLE:: parOPT(:)                   !best parameter values 
            CHARACTER(LEN = 10), ALLOCATABLE:: parName(:)   !parameter names
        END TYPE sSCE_PAR
      END MODULE scemod

!!2014-07-14----------------------------------------------- 
!! !-----------------------------------------------------------------------------!     
!!        TYPE sSCE_GLOBAL  !!Global Variables for Model Calibration
!!            INTEGER itype_opt                               !!type of data for calibration: 0-daily flow(m3/s), 2-daily reservoir storage(10^4 m3), 3-daily soil water(mm), 10-monthly flow(m3/s), 11-monthly HUC8 runoff(mm), 20-monthly nutrient all(21+22+23), 21-sediment(tons), 22-TN|NO2+NO3|NO3(tons),23-TP(tons)
!!            INTEGER nsub_opt                                !!# of subbasins included in calibration !!noptsub,icodesub,icoderes,ndaysim,idaysim
!!            INTEGER icode_sub                               !!code of the subbasin with flow gage/reservoir, etc.
!!            INTEGER icode_res                               !!code of the reservoir in SWAT, ATTENTION, different from the code of corresponding subbasin
!!            INTEGER nyear_warmup                            !!# of years for SWAT warm-up
!!            INTEGER nday_sim                                !!(should = nday_sim0, but computed by different ways); # of days for simulation, particularly, for calibration, counting through SWAT simulations 
!!            INTEGER iday_sim                                !!the ith day in nday_sim
!!            INTEGER imon_sim                                !!the ith month in nday_sim
!!            INTEGER iyear0                                  !!Beginning year of simulation, = IYR in 'file.cio'
!!            INTEGER nyear_sim                               !!# of years simulated, excluding 'nyear_warmup'
!!            INTEGER nday_sim0                               !!=nday_sim, # of days simulated, computed by iyear0,nyear_sim, nyear_warmup
!!            INTEGER nmon_sim                                !!# of months simulated,excluding warmup
!!            INTEGER iSWAT                                   !!the ith SWAT simulation run: call functn(npar, x) in 'SCEfunctn.f'
!!            INTEGER iModel                                  !!0: run SWAT; >0: SCEUA optimization
!!            REAL rOBJ0                                      !!sGLB%rOBJ0  = 0-NSE; 1-MARE; 2-NRMSE
!!            REAL, DIMENSION(3) :: wOBJ                      !!wOBJ(1:3),weighting factors for 1st 3 objectives, wOBJ(>3) = wOBJ(3)
!!            INTEGER, DIMENSION (:), ALLOCATABLE :: ivar_par !!variation method for parameter: 1-absolute value; 2-relative value, multiplied by a factor
!!            INTEGER, DIMENSION (:), ALLOCATABLE :: iopt_par !!the ith parameter: optimize or not: 1-optimize, 0-keep original value
!!            INTEGER, DIMENSION (:), ALLOCATABLE :: isub_opt !![nsub_opt]: subbasin IDs to be calibrated 
!!            INTEGER, DIMENSION (:), ALLOCATABLE :: iGateDam !!0-SubBasin,1-FloodGate,2-Res/Dam
!!            REAL, DIMENSION (:), ALLOCATABLE :: xua         !!parameter values passed into SWAT
!!            REAL, DIMENSION (:), ALLOCATABLE :: subsurlag   !!surlag for each subbasin, read 'sub.lag' 
!!            REAL, DIMENSION (:), ALLOCATABLE :: cwus        !!wateruse
!!            REAL, DIMENSION (:), ALLOCATABLE :: crwus       !!wateruse coef
!!            REAL, DIMENSION (:), ALLOCATABLE :: par_min     !!min of absolute parameter values
!!            REAL, DIMENSION (:), ALLOCATABLE :: par_max     !!max of absolute parameter values
!!
!!
!!            REAL, DIMENSION (:), ALLOCATABLE :: sub_area   !!subbasin area (km2), read 'sub_area.txt'
!!            REAL, DIMENSION (:,:), ALLOCATABLE :: data_sim !!simulated time series, may include multiple variables, e.g., multiple subbasins within a HUC8 
!!            REAL, DIMENSION (:,:), ALLOCATABLE :: data_obs !!observed time series for calibration
!!        END TYPE sSCE_GLOBAL
!!        
!!        TYPE (sSCE_GLOBAL) sGLB  !!Global structure used for SWAT optimization
!!!-----------------------------------------------------------------------------!

!!2014-07-14-----------------------------------------------


