      REAL FUNCTION functn(npar, x)
!! GANGSHENG WANG - ORNL, MARCH 2015
!c  This is the SWAT Function CALLed by SCEUA
        USE parm
!        implicit REAL*8 (a - h, o - z)
        IMPLICIT NONE
        REAL, PARAMETER::dFill = -9999
        INTEGER npar,j,k,jua,jyear,jday_mon,jday_yr
        REAL x(npar)
!        REAL rNSE_mon, rNSE, rCC, rMARE  !rNSE_mon: NSE for monthly data; rNSE: NSE for daily data; rCC: correlation coefficient; rSO: ratio of simulation to observation
        REAL sum11, sum12, sum13, avg11, avg12
        CHARACTER*100 format2106, format2107
        
        REAL fCORR, f1NSE,f1RAVG,fMARE, fSUM, fAVG,fHMLE1,fSUM2,fAVG2 !!FUNCTIONS; f1NSE = 1-fNSE; f1RAVG = abs(1.0-fAVG_sim/fAVG_obs)
        REAL fWAVG,fNRMSE!!FUNCTIONS
        INTEGER nDaysofMon, nDaysofYear !!FUNCTIONs, see "WGSfunc.f"
        INTEGER nArray,nDay,nMon  !!# of months simulated
        INTEGER, DIMENSION (:), ALLOCATABLE :: iday_yr    !!iday of the end of each year
        INTEGER, DIMENSION (:), ALLOCATABLE :: iday_mon   !!iday of the end of each month
        REAL, DIMENSION (:), ALLOCATABLE :: dobs      !!daily observations
        REAL, DIMENSION (:), ALLOCATABLE :: dsim      !!daily observations
        REAL, DIMENSION (:), ALLOCATABLE :: dmon_obs      !!monthly observations
        REAL, DIMENSION (:), ALLOCATABLE :: dmon_sim      !!monthly observations
        REAL, DIMENSION (:), ALLOCATABLE :: dyr_obs      !!monthly observations
        REAL, DIMENSION (:), ALLOCATABLE :: dyr_sim      !!monthly observations
        
        REAL, DIMENSION (:), ALLOCATABLE :: fobj         !!multiple objective function values
        REAL, DIMENSION (:), ALLOCATABLE :: wobj         !!weighting factors for multiple objective function values
        INTEGER nobj,iCASE                                     !!# of obj
!        write(*,*)"No. of Months = ",nMon,sGLB%nmon_sim
!!---------------------------------------------------------------------
        nDay = sGLB%nday_sim0
        nMon = sGLB%nmon_sim !!sGLB%nyear_sim * 12
        iCASE = sGLB%itype_opt

        if(sGLB%pid.eq.0) then
            write(*,'("SPIN-UP: nYEAR=",I5)')sGLB%nyear_warmup
            write(*,'("SWATOPT: nYEAR=",I5,"; nMon=",I5,"; nDay=",I5)') 
     &        sGLB%nyear_sim,nMon,nDay
        end if
  
        ALLOCATE(dobs(nDay))
        ALLOCATE(dsim(nDay))
        ALLOCATE(dmon_obs(nMon))
        ALLOCATE(dmon_sim(nMon))
        ALLOCATE(iday_mon(nMon))
        
        ALLOCATE(iday_yr(sGLB%nyear_sim))
        ALLOCATE(dyr_obs(sGLB%nyear_sim))
        ALLOCATE(dyr_sim(sGLB%nyear_sim))
        
        jday_mon = 0
        jday_yr = 0
        do j = 1,sGLB%nyear_sim
            jyear = sGLB%iyear0 + sGLB%nyear_warmup + j - 1
            jday_yr = jday_yr + nDaysofYear(jyear)
            iday_yr(j) = jday_yr
            do k = 1,12
                jday_mon = jday_mon+nDaysofMon(jyear,k)
                iday_mon(12*(j-1)+k) = jday_mon
!                write(*,*)12*(j-1)+k,jyear,k,jday
            end do
        end do
        
!        write(*,*)x
        sGLB%xua = x
!!---------------------------------------------------------------------
!!SWAT2012_BEGIN
        CALL readfile  !!open output files  
        CALL readbsn
        CALL readwwq
        if (fcstyr > 0 .and. fcstday > 0) CALL readfcst
        CALL readplant             !! read in the landuse/landcover database
        CALL readtill              !! read in the tillage database
        CALL readpest              !! read in the pesticide database
        CALL readfert              !! read in the fertilizer/nutrient database
        CALL readurban             !! read in the urban land types database
        CALL readseptwq            !! read in the septic types database     
        CALL readlup
        CALL readfig   !in this subroutine, CALL readsub->readmgt/readhru
!       CALL readatmodep
        CALL readinpt
        if(sGLB%iModel.EQ.0.or.sGLB%iSWAT.LT.0) then
            !! DO NOT call while doing optimization
            CALL std1    
            CALL std2    
        end if
        CALL openwth
        CALL headout

      !! convert integer to string for output.mgt file
        subnum = ""
        hruno = ""
        do i = 1, mhru
            write (subnum(i),fmt=' (i5.5)') hru_sub(i)
            write (hruno(i),fmt=' (i4.4)') hru_seq(i)  
        end do

        if (isproj == 2) then 
            hi_targ = 0.0
        end if

!! save initial values
        if (isproj == 1) then
            scenario = 2
            CALL storeinitial
        else if (fcstcycles > 1) then
            scenario =  fcstcycles
            CALL storeinitial
        else
            scenario = 1
        endif
      
        if (iclb /= 4) then
            do iscen = 1, scenario
                !! simulate watershed processes
                CALL simulate

                !! perform summary calculations
                CALL finalbal
                CALL writeaa
                CALL pestw
                !!reinitialize for new scenario
                if (scenario > iscen) CALL rewind_init
            end do
        end if
!!SWAT2012_END     
	
!        write(*,*)'check nday: ',sGLB%nday_sim0,sGLB%nday_sim

!        write(*,*)"itype_opt = ",sGLB%itype_opt
	    SELECT CASE(sGLB%itype_opt)
            CASE (0) !!daily streamflow
                nobj = 2!!rNSE,
                ALLOCATE(fobj(nobj))
                ALLOCATE(wobj(nobj))
                call WOBJ_INI(nobj,wobj,3,sGLB%wOBJ)  !!wobj will be normalized in FUNCTION fWAVG()
                dobs = sGLB%data_obs(1,:)
                dsim = sGLB%data_sim(1,:)
                fobj(1) = f1NSE(nDay,dobs,dsim,dFill) !!f1NSE = 1-fNSE
                fobj(2) = f1RAVG(nDay,dobs,dsim,dFill)!!fAVG(nDay,dsim,dFill)/fAVG(nDay,dobs,dFill)
                functn = fWAVG(nobj,wobj,fobj,dFill)
              if(sGLB%iSWAT.lt.0) then
                write(319,*)'Day | Qobs_cms Qsim_cms | ',
     &                      'Qsim_in_cms PCP_mm'
                do j = 1, nDay
                    write(319,*)j,'|',
     &            sGLB%data_obs(1,j),sGLB%data_sim(1,j),"|",   
     &            sGLB%data_sim(2:3,j)
                end do
              end if
            CASE (2) !!RESERVOIR
                !!data_sim:(1)resv,(2)resqsim,(3)resqin,(4)respcp,(5)reseva,(6)ressa
                nobj = 3 !!rNSE,rNSEm,rMARE 
                ALLOCATE(fobj(nobj))
                ALLOCATE(wobj(nobj))
                call WOBJ_INI(nobj,wobj,3,sGLB%wOBJ)  !!wobj will be normalized in FUNCTION fWAVG()
                dobs = sGLB%data_obs(1,:)
                dsim = sGLB%data_sim(1,:)
                fobj(1) = f1NSE(nDay,dobs,dsim,dFill)!!1.0 - fNSE(nDay,dobs,dsim,dFill)
                                 
                dmon_obs(1) = fAVG2(nDay,dobs,1,31,dFill) 
                dmon_sim(1) = fAVG2(nDay,dsim,1,31,dFill) 
                do j = 2, nMon       
                    dmon_obs(j) = fAVG2(nDay,dobs,
     &                         iday_mon(j-1)+1,iday_mon(j),dFill) 
                    dmon_sim(j) = fAVG2(nDay,dsim,
     &                         iday_mon(j-1)+1,iday_mon(j),dFill) 
                end do 
                fobj(2) = f1NSE(nMon, dmon_obs, dmon_sim,dFill)!!1.0 - fNSE(nMon, dmon_obs, dmon_sim,dFill)  !!NSE of monthly averages

                do j = 1,sGLB%nyear_sim
                    dyr_obs(j) = dobs(iday_yr(j))
                    dyr_sim(j) = dsim(iday_yr(j))
                end do
                fobj(3) = fMARE(sGLB%nyear_sim, dyr_obs, dyr_sim,dFill) 
                functn = fWAVG(nobj,wobj,fobj,dFill)
              if(sGLB%iSWAT.lt.0) then  
                write(319,*)"Day | RESVobs_10^4m3 RESVsim_10^4m3 | ",
     &           "RESQobs_cms RESQsim_cms | RESQin_10^4m3 ",
     &           "RESpcp_10^4m3 RESea_10^4m3 RESsa_ha"
                do j = 1, nDay
                write(319,*)j,'|',
     &            (sGLB%data_obs(k,j),sGLB%data_sim(k,j),"|",k=1,2),   !!resv,resq
     &            sGLB%data_sim(3:6,j)
                end do
              end if
            CASE (3) !!daily SW (mm)
                nobj = 2!!rNSE,
                ALLOCATE(fobj(nobj))
                ALLOCATE(wobj(nobj))
                call WOBJ_INI(nobj,wobj,3,sGLB%wOBJ)  !!wobj will be normalized in FUNCTION fWAVG()
                dobs = sGLB%data_obs(1,:)
                dsim = sGLB%data_sim(1,:)
                fobj(1) = f1NSE(nDay,dobs,dsim,dFill)
                fobj(2) = f1RAVG(nDay,dobs,dsim,dFill)!!fAVG(nDay,dsim,dFill)/fAVG(nDay,dobs,dFill)
                functn = fWAVG(nobj,wobj,fobj,dFill)
              if(sGLB%iSWAT.lt.0) then
                write(319,*)"Day | SWobs_mm SWsim_mm | ", 
     &                      "PCP_mm WYLD_mm ETA_mm"
                do j = 1, nDay
                    write(319,*)j,'|',
     &            sGLB%data_obs(1,j),sGLB%data_sim(1,j),"|",   
     &            sGLB%data_sim(2:4,j)
                end do
              end if
            CASE (10)  !! monthly streamflow
                nobj = 2 !!NSEm,MARE
                ALLOCATE(fobj(nobj))
                ALLOCATE(wobj(nobj))
                call WOBJ_INI(nobj,wobj,3,sGLB%wOBJ)  !!wobj will be normalized in FUNCTION fWAVG()

                dmon_obs(:) = sGLB%data_obs(1,:) 
                dmon_sim(:) = sGLB%data_sim(1,:)
                fobj(1) = f1NSE(nMon, dmon_obs, dmon_sim,dFill)!!1.0 - fNSE(nMon, dmon_obs, dmon_sim,dFill)!!rNSE_mon: NSE of monthly data series

                do j = 1,sGLB%nyear_sim
                    dyr_obs(j)=fAVG2(nMon,dmon_obs,
     &                               12*(j-1)+1,12*j,dFill) !!sGLB%data_obs,annual average 
                    dyr_sim(j)=fAVG2(nMon,dmon_sim,
     &                               12*(j-1)+1,12*j,dFill) 
!                    write(*,*)j,dyr_obs(j),dyr_sim(j)
                end do
                fobj(2) = fMARE(sGLB%nyear_sim, dyr_obs, dyr_sim,dFill)   !!rMARE: MARE of yearly data series
                functn = fWAVG(nobj,wobj,fobj,dFill)
!                write(*,*)functn,fobj
              if(sGLB%iSWAT.lt.0) then
                write(319,*)"Month | Qobs_cms Qsim_cms | ",
     &                      "Qsim_in_cms | PCP_mm"
                do j = 1,nMon
                    write(319,*)j,'|',dmon_obs(j),dmon_sim(j),"|",
     &                sGLB%data_sim(2,j),"|",sGLB%data_sim(3,j)               
                end do
              end if
            CASE (11)  !!subbasin monthly runoff
                nobj = 2 !!NSEm,MARE
                ALLOCATE(fobj(nobj))
                ALLOCATE(wobj(nobj))
                call WOBJ_INI(nobj,wobj,3,sGLB%wOBJ)  !!wobj will be normalized in FUNCTION fWAVG()

                dmon_obs(:) = sGLB%data_obs(1,:) 

                do j = 1,nMon
                    sum11 = 0
                    sum12 = 0
                    do k = 1, sGLB%nsub_opt
                        sum11 = sum11 + sGLB%sub_area(sGLB%isub_opt(k))
                        sum12 = sum12 + sGLB%sub_area(sGLB%isub_opt(k))
     &                                *sGLB%data_sim(k,j)
                    end do
                    dmon_sim(j) = sum12/sum11  !!Area-Weighted Average
!                    write(*,*)j,dmon_obs(j),dmon_sim(j)
                end do
                fobj(1) = f1NSE(nMon, dmon_obs, dmon_sim,dFill)!!1.0 - fNSE(nMon, dmon_obs, dmon_sim,dFill)!!rNSE_mon: NSE of monthly data series

                do j = 1,sGLB%nyear_sim
                    dyr_obs(j)=fSUM2(nMon,dmon_obs,
     &                               12*(j-1)+1,12*j,dFill) !!sGLB%data_obs,annual average 
                    dyr_sim(j)=fSUM2(nMon,dmon_sim,
     &                               12*(j-1)+1,12*j,dFill) 
!                    write(*,*)j,dyr_obs(j),dyr_sim(j)
                end do
                fobj(2) = fMARE(sGLB%nyear_sim, dyr_obs, dyr_sim,dFill)   !!rMARE: MARE of yearly data series
                functn = fWAVG(nobj,wobj,fobj,dFill)
!                write(*,*)functn,fobj
              if(sGLB%iSWAT.lt.0) then
                write(319,*)"Month | Robs_mm Rsim_mm | Rsim_SUB_mm | ",
     &                      "PCP_SUB_mm"
                do j = 1,nMon
                    write(319,*)j,'|',dmon_obs(j),dmon_sim(j),"|",
     &                sGLB%data_sim(1:sGLB%nsub_opt,j),"|",
     &                sGLB%data_sim(sGLB%nsub_opt+1:sGLB%nsub_opt*2,j)
                end do
              end if
            CASE (20,30)  !!monthly nutrient
                nobj = 5 !!!!(1)Flow_cms, (2)Sed_ton, (3)TP_ton, (4)TN_ton, (5)NO2+3_ton, (6)NO3_ton
                ALLOCATE(fobj(nobj))
                ALLOCATE(wobj(nobj))
                call WOBJ_INI(nobj,wobj,3,sGLB%wOBJ)  !!wobj will be normalized in FUNCTION fWAVG()
              if(sGLB%iSWAT.lt.0) then
                  if(sGLB%itype_opt.eq.20) then
                        write(319,*)"Month | Qobs_cms Qsim_cms | ", 
     &                      "SEDobs_ton SEDsim_ton | ",
     &                      "TPobs_ton TPsim_ton | ",
     &                      "TNobs_ton TNsim_ton | ",
     &                      "NO2+3obs_ton NO2+3sim_ton | ",
     &                      "NO3obs_ton NO3sim_ton"
                  else !!(=30)
                        write(319,*)"Month | Qobs_cms Qsim_cms | ", 
     &                      "SEDobs_mgL SEDsim_mgL | ",
     &                      "TPobs_mgL TPsim_mgL | ",
     &                      "TNobs_mgL TNsim_mgL | ",
     &                      "NO2+3obs_mgL NO2+3sim_mgL | ",
     &                      "NO3obs_mgL NO3sim_mgL"                      
                  end if
                do j = 1, nMon
                write(319,'(I5,6(A5,2F16.4))')j,
     &               ("|",sGLB%data_obs(k,j),sGLB%data_sim(k,j),k=1,6)
                end do
              end if
              
                do j = 1, nobj
                    dmon_obs(:) = sGLB%data_obs(j+1,:)  !!1st Col: flow_cms
                    dmon_sim(:) = sGLB%data_sim(j+1,:)
                    if(sGLB%iOBJ.eq.2) then
                        fobj(j) = fNRMSE(nMon,dmon_obs,
     &                      sGLB%data_obs(nobj+1+j,:),dmon_sim,dFill)
                    elseif(sGLB%iOBJ.eq.1) then
                        fobj(j) = fMARE(nMon, dmon_obs, dmon_sim,dFill)
                    else
                        fobj(j) = f1NSE(nMon, dmon_obs, dmon_sim,dFill)!!1-NSE 
                    end if
                end do  
                functn = fWAVG(nobj,wobj,fobj,dFill)
            CASE (21,31)  !!monthly sediment
                !!(1)Flow_cms, (2)Sed_ton, (3)TP_ton, (4)TN_ton, (5)NO2+3_ton, (6)NO3_ton
                nobj = 1 !!Sediment 
                k = 2  !!iCOL for Sediment
                ALLOCATE(fobj(nobj))
                ALLOCATE(wobj(nobj))
                call WOBJ_INI(nobj,wobj,3,sGLB%wOBJ)  !!wobj will be normalized in FUNCTION fWAVG()
                if(sGLB%iSWAT.lt.0) then  
                    if(sGLB%itype_opt.eq.21) then
                        write(319,*)"Month | SEDobs_ton SEDsim_ton"
                    else !! (=31)
                        write(319,*)"Month | SEDobs_mgL SEDsim_mgL"  
                    end if
                    do j = 1, nMon
                    write(319,*)j,
     &               "|",sGLB%data_obs(k,j),sGLB%data_sim(k,j)
                    end do
                end if  
!                do j = 1, nobj
                    dmon_obs(:) = sGLB%data_obs(k,:)  !!1st Col: flow_cms
                    dmon_sim(:) = sGLB%data_sim(k,:)
                    if(sGLB%iOBJ.eq.2) then
                        fobj(1) = fNRMSE(nMon,dmon_obs,
     &                      sGLB%data_obs(5+k,:),dmon_sim,dFill)
                    elseif(sGLB%iOBJ.eq.1) then
                        fobj(1) = fMARE(nMon, dmon_obs, dmon_sim,dFill)
                    else
                        fobj(1) = f1NSE(nMon, dmon_obs, dmon_sim,dFill)
                    end if
!                end do  
                functn = fobj(1) !!fWAVG(nobj,wobj,fobj,dFill)
            CASE (22,32)  !!monthly Nitrogen
                nobj = 3 !!!!(1)Flow_cms, (2)Sed_ton, (3)TP_ton, (4)TN_ton, (5)NO2+3_ton, (6)NO3_ton
                ALLOCATE(fobj(nobj))
                ALLOCATE(wobj(nobj))
                call WOBJ_INI(nobj,wobj,3,sGLB%wOBJ)  !!wobj will be normalized in FUNCTION fWAVG()
                if(sGLB%iSWAT.lt.0) then
                    if(sGLB%itype_opt.eq.22) then
                        write(319,*)"Month | ", 
     &                      "TNobs_ton TNsim_ton | ",
     &                      "NO2+3obs_ton NO2+3sim_ton | ",
     &                      "NO3obs_ton NO3sim_ton"
                    else !!(=32)
                        write(319,*)"Month | ", 
     &                      "TNobs_mgL TNsim_mgL | ",
     &                      "NO2+3obs_mgL NO2+3sim_mgL | ",
     &                      "NO3obs_mgL NO3sim_mgL"
                    end if
                    do j = 1, nMon
                        write(319,*)j,
     &               ("|",sGLB%data_obs(k,j),sGLB%data_sim(k,j),k=4,6)
                    end do
                end if  
                do j = 1, nobj
                    dmon_obs(:) = sGLB%data_obs(j+3,:)  !!1st Col: flow_cms
                    dmon_sim(:) = sGLB%data_sim(j+3,:)
                    if(sGLB%iOBJ.eq.2) then
                        fobj(j) = fNRMSE(nMon,dmon_obs,
     &                      sGLB%data_obs(5+nobj+j,:),dmon_sim,dFill)
                    elseif(sGLB%iOBJ.eq.1) then
                        fobj(j) = fMARE(nMon, dmon_obs, dmon_sim,dFill)
                    else
                        fobj(j) = f1NSE(nMon, dmon_obs, dmon_sim,dFill)!!1-NSE
                    end if
                end do  
                functn = fWAVG(nobj,wobj,fobj,dFill)

            CASE (23,33)  !!monthly TP
                !!(1)Flow_cms, (2)Sed_ton, (3)TP_ton, (4)TN_ton, (5)NO2+3_ton, (6)NO3_ton
                nobj = 1 !!TP
                k = 3  !!iCOL for Sediment
                ALLOCATE(fobj(nobj))
                ALLOCATE(wobj(nobj))
                call WOBJ_INI(nobj,wobj,3,sGLB%wOBJ)  !!wobj will be normalized in FUNCTION fWAVG()
                if(sGLB%iSWAT.lt.0) then
                    if(sGLB%itype_opt.eq.23) then
                        write(319,*)"Month | TPobs_ton TPsim_ton"
                    else !!(=33)
                        write(319,*)"Month | TPobs_mgL TPsim_mgL"
                    end if
                    do j = 1, nMon
                        write(319,*)j,
     &               "|",sGLB%data_obs(k,j),sGLB%data_sim(k,j)
                    end do
                end if  
!                do j = 1, nobj
                    dmon_obs(:) = sGLB%data_obs(k,:)  !!1st Col: flow_cms
                    dmon_sim(:) = sGLB%data_sim(k,:)
                    if(sGLB%iOBJ.eq.2) then
                        fobj(1) = fNRMSE(nMon,dmon_obs,
     &                      sGLB%data_obs(5+k,:),dmon_sim,dFill)
                    elseif(sGLB%iOBJ.eq.1) then
                        fobj(1) = fMARE(nMon, dmon_obs, dmon_sim,dFill)
                    else
                        fobj(1) = f1NSE(nMon, dmon_obs, dmon_sim,dFill)
                    end if
!                    fobj(j) = fNSE(nMon, dmon_obs, dmon_sim,dFill)!!NSE of monthly data series
                functn = fobj(1) !!fWAVG(nobj,wobj,fobj,dFill)
            CASE (24,34)  !!monthly nutrient: Sed+TP
                nobj = 2 !!!!(1)Flow_cms, (2)Sed_ton, (3)TP_ton, (4)TN_ton, (5)NO2+3_ton, (6)NO3_ton
                ALLOCATE(fobj(nobj))
                ALLOCATE(wobj(nobj))
                call WOBJ_INI(nobj,wobj,3,sGLB%wOBJ)  !!wobj will be normalized in FUNCTION fWAVG()
                if(sGLB%iSWAT.lt.0) then
                    if(sGLB%itype_opt.eq.24) then
                        write(319,*)"Month | ", 
     &                  "SEDobs_ton SEDsim_ton | TPobs_ton TPsim_ton"
                    else !!(=34)
                        write(319,*)"Month | ", 
     &                  "SEDobs_mgL SEDsim_mgL | TPobs_mgL TPsim_mgL"
                    end if
                    do j = 1, nMon
                        write(319,'(I5,2(A5,2F16.4))')j,
     &               ("|",sGLB%data_obs(k,j),sGLB%data_sim(k,j),k=2,3)
                    end do
                end if 
                do j = 1, nobj
                    dmon_obs(:) = sGLB%data_obs(j+1,:)  !!1st Col: flow_cms
                    dmon_sim(:) = sGLB%data_sim(j+1,:)
                    if(sGLB%iOBJ.eq.2) then
                        fobj(j) = fNRMSE(nMon,dmon_obs,
     &                      sGLB%data_obs(nobj+1+j,:),dmon_sim,dFill)
                    elseif(sGLB%iOBJ.eq.1) then
                        fobj(j) = fMARE(nMon, dmon_obs, dmon_sim,dFill)
                    else
                        fobj(j) = f1NSE(nMon, dmon_obs, dmon_sim,dFill)!!1-NSE 
                    end if
                end do  
                functn = fWAVG(nobj,wobj,fobj,dFill)
            CASE DEFAULT		 !!E.G, subbasin with STREAMFLOW, SWC
                write(*,*)">>>NO SPECIFIC CASE WAS SELECTED!!!"
                dobs = sGLB%data_obs(1,:)
                dsim = sGLB%data_sim(1,:)
                nArray = size(dobs)
                functn = f1NSE(nArray,dobs,dsim,dFill)
              if(sGLB%iSWAT.lt.0) then
                write(319,*)"Day | dobs dsim"
                do j = 1, nMon
                write(319,*)j,
     &               "|",sGLB%data_obs(k,j),sGLB%data_sim(k,j)
                end do
              end if
        END SELECT
!!------------------------------------------------------------------

        sGLB%iSWAT = sGLB%iSWAT + 1
        if(sGLB%pid.eq.0) then
            write(format2107, *) "(a1,i5,a25,", sGLB%nsub_opt,"i4)"
            write(*,format2107)"#",sGLB%iSWAT,
     &      " SWAT-RUN for SUBBASINs = ",sGLB%isub_opt
            write(format2107, *) "(A7,", npar, "f10.4)"
            write(*,format2107)">>>PAR=",sGLB%xua
            write(format2107, *) "(A7,f10.3,A5,",nobj,"f10.3)"
            write(*,format2107)">>>OBJ=",functn," || ", fobj(1:nobj)
            write(*,*)
        end if !!if(sGLB%pid.eq.0) then
 
        !!sSCE%iFO3 = 20067
!!        write(format2106, *) "(", npar, "f10.4,A10,",nobj+1,"f10.3)"
!!	write(20067,format2106)sGLB%xua,"<>",functn,fobj(1:nobj)  
!!        close(20067)
!!        open(unit=20067,file=trim(sGLB%dir_sceout)//'SCEOUT_ALL.dat',
!!     &   status="unknown",position = "append")

        do j = 1, nrgage  !openwth.f
                close(100+j)  !SWAT2000: close(9+j)
        end do
        do j = 1, ntgage   !openwth.f
                close (118+j)  !SWAT2000: close (27+j)
        end do
        do j = 1, 4        !openwth.f
                close (136+j)  !SWAT2000: close (45+j)
        end do
        do j = 1, nres  !!see readres.f
              close (350+j)
        end do
!!SCE_BEGIN
        close(7) !output.rch
        close(31) !output.sub
        close(28) !output.hru
        close(8)  !output.rsv
        close(84) !output.sed
        close(26) !output.std
        close(24) !input.std
        close(11123) !hyd.out
        close(50+inum1) !watout.dat, in "saveconc.f"
!!SCE_END
	
        CALL zero0
        CALL zero1
        CALL zero2
        CALL zeroini
        CALL zero_urbn

!!	CALL openwth
!
!	!!    open daily reservoir outflow file 
!!	do i = 1, nres
!!          if (iresco(i) == 3) then
!		
!!			open (350+i,file=resdayo)
!!			read (350+i,*) titldum
!!		end if
!!      end do

        RETURN 
      END



	
