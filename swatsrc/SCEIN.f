      subroutine scein(sSCE)
        USE parm
        USE scemod
!$debug
!
!   THIS SUBROUTINE READS AND PRINTS THE INPUT VARIABLES FOR
!   SHUFFLED COMPLEX EVOLUTION METHOD FOR GLOBAL OPTIMIZATION
!     -- Version 2.1
!
!   WRITTEN BY QINGYUN DUAN - UNIVERSITY OF ARIZONA, APRIL 1992
!   MODIFIED BY GANGSHENG WANG - ORNL, MARCH 2015
!
        IMPLICIT NONE
        REAL,PARAMETER::dFill = -9999
        TYPE(sSCE_PAR), intent(inout):: sSCE
        
        INTEGER msub1,nbyr1,iyear1,iprint1,iModel1
        INTEGER nStation !!iStation !# of stations (streamflow, reservoir, SWC, etc
        INTEGER iFin0,iFin !input file unit
        INTEGER iFobs,iFsurlag !file for observed reservoir volume; file for surlag of each subbasin
        INTEGER ierror,iwarn, jyear,iCASE,eof

        character*10 pcntrl,deflt,usrsp
        character*5 reduc,initl,ysflg,noflg !,xname(npar)
!        integer iVar(16)   !now global variables, variation method (1-absolute value, 2-relative value: multiplied by a factor)
        integer ivoid,ivoid1,j,k,nSub         !whether the parameter will be optimized (1-yes, 0-no)
        real rvoid,rvoid1,pcenta
        character*20 sGage
        character*200 svoid,svoid1,sRead
        character*200 sfSCEIN,sfSCE_SUM,sfSCE_OPT,sfSCE_ALL,sfSCEini !!data file
        character*100 sStation !station name
        character*200 sDIR_SCEIN,sDIR_SCEOUT,sDIR_OBS 
        character*200 file_obs!!file_resv, file_mon_runoff,
        
        character*200  file_sub_area
        integer iYearMon_begin, iYearMon_end
        REAL, DIMENSION(:),ALLOCATABLE :: data_obs_read
        INTEGER mNPS,mCOL !!# of NPS (Nitrogen, Phosphorus, Sediment),mCOL = mNPS*2: Mean+SD
        
        INTEGER npar,nyear_warmup,itype_opt,icode_sub,icode_res
        INTEGER nsub_opt, iOBJ
        REAL wOBJ(3)!!rNSE0,wNSEm,wNSE
        INTEGER, DIMENSION(:),ALLOCATABLE :: isub_opt

        data deflt/' DEFAULT  '/
        data usrsp/'USER SPEC.'/
        data ysflg/'YES '/
        data noflg/'NO  '/

        character*50 format2013
        INTEGER nDaysofYear !!FUNCTION
	
        write (*,*) '>>>ENTER SCEIN SUBROUTINE...'

!  INITIALIZE I/O VARIABLES
        call sGLB_INI_VAR()  !!initialize variables (excluding Arrays) in sGLB
        write (*,*) 
     &  ">>>READ SWAT DIR INFO in File 'SWAT.dir' in Current Model DIR"
        open(1,file='SWAT.dir', status='old')
        read(1,*)svoid
        read(1,*)svoid
        read(1,*)svoid
        read(1,'(a)')sRead  !!Line-4
        call DIR_ADD_SLASH(sRead,svoid) !!dir for swat in & out files
        sGLB%dir_swatio = svoid
!        print*,svoid
!        print*,sGLB%dir_swatio
        read(1,*)svoid
        read(1,*)svoid
        read(1,'(a)')sRead  !!Line-7
        call DIR_ADD_SLASH(sRead,svoid) 
        sGLB%dir_userio = svoid  !!used in readres.f to read observed outflow file
        
        sDIR_OBS        = trim(sGLB%dir_userio)//"aOBS/"
        sDIR_SCEIN      = trim(sGLB%dir_userio)//"aSCEIN/"
        sDIR_SCEOUT     = trim(sGLB%dir_userio)//"aSCEOUT/"
        
        sGLB%dir_obs    = sDIR_OBS
        sGLB%dir_scein  = sDIR_SCEIN
        sGLB%dir_sceout = sDIR_SCEOUT

        close(1)    
        
        iFin0 = 20060
        iFin = 20061

        sSCE%iFO1 = 20065  !ipr
        sSCE%iFO2 = 20066
        sSCE%iFO3 = 20067  !iprwgs
        iFobs = 20068
	iFsurlag = 20069

        sfSCEini = trim(sDIR_SCEIN)//'SCE.ini'
        print*, ">>>------------------------------------------"
        print*, "dir_SWATio = ", trim(sGLB%dir_swatio)
        print*, "dir_USERio = ", trim(sGLB%dir_userio)
        print*, "dir_obs    = ", trim(sGLB%dir_obs)
        print*, "dir_SCEin  = ", trim(sGLB%dir_scein)
        print*, "dir_SCEout = ", trim(sGLB%dir_sceout)
        print*, ">>>------------------------------------------"
        open(unit=iFin0,file = sfSCEini,status='old')
        read(iFin0,*)
        read(iFin0,*)sGage !!iStation
        read(iFin0,*)
        read(iFin0,*)nStation
        read(iFin0,*)
        sGage = trim(sGage)
        sStation = '' !default file name
        do j = 1, nStation
            read(iFin0,*,iostat=eof)svoid1, svoid, ivoid1  !!sGage, GAGE/RES NAME, ID
            if(eof < 0) then
                exit
            end if
            svoid1 = trim(svoid1)
            if(svoid1.eq.sGage) then
                sStation = svoid
                exit
            end if
        end do
        close(iFin0)
        if(sStation.eq.'') then
            write(*,*)'!!!DO NOT FIND THE DESIGNATED GAGE: ',sGage
            write(*,*)'!!!PLEASE CHECK THE FILE: ',sfSCEini
            stop
        end if
        
        sfSCEIN = trim(sDIR_SCEIN)//'SCEIN_'//trim(sStation)//'.dat'
        sfSCE_SUM = trim(sDIR_SCEOUT)//'SCEOUT_SUM.dat'
        sfSCE_OPT = trim(sDIR_SCEOUT)//'SCEOUT_OPT.dat'
        sfSCE_ALL = trim(sDIR_SCEOUT)//'SCEOUT_ALL.dat'
        print*, ">>>File Names:"
        print*, ">>>------------------------------------------"
        print*, "File_SCE_initial = ", trim(sfSCEini)
        print*, "File_SCE_input   = ", trim(sfSCEIN)
        print*, "File_SCE_summary = ", trim(sfSCE_SUM)
        print*, "File_SCE_par_opt = ", trim(sfSCE_OPT)
        print*, "File_SCE_par_all = ", trim(sfSCE_ALL)
        
        sRead = trim(sDIR_SCEOUT)//'SCE-SWAT.chk'
        print*, "File_SIM_vs_OBS  = ", trim(sRead)
        open(319, file=sRead, status='unknown')
        print*, ">>>------------------------------------------"
!!wgs--------------------------------------------------------------
        open(unit=iFin,file=sfSCEIN,status='old')
        open(unit=sSCE%iFO1,file=sfSCE_SUM)
        open(unit=sSCE%iFO2,file=sfSCE_OPT)
        open(unit=sSCE%iFO3,file=sfSCE_ALL)
!!wgs===============================================================  
!!wgs: write to SCEOUT_ALL.dat
        write(sSCE%iFO3,*)"SWAT <VER 2012/Rev 627> MODEL RUN & OPT"
        write(sSCE%iFO3,*)
     &   ">>>Modified by GANGSHENG WANG @ ORNL:JUNE 3, 2015"
        write (sSCE%iFO3,*) '>>>ENTER SCEIN SUBROUTINE...'
        write (sSCE%iFO3,*) 
     &  '>>>Initialize variables (excluding Arrays) in sGLB'
        write (sSCE%iFO3,*) 
     &  ">>>READ SWAT DIR INFO in File 'SWAT.dir' in Current Model DIR"
        write(sSCE%iFO3,*)">>>----------------------------------------"
        write(sSCE%iFO3,*)"dir_SWATio = ", trim(sGLB%dir_swatio)
        write(sSCE%iFO3,*)"dir_USERio = ", trim(sGLB%dir_userio)
        write(sSCE%iFO3,*)"dir_obs    = ", trim(sGLB%dir_obs)
        write(sSCE%iFO3,*)"dir_SCEin  = ", trim(sGLB%dir_scein)
        write(sSCE%iFO3,*)"dir_SCEout = ", trim(sGLB%dir_sceout)
        write(sSCE%iFO3,*)">>>----------------------------------------"
        write(sSCE%iFO3,*)">>>File Names:"
        write(sSCE%iFO3,*)">>>----------------------------------------"
        write(sSCE%iFO3,*)"File_SCE_initial = ", trim(sfSCEini)
        write(sSCE%iFO3,*)"File_SCE_input   = ", trim(sfSCEIN)
        write(sSCE%iFO3,*)"File_SCE_summary = ", trim(sfSCE_SUM)
        write(sSCE%iFO3,*)"File_SCE_par_opt = ", trim(sfSCE_OPT)
        write(sSCE%iFO3,*)"File_SCE_par_all = ", trim(sfSCE_ALL)
        write(sSCE%iFO3,*)"File_SIM_vs_OBS  = ", trim(sRead)
        write(sSCE%iFO3,*)">>>----------------------------------------"
!!wgs===============================================================
        ierror = 0
        iwarn = 0
        write(sSCE%iFO1,700)
  700   format(10x,'SHUFFLED COMPLEX EVOLUTION GLOBAL OPTIMIZATION',
     &       /,10x,46(1h=))


!  READ THE SCE CONTROL PARAMETERS
!!2006-08-04------------------------------------------------
        read(iFin, *)
        read(iFin,*)msub1,nbyr1,iyear1,iprint1,iModel1
        sRead = sGLB%dir_swatio
        CALL Write_filecio(sRead,nbyr1,iyear1,iprint1)
        read(iFin, *)
        read(iFin,*)npar,nyear_warmup,
     &   itype_opt,icode_sub,icode_res,nsub_opt,
     &   iOBJ,wOBJ(1:3)!!rNSE0,wNSEm,wNSE !!'(2I5,3f10.2)'
     
        
        sSCE%npar = npar
        sGLB%iModel = iModel1
        sGLB%iyear0 = iyear1
        sGLB%nyear_warmup = nyear_warmup
        sGLB%itype_opt = itype_opt
        sGLB%icode_sub = icode_sub
        sGLB%icode_res = icode_res
        sGLB%nsub_opt = nsub_opt
        sGLB%iOBJ = iOBJ
        sGLB%wOBJ = wOBJ
!        write(*,*)sSCE%npar,sGLB%nyear_warmup,
!     &   sGLB%itype_opt,sGLB%icode_sub,sGLB%icode_res,sGLB%nsub_opt,
!     &   sGLB%rOBJ0,sGLB%rNSE0,sGLB%wNSEm,sGLB%wNSE !!'(2I5,3f10.2)'
!!wgs-------------------------------------------
        write(*,*)">>>ALLOCATE ARRAYS in STRUCTURE sSCE:"
        write (sSCE%iFO3,*)">>>ALLOCATE ARRAYS in STRUCTURE sSCE:"
!          ALLOCATE(iPar_sel(sSCE%npar))
          ALLOCATE(sSCE%parName(sSCE%npar))
          ALLOCATE(sSCE%parINI(sSCE%npar))
          ALLOCATE(sSCE%parMIN(sSCE%npar))
          ALLOCATE(sSCE%parMAX(sSCE%npar))
          ALLOCATE(sSCE%parOPT(sSCE%npar))

!!wgs-------------------------------------------      
        sGLB%nyear_sim = nbyr1 - sGLB%nyear_warmup  !!'sGLB%nyear_sim' in the rhs is the #(NBYR) in 'file.cio', see Line 139 in 'getallo.f'
!        sGLB%nyear_sim = ivoid                      !!excluding warm-up period
        sGLB%nmon_sim = sGLB%nyear_sim * 12   !!# of simulation months for calibration
!        write(*,*)"# of months = ",sGLB%nmon_sim
        sGLB%nday_sim0 = 0
        do j = 1,sGLB%nyear_sim
            jyear = sGLB%iyear0 + sGLB%nyear_warmup + j - 1
            sGLB%nday_sim0 = sGLB%nday_sim0 + nDaysofYear(jyear)
        end do
       
        write(*,*)">>>ALLOCATE ARRAYS in STRUCTURE sGLB:"
        write (sSCE%iFO3,*)">>>ALLOCATE ARRAYS in STRUCTURE sGLB:"
        allocate (isub_opt(sGLB%nsub_opt)) 
        allocate (sGLB%isub_opt(sGLB%nsub_opt))      !!subbasin ID to be optimized
        allocate (sGLB%ivar_par(sSCE%npar))
        ALLOCATE (sGLB%iopt_par(sSCE%npar))          
        allocate (sGLB%xua(sSCE%npar))          !!parameter values
        allocate (sGLB%subsurlag(msub1))   !!subbasin surface runoff lag (day)
        allocate (sGLB%par_min(sSCE%npar))
        allocate (sGLB%par_max(sSCE%npar))
        ALLOCATE (sGLB%sub_area(msub1)) !nSub
        allocate (sGLB%iGateDam(msub1))     !!0-subbasin,1-floodgate,2-reservoir
        allocate (sGLB%cwus(msub1))        !!wgs: different wateruse coefficient for each subbasin
	allocate (sGLB%crwus(msub1))        !!wgs:
        
        write (sSCE%iFO3,*)">>>INITIALIZE ARRAYS in STRUCTURE sGLB:"
        call sGLB_INI_ARRAY()  !!initialize Arrays in sGLB      
!!wgs------------------------------------------- 
        
        read(iFin, *) !!isub_opt
        read(iFin,*)isub_opt(1:sGLB%nsub_opt) 
        sGLB%isub_opt(1:sGLB%nsub_opt)  = isub_opt(1:sGLB%nsub_opt)  
!        write(*,*)sGLB%isub_opt
        read(iFin, *) !!FILE WITH OBSERVATION
	    read(iFin,'(a20)')file_obs
        write(*,*)'File_OBS: ', file_obs
        file_obs = trim(sDIR_OBS)//trim(file_obs)
!!2006-08-04------------------------------------------------
        sSCE%ideflt = 0
        read(iFin, *)svoid  !"SCE PARAMETERS"
        read(iFin, *)svoid  !"maxn kstop pcento  ngs iseed ideflt"
        read(iFin,*)sSCE%maxn,sSCE%kstop,
     &   sSCE%pcento,sSCE%ngs,sSCE%nSCE,sSCE%ideflt

!  IF ideflt IS EQUAL TO 1, READ THE SCE CONTROL PARAMETERS
        read(iFin, *) !"npg		nps  nspl 	mings 	iniflg ifprint"
        if (sSCE%ideflt .eq. 1) Then
            read(iFin,*) sSCE%npg,sSCE%nps,sSCE%nspl,
     &       sSCE%mings,sSCE%iniflg,sSCE%ifprint
            pcntrl = usrsp
        else
            read(iFin,*)  !!read data but do NOT use them
            pcntrl = deflt
        end if
!  810   format(6i5)

!  READ THE INITIAL PARAMETER VALUES AND THE PARAMETER BOUNDS
!!wgs---------------------------------------------------------
        read(iFin, *)  !SWAT PARAMETERS
        read(iFin, *)  svoid!column names
!        write(*,*) svoid

        sSCE%nopt = 0  !# of parameters selected for optimization
        do j = 1,sSCE%npar
            read(iFin,*)ivoid,sSCE%parName(j),
     &      ivoid,ivoid1,rvoid, rvoid1,
     &      sSCE%parINI(j),sSCE%parMIN(j),sSCE%parMAX(j) 
     
        sGLB%ivar_par(j) = ivoid
        sGLB%iopt_par(j) = ivoid1
        sGLB%par_min(j) = rvoid
        sGLB%par_max(j)= rvoid1
!            write(*,'(I5,A10,2I5,5f10.4)')
!     &       ivoid,sSCE%parName(j),sGLB%ivar_par(j),
!     &      sGLB%iopt_par(j),sGLB%par_min(j), sGLB%par_max(j),
!     &      sSCE%parINI(j),sSCE%parMIN(j),sSCE%parMAX(j) 
         end do
!!---------------------------------------------------------------------         
        if(sGLB%itype_opt.lt.20) then!!water yield, DO NOT calibrate water quality parameters 
            sGLB%iopt_par(15:sSCE%npar) = 0  
        elseif(sGLB%itype_opt.ge.20.and.sGLB%itype_opt.le.39) then !!water quality, DO NOT calibrate water quantity parameters
            sGLB%iopt_par(1:14) = 0   
            SELECT CASE(sGLB%itype_opt)
                CASE(21,31) !!Sediment, PAR 15-21
!            if(sGLB%itype_opt.eq.21) then !!Sediment, PAR 15-21
                    sGLB%iopt_par(22:39) = 0
!            elseif(sGLB%itype_opt.eq.22) then !!Nitrogen, PAR 22-30
                CASE(22,32) !!Nitrogen, PAR 22-30
                    sGLB%iopt_par(15:21) = 0
                    sGLB%iopt_par(31:39) = 0
!            elseif(sGLB%itype_opt.eq.23) then !!Phosphorus, PAR 31-39
                CASE (23,33) !!Phosphorus, PAR 31-39
                    sGLB%iopt_par(15:30) = 0
!            elseif(sGLB%itype_opt.eq.24) then !!Sediment+Phosphorus, PAR 15-21 & 31-39
                CASE (24,34) !!Sediment+Phosphorus, PAR 15-21 & 31-39
                    sGLB%iopt_par(22:30) = 0
                CASE DEFAULT
                    sGLB%iopt_par(1:14) = 0
!            end if
            END SELECT !!(sGLB%itype_opt)
        end if
        
!!---------------------------------------------------------------------            
        do j = 1,sSCE%npar
            if(sGLB%iopt_par(j).gt.0) then
                sSCE%nopt = sSCE%nopt + 1
            end if
        end do 
!  830   format(i10,a10,2i10,5f10.3)
        read(iFin, *)   !parameter names
        read(iFin,*)sSCE%parINI     !re-read the initial par values, this is convenient for pasting par values into this line 
	close(iFin)
        
        sSCE%parName = ADJUSTR(sSCE%parName)
        ALLOCATE(sSCE%iOpt(sSCE%nopt))
        k = 0
        do j = 1, sSCE%npar
            if(sGLB%iopt_par(j).gt.0) then
                k = k + 1
                sSCE%iOpt(k) = j
            end if
        end do

!c  IF ideflt IS EQUAL TO 0, SET THE SCE CONTROL PARAMETERS TO
!c  THE DEFAULT VALUES
      if (sSCE%ideflt .eq. 0) then
        sSCE%npg = 2*sSCE%nopt + 1
        sSCE%nps = sSCE%nopt + 1
        sSCE%nspl = sSCE%npg
        sSCE%mings = sSCE%ngs
        sSCE%iniflg = 0
        sSCE%ifprint = 0
      end if

      !!---------------------------------------------------------------------
!!SCE_BEGIN   
        file_sub_area = trim(sDIR_OBS)//'sub_area.txt'
        write(*,*)">>>Read SWAT Subbasin Area Values in '",
     &             trim(file_sub_area),"'"
        write(sSCE%iFO3,*)">>>Read SWAT Subbasin Area Values in '",
     &             trim(file_sub_area),"'"
        open(unit=101,file = file_sub_area,status='old')
        read(101,*)  !!"No. of Subbasins"
        read(101,*)nSub
        if(nSub.ne.msub1) then
            write(*,*)"No. of subbasins in ", file_sub_area, 
     &       " = ",nSub," <> in SWAT = ",msub1
            write(sSCE%iFO3,*)"No. of subbasins in ", file_sub_area, 
     &       " = ",nSub," <> in SWAT = ",msub1
        end if
!        ALLOCATE(sGLB%sub_area(nSub))
        read(101,*)  !!head
        do i = 1, nSub
            read(101,*)ivoid, ivoid1, rvoid, rvoid1, rvoid1  
                  !!Subbasin, #HRUs,  Area(km2),Latitude,Elev(m)
            sGLB%sub_area(i) = rvoid
!            write(*,*)ivoid, sGLB%sub_area(i)
        end do
        close(101)
        
        sRead = trim(sGLB%dir_swatio)//"sub.lag"
        open(unit = iFsurlag,file=sRead,status = 'old')
        write(*,*)">>>Read SUB SURLAG Values in '",trim(sRead),"'" 
        write(sSCE%iFO3,*)">>>Read SUB SURLAG Values in '",
     &                     trim(sRead),"'" 
	read(iFsurlag,*)
        do j = 1,msub1
                read(iFsurlag,*)ivoid,sGLB%subsurlag(j),
     &           sGLB%cwus(j),sGLB%crwus(j),sGLB%iGateDam(j) 
        end do
        close(iFsurlag)
!!SCE_END     

!!wgs---------------------------------------------------------
        write(*,'(a11,i5,a12,a15,a7,i5)')
     &   'itype_opt =',sGLB%itype_opt,
     &    '; Station = ',sGage,'; RES = ',sGLB%icode_res
        
        write(format2013,*)"(A10,",sGLB%nsub_opt,"(I4))"
        write(*,'(A10,I4)')'nsub_opt = ',sGLB%nsub_opt
        write(*,format2013)'isub_opt = ',sGLB%isub_opt
        write(*,'(a24,i3)')'No. of PAR    : npar = ',sSCE%npar
	write(*,'(a24,i3)')'No. of PAR-OPT: nopt = ',sSCE%nopt
        write(*,'(A10,30I5)')"iOpt[] = ",sSCE%iOpt
	write(*,*) 
!!wgs---------------------------------------------------------
        write(sSCE%iFO3,*)">>>--------------------------------------"
        write(sSCE%iFO3,*)'OBF (0-NSE,1-MARE,2-NRMSE) = ',sGLB%iOBJ
	write(sSCE%iFO3,'(a11,i5,a12,a15,a7,i5)')
     &    'itype_opt =',sGLB%itype_opt,  
     &    '; Station = ',sGage,'; RES = ',sGLB%icode_res
        write(sSCE%iFO3,*)">>>--------------------------------------"
        write(sSCE%iFO3,'(A10,I4)')'nsub_opt = ',sGLB%nsub_opt
        write(sSCE%iFO3,format2013)'isub_opt = ',sGLB%isub_opt
        write(sSCE%iFO3,*)">>>--------------------------------------"
        write(sSCE%iFO3,'(a24,i3)')'No. of PAR    : npar = ',sSCE%npar
	write(sSCE%iFO3,'(a24,i3)')'No. of PAR-OPT: nopt = ',sSCE%nopt
        write(sSCE%iFO3,'(A10,30I5)')"iOpt[] = ",sSCE%iOpt
        write(sSCE%iFO3,*)">>>--------------------------------------"
        write(sSCE%iFO3,*)"nYEARS_SPINUP      =",sGLB%nyear_warmup,
     &        "; FROM",sGLB%iyear0,
     &        " TO",sGLB%iyear0+sGLB%nyear_warmup-1   
        write(sSCE%iFO3,*)"nYEARS_CALIBRATION =",sGLB%nyear_sim,
     &        "; FROM",sGLB%iyear0+sGLB%nyear_warmup,
     &        " TO",sGLB%iyear0+sGLB%nyear_warmup+sGLB%nyear_sim-1
!!wgs---------------------------------------------------------   
	write(sSCE%iFO2,*)"BEST PARAMETER SETS:"
        write(sSCE%iFO3,*)">>>--------------------------------------"
        write(sSCE%iFO3,*)"ALL PARAMETER SETS:"
        write(sSCE%iFO3,*)"Calibrate or NOT (SEE BELOW): 1-YES,0-NO"
        write(format2013,*)"(",sSCE%npar,"(I10))"
        write(sSCE%iFO3,format2013)sGLB%iopt_par
        write(format2013,*)"(",sSCE%npar+2,"(a10))"
	write(sSCE%iFO3,format2013)sSCE%parName,"<>","OBF"
!!wgs---------------------------------------------------------
!  CHECK IF THE SCE CONTROL PARAMETERS ARE VALID
        if (sSCE%ngs .lt. 1 .or. sSCE%ngs .ge. 1320) then
            write(sSCE%iFO1,900) sSCE%ngs
  900       format(//,1x,'**ERROR** NUMBER OF COMPLEXES IN INITIAL ',
     &         ' POPULATION ',i5,' IS NOT A VALID CHOICE')
            ierror = ierror + 1
        end if
c
        if (sSCE%kstop .lt. 0 .or. sSCE%kstop .ge. 20) then
            write(sSCE%iFO1,901) sSCE%kstop
  901       format(//,1x,'**WARNING** THE NUMBER OF SHUFFLING LOOPS IN',
     &      ' WHICH THE CRITERION VALUE MUST CHANGE ',/,13x,'SHOULD BE',
     &      ' GREATER THAN 0 AND LESS THAN 10.  ','kstop = ',i2,
     &      ' WAS SPECIFIED.'/,13x,
     &      'BUT kstop = 5 WILL BE USED INSTEAD.')
            iwarn = iwarn + 1
            sSCE%kstop=5
        end if

        if (sSCE%mings .lt. 1 .or. sSCE%mings .gt. sSCE%ngs) then
            write(sSCE%iFO1,902) sSCE%mings
  902       format(//,1x,'**WARNING** THE MINIMUM NUMBER OF COMPLEXES ',
     &         i2,' IS NOT A VALID CHOICE. SET IT TO DEFAULT')
            iwarn = iwarn + 1
            sSCE%mings = sSCE%ngs
        end if

      if (sSCE%npg .lt. 2 .or. sSCE%npg .gt. 1320/max(sSCE%ngs,1)) then
        write(sSCE%iFO1,903) sSCE%npg
  903   format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A COMPLEX ',
     &         I4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        sSCE%npg = 2*sSCE%nopt+1
      end if

      if (sSCE%nps.lt.2 .or. sSCE%nps.gt.sSCE%npg .or. sSCE%nps.gt.50) 
     &    then
        write(sSCE%iFO1,904) sSCE%nps
  904   format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A SUB-',
     &  'COMPLEX ',i4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        sSCE%nps = sSCE%nopt + 1
      end if

      if (sSCE%nspl .lt. 1) then
        write(sSCE%iFO1,905) sSCE%nspl
  905   format(//,1x,'**WARNING** THE NUMBER OF EVOLUTION STEPS ',
     &         'TAKEN IN EACH COMPLEX BEFORE SHUFFLING ',I4,/,13x,
     &         'IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        sSCE%nspl = sSCE%npg
      end if

!  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPULATION
      sSCE%npt = sSCE%ngs * sSCE%npg

      if (sSCE%npt .gt. 1320) then
        write(sSCE%iFO1,906) sSCE%npt
  906   format(//,1x,'**WARNING** THE NUMBER OF POINTS IN INITIAL ',
     &         'POPULATION ',i5,' EXCEED THE POPULATION LIMIT,',/,13x,
     &         'SET NGS TO 2, AND NPG, NPS AND NSPL TO DEFAULTS')
        iwarn = iwarn + 1
        sSCE%ngs = 2
        sSCE%npg = 2*sSCE%nopt + 1
        sSCE%nps = sSCE%nopt + 1
        sSCE%nspl = sSCE%npg
      end if

!  PRINT OUT THE TOTAL NUMBER OF ERROR AND WARNING MESSAGES
      if (ierror .ge. 1) write(sSCE%iFO1,907) ierror
  907 format(//,1x,'*** TOTAL NUMBER OF ERROR MESSAGES IS ',i2)

      if (iwarn .ge. 1) write(sSCE%iFO1,908) iwarn
  908 format(//,1x,'*** TOTAL NUMBER OF WARNING MESSAGES IS ',i2)

      if (sSCE%mings .lt. sSCE%ngs) then
        reduc = ysflg
      else
        reduc = noflg
      end if

      if (sSCE%iniflg .ne. 0) then
        initl = ysflg
      else
        initl = noflg
      end if


!c  PRINT SHUFFLED COMPLEX EVOLUTION OPTIMIZATION OPTIONS
  104 write(sSCE%iFO1,910)
  910 format(//,2x,'SCE CONTROL',5x,'MAX TRIALS',5x,
     &'REQUIRED IMPROVEMENT',5x,'RANDOM',/,3x,'PARAMETER',8x,
     &'ALLOWED',6x,'PERCENT',4x,'NO. LOOPS',6x,'nSCE',/,
     &2x,11(1h-),5x,10(1H-),5x,7(1h-),4x,9(1h-),5x,6(1h-))

      pcenta=sSCE%pcento*100.
      write(sSCE%iFO1,912) pcntrl,sSCE%maxn,pcenta,sSCE%kstop,sSCE%nSCE
  912 format(3x,a10,7x,i5,10x,f3.1,9x,i2,9x,i5)
      write(sSCE%iFO1,914) sSCE%ngs,sSCE%npg,sSCE%npt,sSCE%nps,sSCE%nspl
  914 format(//,18x,'SCE ALGORITHM CONTROL PARAMETERS',/,18x,32(1H=),
     &//,2x,'NUMBER OF',5x,'POINTS PER',5x,'POINTS IN',6x,'POINTS PER',
     &4x,'EVOL. STEPS',/,2x,'COMPLEXES',6X,'COMPLEX',6x,'INI. POPUL.',
     &5x,'SUB-COMPLX',4x,'PER COMPLEX',/,2x,9(1h-),5x,10(1h-),4x,
     &11(1h-),5x,10(1h-),4x,11(1h-),5x,/,2x,5(i5,10x))
      write(sSCE%iFO1,915) reduc,sSCE%mings,initl
  915 format(//,15x,'COMPLX NO.',5x,'MIN COMPLEX',5x,'INI. POINT',/,
     &15x,'REDUCTION',6x,'NO. ALLOWED',6x,'INCLUDED',/,
     &15x,10(1h-),5x,11(1h-),5x,10(1h-),/,18x,a4,6x,i8,13x,a4)
      write(sSCE%iFO1,916)
  916 format(//,8x,'INITIAL PARAMETER VALUES AND PARAMETER BOUNDS',/,
     &       8x,45(1h=),//,2x,'PARAMETER',5x,'INITIAL VALUE',5x,
     &       'LOWER BOUND',5x,'UPPER BOUND',5x,'OPT_Y/N',/,
     &       2x,9(1h-),5x,13(1h-),5x,11(1h-),
     &       5x,11(1h-),5x,11(1h-))
      do 920 j = 1, sSCE%npar
        write(sSCE%iFO1,918) sSCE%parName(j),
     &   sSCE%parINI(j),sSCE%parMIN(j),sSCE%parMAX(j),sGLB%iopt_par(j)
  918   format(a12,3(6x,f10.4),5x,I5)
!  918   format(4x,a4,4x,3(6x,f10.3))
  920 continue
      if (ierror .ge. 1) then
      write(sSCE%iFO1,922)
  922 format(//,'*** THE OPTIMIZATION SEARCH IS NOT CONDUCTED BECAUSE',
     &       ' OF INPUT DATA ERROR ***')
      stop
      end if

!!wgs------------------------------------------------------------
      
!!        CALL readfile  !!open output files
         
!!---------------------------------------------------------------------
        iCASE = sGLB%itype_opt
!        write(*,*)"CASE = ",iCASE
        SELECT CASE(iCASE)
            CASE (0)  !!daily streamflow (m3/s)
!!FILE_OBS:
!!19850101
!!   37.97 
                mCOL = 3
                ALLOCATE (sGLB%data_sim(mCOL,sGLB%nday_sim0)) !!(1)qsim,(2)q_flowin,(3)PCP
                ALLOCATE (sGLB%data_obs(1,sGLB%nday_sim0))    !!qobs
                sGLB%data_obs(:,:) = -9999
                sGLB%data_sim(:,:) = -9999
                call Read_Daily_Obs(file_obs,sGLB%iyear0,
     &               sGLB%nyear_warmup,1,sGLB%nday_sim0,
     &               sGLB%data_obs,1,dFill)

            CASE (2)  !!daily reservoir storage (10^4 m3)
!!FILE_OBS:
!!19850101
!!   37.97 
                mCOL = 6
                ALLOCATE (sGLB%data_sim(mCOL,sGLB%nday_sim0)) !!(1)resv,(2)resqsim,(3)resqin,(4)respcp,(5)reseva,(6)ressa
                ALLOCATE (sGLB%data_obs(2,sGLB%nday_sim0))    !!resv,resqobs,
                sGLB%data_obs(:,:) = -9999
                sGLB%data_sim(:,:) = -9999
                call Read_Daily_Obs(file_obs,sGLB%iyear0,
     &               sGLB%nyear_warmup,2,sGLB%nday_sim0,
     &               sGLB%data_obs,1,dFill)

            CASE (3)  !!daily soil water content (mm,convert SWobs to mm)
!!FILE_OBS:
!!19850101
!!   37.97                
                mCOL = 4
                ALLOCATE (sGLB%data_sim(mCOL,sGLB%nday_sim0)) !!(1)SWsim,(2)PCP,(3)Water_Yield,(4)ETa
                ALLOCATE (sGLB%data_obs(1,sGLB%nday_sim0))    !!SWobs
                sGLB%data_obs(:,:) = -9999
                sGLB%data_sim(:,:) = -9999
                call Read_Daily_Obs(file_obs,sGLB%iyear0,
     &               sGLB%nyear_warmup,1,sGLB%nday_sim0,
     &               sGLB%data_obs,1,dFill)
     
            CASE (10) !!monthly streamflow
!!FILE_OBS (ONLY 1 COL-Flow_cms WILL BE READ):
!!USGS Station ID 03609750: Tennessee River at Highway 60 near Paducah, KY
!197310										
!YYYYMM  Flow_cms  Sed_ton  TP_ton   TN_ton  NO2+3_ton NO3_ton  Sed_sd  TP_sd   TN_sd   NO2+3_sd   NO3_sd 
!197310         927     8290     197    1130        424   -9999    1388      21     125         86   -9999
                mCOL = 1  !!Flow_cms
                ALLOCATE (sGLB%data_sim(3,sGLB%nmon_sim))  !!(1)qsim,(2)q_flowin,(3)PCP		 	
                ALLOCATE (sGLB%data_obs(mCOL,sGLB%nmon_sim))
                sGLB%data_obs(:,:) = -9999
                sGLB%data_sim(:,:) = -9999
                write(*,*)">>>Read Observed Monthly FLOW(cms):"
                !!file_obs
                iYearMon_begin = (sGLB%iyear0+sGLB%nyear_warmup)*100+1                !!begin from January
                iYearMon_end = (sGLB%iyear0+sGLB%nyear_warmup
     &                       +sGLB%nyear_sim - 1)*100 + 12  !!end in December
                call Read_Month_NPS(file_obs,
     &               iYearMon_begin,iYearMon_end,mCOL,
     &               sGLB%nmon_sim,sGLB%data_obs)

            CASE (11)  !!monthly HUC8_Runoff
!!FILE_OBS:
!!FIXED DATA FILE DOWNLOADED FROM USGS: 'mv01d_row_data.txt'
                ALLOCATE (sGLB%data_sim(sGLB%nsub_opt*2,sGLB%nmon_sim)) !!runoff + pcp
                ALLOCATE (sGLB%data_obs(1,sGLB%nmon_sim))
                sGLB%data_obs(:,:) = -9999
                sGLB%data_sim(:,:) = -9999
                write(*,*)">>>Read Observed Runoff_HUC8:"
                file_obs = trim(sDIR_OBS)//'mv01d_row_data.txt'  !!
                iYearMon_begin = (sGLB%iyear0+sGLB%nyear_warmup)*100+1                !!begin from January
                iYearMon_end = (sGLB%iyear0+sGLB%nyear_warmup
     &                       +sGLB%nyear_sim - 1)*100 + 12  !!end in December

                ALLOCATE (data_obs_read(sGLB%nmon_sim))
                Call Read_Month_Runoff_HUC8
     &               (file_obs,sStation,iYearMon_begin,iYearMon_end,
     &                sGLB%nmon_sim,data_obs_read)
                sGLB%data_obs(1,:) = data_obs_read
!                write(*,*)sGLB%data_obs(1,:)
            CASE (20:39) !!
!!FILE_OBS:
!!USGS Station ID 03609750: Tennessee River at Highway 60 near Paducah, KY
!197310										
!YYYYMM  Flow_cms  Sed_ton  TP_ton   TN_ton  NO2+3_ton NO3_ton  Sed_sd  TP_sd   TN_sd   NO2+3_sd   NO3_sd 
!197310         927     8290     197    1130        424   -9999    1388      21     125         86   -9999
                mNPS = 5
                mCOL = mNPS*2+1  !!1st COL: Flow_cms
                ALLOCATE (sGLB%data_sim(mNPS + 1,sGLB%nmon_sim))  !!(1)Flow_cms, (2)Sed_ton, (3)TP_ton, (4)TN_ton, (5)NO2+3_ton, (6)NO3_ton		 	
                ALLOCATE (sGLB%data_obs(mCOL,sGLB%nmon_sim))
                sGLB%data_obs(:,:) = -9999
                sGLB%data_sim(:,:) = -9999
                write(*,*)">>>Read Observed Monthly Nutrients in '",
     &                     trim(file_obs),"'"
                !!file_obs
                iYearMon_begin = (sGLB%iyear0+sGLB%nyear_warmup)*100+1                !!begin from January
                iYearMon_end = (sGLB%iyear0+sGLB%nyear_warmup
     &                       +sGLB%nyear_sim - 1)*100 + 12  !!end in December
                call Read_Month_NPS(file_obs,
     &               iYearMon_begin,iYearMon_end,mCOL,
     &               sGLB%nmon_sim,sGLB%data_obs)
            CASE DEFAULT
                write(*,*)">>>NO SPECIFIC CASE WAS SELECTED!!!"
        END SELECT
!!---------------------------------------------------------------------        
        close(sSCE%iFO1)  !!SCEOUT_SUM.dat
        open(unit=sSCE%iFO1,file=sfSCE_SUM,
     &   status="unknown",position = "append")
        write(*,*)">>>EXIT SCEIN SUBROUTINE."
        write(*,*)

!C  END OF SUBROUTINE SCEIN
      return
      end

