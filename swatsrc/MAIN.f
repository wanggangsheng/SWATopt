!      include 'parm.f'
!      include 'scemod.f'
      program main
!! MODIFIED BY GANGSHENG WANG - ORNL, MARCH 2015
!!    this is the main program that reads input, calls the main simulation
!!    model, and writes output.
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    date        |NA            |date simulation is performed where leftmost
!!                               |eight characters are set to a value of
!!                               |yyyymmdd, where yyyy is the year, mm is the 
!!                               |month and dd is the day
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    time        |NA            |time simulation is performed where leftmost
!!                               |ten characters are set to a value of
!!                               |hhmmss.sss, where hh is the hour, mm is the 
!!                               |minutes and ss.sss is the seconds and
!!                               |milliseconds
!!    values(1)   |year          |year simulation is performed
!!    values(2)   |month         |month simulation is performed
!!    values(3)   |day           |day in month simulation is performed
!!    values(4)   |minutes       |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    values(5)   |hour          |hour simulation is performed
!!    values(6)   |minutes       |minute simulation is performed
!!    values(7)   |seconds       |second simulation is performed
!!    values(8)   |milliseconds  |millisecond simulation is performed
!!    zone        |NA            |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    prog        |NA            |program name and version
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: date_and_time
!!    SWAT: getallo, allocate_parms, readfile, readfig
!!    SWAT: readbsn, std1, readwwq, readinpt, std2, storeinitial
!!    SWAT: openwth, headout, simulate, finalbal, writeaa, pestw 


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      USE parm
      USE scemod
      IMPLICIT NONE
      TYPE(sSCE_PAR) sSCE 
        integer irun,nrun, iwgs, iseed
        real pcento,fa0, objOPT
        real, dimension (:), allocatable :: bOBF  !parameter values corresponding bestf
        real, dimension (:,:), allocatable :: bPAR  !parameter values corresponding bestf
        real, dimension (:), allocatable :: parOPT  !best of best par sets
        integer, allocatable:: iwk(:)
        
        character*50 format_bestx, format_xname
        real functn, gasdev
        
        INTEGER t_start,t_end,t_rate,t_elapse  !!time_start, time_end, time_elapsed
        INTEGER, DIMENSION(3):: tHMS  !!call subroutine "Sec2HMS" in "WGSfunc.f"

      prog = "SWAT Jun 11 2014    VER 2012/Rev 627"
      write (*,1000)
 1000 format(1x,"      SWAT2012 MODEL RUN & OPT       ",/,             
     &          "               Rev. 627              ",/,             
     &          "      Soil & Water Assessment Tool    ",/) !!,             
!     &          "               PC Version             ",/,             
!     &          " Program reading from file.cio . . . executing",/)
      
!! process input
      call scein(sSCE)  
      
      write(*,*)'>>>INITIALIZE SWAT VARIABLES...'
      call getallo     
      call allocate_parms
!!=====================================================================
!!SCE_BEGIN
!!MOVED TO SCEfunctn.f
!!      call readfile
!! ...................
!!        !!reinitialize for new scenario
!!        if (scenario > iscen) call rewind_init
!!      end do
!!         end if
!!MOVED TO SCEfunctn.f
!!SCE_END
!!=====================================================================
         
!        call SGLB_WRITE(sGLB)
        write(*,*)'>>>RUN SWAT with INITIAL PARs...'
        call system_clock(t_start,t_rate)
!!WGS: BEG
!!The 1st SWAT-RUN with initial parameters in SWAT input files, NOT included in optimization
        sGLB%iSWAT = -1
     	fa0 = functn(sSCE%npar,sSCE%parINI)
!!Now,sGLB%iSWAT = 0; SWAT-RUN "functn" was called above for the 1st time, sGLB%iSWAT=sGLB%iSWAT+1
!!SCEIN.f, Line 141:  open(319, file=sRead, status='unknown')
!!sRead = trim(sDIR_SCEOUT)//'SCE-SWAT.chk'
!!SCEfunctn.f, Line 154:  write(319,*)j,dmon_obs(j),dmon_sim(j)
!!below, close the file; only if(sGLB%iSWAT.lt.0), write output_sim_vs_obs to the file
	close(319) 
	write(*,*)">>>ATTENTION: Finish Writing SCE-SWAT.chk"
        write(*,*)
!!WGS: END
        call system_clock(t_end)!!timer(t_end)
        t_elapse = (t_end - t_start)/real(t_rate)
        call Sec2HMS(t_elapse,tHMS)
        write(*,*)">>>Elapsed time for running SWAT = ",tHMS(1),"Hours",
     &            tHMS(2),"Minutes",tHMS(3),"Seconds"
        ALLOCATE(parOPT(sSCE%npar))
        
        call EXECUTE_COMMAND_LINE('mkdir -p '//trim(sGLB%dir_swatio)
     &                            //'dirout')
        call EXECUTE_COMMAND_LINE('cp '//trim(sGLB%dir_swatio)//
     &          'output.* '//trim(sGLB%dir_swatio)//'dirout/') 
        write(*,*)">>>Please check Output.* Files ", 
     &            "that were copied to dirout" 
!!!wgs--------------------------------------------------------
        if(sGLB%iMODEL.eq.1) then
        write (*, *) '>>>ENTER SWAT-SCEUA PROGRAM...'
        if(sSCE%nSCE.lt.1) then
            nrun = 1
        else !!if (sSCE%nSCE .gt. 0) then  !!
            nrun = min(sSCE%nSCE, 50)
        end if
      
        allocate(iwk(nrun))
        allocate(bOBF(nrun))
        allocate(bPAR(nrun,sSCE%npar))
        write(format_bestx,*)"(I10,",sSCE%npar+1,"f10.3)"
        
        do irun = 1, nrun
            if (nrun .ne. 1) iseed = IRAND(0)!iseed = jseed(irun)
            call srand(iseed)
            write (*, *) '@ SCE-UA Run Number', irun, 
     &        ' Random Seed = ',iseed
!            CALL SCEPAR_WRITE(sSCE)
            call sceua(sSCE)   
     
            bOBF(irun) = sSCE%objOPT
            bPAR(irun,:) = sSCE%parOPT
        end do
        
        call indexx(nrun, bOBF, iwk)  !rank best OBF
        parOPT = bPAR(iwk(1),:)
        objOPT = functn(sSCE%npar,parOPT)
!        write(sSCE%iFO2,*)
       
        do irun = 1,nrun
            write(sSCE%iFO2,format_bestx)irun,bOBF(iwk(irun)),
     &       bPAR(iwk(irun),:)
        end do
      end if !!if (sGLB%iModel .gt. 0)
!!---------------------------------------------------------------------      
      do i = 101, 109       !Claire 12/2/09: change 1, 9  to 101, 109.
        close (i)
      end do
!!      close(124)
      
      close(sSCE%iFO1)
      close(sSCE%iFO2)
      close(sSCE%iFO3)  
      
      write (*,1001)
 1001 format (/," Execution successfully completed ")
	
        iscen=1
!! file for Mike White to review to ensure simulation executed normally
      open (9999,file=trim(sGLB%dir_swatio)//'fin.fin')
      write (9999,*) 'Execution successful'
      close (9999)
      
      stop
      end
