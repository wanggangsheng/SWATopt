      subroutine writed
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine contains the daily output writes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha       |ha            |area of watershed in hectares
!!    hrupest(:)  |none          |pesticide use flag:
!!                               | 0: no pesticides used in HRU
!!                               | 1: pesticides used in HRU
!!    hrupstd(:,1,:)|mg pst      |amount of pesticide type in surface runoff
!!                               |contribution to stream from HRU on day
!!                               |(in solution)
!!    hrupstd(:,2,:)|mg pst      |amount of pesticide type in surface runoff
!!                               |contribution to stream from HRU on day
!!                               |(sorbed to sediment)
!!    iida        |julian date   |current day of simulation 
!!    iprint      |none          |print code:
!!                               |0 monthly
!!                               |1 daily
!!                               |2 annually
!!    iprp        |none          |print code for output.pst file
!!                               |0 do not print pesticide output
!!                               |1 print pesticide output
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    iyr         |year          |year being simulated (eg 1980)
!!    mstdo       |none          |watershed output array size
!!    nhru        |none          |number of HRUs in watershed
!!    npmx        |none          |number of different pesticides used in
!!                               |the simulation
!!    subtot      |none          |number of subbasins in watershed
!!    wshddayo(1) |mm H2O        |average amountof precipitation in watershed
!!                               |for the day
!!    wshddayo(3) |mm H2O        |surface runoff in watershed for day
!!    wshddayo(4) |mm H2O        |lateral flow contribution to streamflow in
!!                               |watershed for day
!!    wshddayo(5) |mm H2O        |water percolation past bottom of soil profile
!!                               |in watershed for day
!!    wshddayo(6) |mm H2O        |water yield to streamflow from HRUs in
!!                               |watershed for day
!!    wshddayo(7) |mm H2O        |actual evapotranspiration in watershed
!!                               |for day
!!    wshddayo(12)|metric tons   |sediment yield from HRUs in watershed 
!!                               |for day
!!    wshddayo(35)|mm H2O        |amount of water stored in soil profile in
!!                               |watershed for day
!!    wshddayo(40)|kg N/ha       |organic N loading to stream in watershed for
!!                               |day
!!    wshddayo(41)|kg P/ha       |organic P loading to stream in watershed for
!!                               |day
!!    wshddayo(42)|kg N/ha       |nitrate loading to stream in surface runoff
!!                               |in watershed for day
!!    wshddayo(43)|kg P/ha       |soluble P loading to stream in watershed for
!!                               |day
!!    wshddayo(44)|kg N/ha       |plant uptake of N in watershed for day
!!    wshddayo(45)|kg N/ha       |nitrate loading to stream in lateral flow
!!                               |in watershed for day
!!    wshddayo(46)|kg N/ha       |nitrate percolation past bottom of soil
!!                               |profile in watershed for day
!!    wshddayo(104)|mm H2O        |groundwater contribution to stream in
!!                               |watershed on day
!!    wshddayo(108)|mm H2O        |potential evapotranspiration in watershed
!!                               |on day
!!    wshddayo(109)|mm H2O        |drainage tile flow contribution to stream
!!                               |in watershed on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupstm(:,1,:)|mg pst      |amount of pesticide type in surface runoff
!!                               |contribution to stream from HRU during month
!!                               |(in solution)
!!    hrupstm(:,2,:)|mg pst      |amount of pesticide type in surface runoff
!!                               |contribution to stream from HRU during month
!!                               |(sorbed to sediment)
!!    hrupstm(:,3,:)|mg pst/ha   |total pesticide loading to stream in surface
!!                               |runoff from HRU during month
!!    wshddayo(12)|metric tons/ha|sediment yield from HRUs in watershed 
!!                               |for day
!!    wshdmono(:) |varies        |watershed monthly output array
!!                               |(see definitions for wshddayo array elements)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |counter
!!    k           |none          |counter
!!    pstsum      |mg pst        |pesticide loading in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: rchday

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm

      integer :: j, k
      real :: pstsum


!!    write statement to new output file (output.swr)
!!    writes out the amount of water stored in the soil layer
!!SCE_BEGIN
      IF(sGLB%iModel.EQ.0) THEN !!run SWAT
      if (isto > 0) then 
        do j = 1, nhru
          write (129,5000) iida, j, (sol_st(j1,j), j1 = 1, sol_nly(j))
!          write (129,5000) iida, subnum(j), hruno(j),                   
!     &             (sol_no3(j1,j), j1 = 1, sol_nly(j))
        enddo
      end if
      END IF !!IF(sGLB%iModel.EQ.0) THEN !!run SWAT
!!SCE_END
!!---------------------------------------------------------------------
!!SCE_BEGIN
!      write(*,*)"rchday.f"
      if(curyr.gt.sGLB%nyear_warmup) then
          if(sGLB%itype_opt.eq.0) then  !!streamflow
            sGLB%data_sim(1,sGLB%iday_sim) = rchdy(2,sGLB%icode_sub) !!flow out    
            sGLB%data_sim(2,sGLB%iday_sim) = rchdy(1,sGLB%icode_sub) !!flow_in 
            sGLB%data_sim(3,sGLB%iday_sim) = sub_subp(sGLB%icode_sub) !!pcp 
          elseif(sGLB%itype_opt.eq.3) then !!Soil Water
            sGLB%data_sim(1,sGLB%iday_sim) = sub_sw(sGLB%icode_sub)   !!SW: mm
            sGLB%data_sim(2,sGLB%iday_sim) = sub_subp(sGLB%icode_sub)  !!PCP
            sGLB%data_sim(3,sGLB%iday_sim) = sub_etday(sGLB%icode_sub) !!ETa
            sGLB%data_sim(4,sGLB%iday_sim) = sub_wyld(sGLB%icode_sub)  !!water yield
!            write(*,*)sGLB%iday_sim,sGLB%data_sim(:,sGLB%imon_sim)
          end if
      end if
!!SCE_END
!!---------------------------------------------------------------------
      
      if (iprint == 1.or.iprint==3) then
        if (da_ha < 1.e-9) then
            IF(sGLB%iModel.EQ.0) THEN !!run SWAT
	    call rchday  !!SCE
	    call rseday  !!SCE
            END IF !!IF(sGLB%iModel.EQ.0) THEN !!run SWAT
	    return
	  end if

        !! daily write to output.std
!!SCE_BEGIN
        IF(sGLB%iModel.EQ.0) THEN !!run SWAT
        if (iscen == 1) then
        write (26,6200) iida, wshddayo(1), wshddayo(3), wshddayo(4),    
     &                 wshddayo(104), wshddayo(5), wshddayo(109),       
     &                 wshddayo(35), wshddayo(7), wshddayo(108),        
     &                 wshddayo(6), wshddayo(12) / da_ha, wshddayo(42), 
     &                 wshddayo(45), wshddayo(46), wshddayo(44),        
     &                 wshddayo(40), wshddayo(43), wshddayo(41),        
     &                 wshddayo(111)
        else if (isproj == 1) then
        write (19,6200) iida, wshddayo(1), wshddayo(3), wshddayo(4),    
     &                 wshddayo(104), wshddayo(5), wshddayo(109),       
     &                 wshddayo(35), wshddayo(7), wshddayo(108),        
     &                 wshddayo(6), wshddayo(12) / da_ha, wshddayo(42), 
     &                 wshddayo(45), wshddayo(46), wshddayo(44),        
     &                 wshddayo(40), wshddayo(43), wshddayo(41)
        endif
        END IF !!IF(sGLB%iModel.EQ.0) THEN !!run SWAT
!!SCE_END
        !! daily write to pesticide output file (output.pst) for HRUs
        do j = 1, nhru
          if (hrupest(j) == 1) then
          pstsum = 0.
          do k = 1, npmx
            pstsum = pstsum + hrupstd(k,1,j) + hrupstd(k,2,j)
          end do
!!SCE_BEGIN
          IF(sGLB%iModel.EQ.0) THEN !!run SWAT
          if (pstsum > 0. .and. iprp == 1) then
                write (30,5100) subnum(j), hruno(j), iyr, iida,         
     &                     (hrupstd(k,1,j), hrupstd(k,2,j), k = 1, npmx)
          end if
          END IF !!IF(sGLB%iModel.EQ.0) THEN !!run SWAT
!!SCE_END
          end if
        end do
        
        IF(sGLB%iModel.EQ.0) THEN !!run SWAT

        !! write daily reach output
        call rchday  !!SCE

        !! write daily sediment routing output (.sed)
        call rseday  !!SCE
        END IF !!IF(sGLB%iModel.EQ.0) THEN !!run SWAT

      end if

      !! write velocities for steve/woody in temp file (Balaji)
!!SCE_BEGIN
      IF(sGLB%iModel.EQ.0) THEN !!run SWAT
      if (itemp == 1 .and. nrch > 0) then 
         write (141,5001) iida,iyr,(vel_chan(k),k= 1,nrch)
         write (142,5001) iida,iyr,(dep_chan(k),k= 1,nrch)
      end if 
      END IF !!IF(sGLB%iModel.EQ.0) THEN !!run SWAT
!!SCE_END
      
!! monthly watershed output
      wshddayo(12) = wshddayo(12) / (da_ha + 1.e-6)

      wshdmono = wshdmono + wshddayo
      wpstmono = wpstmono + wpstdayo
      hrupstm = hrupstm + hrupstd

!5000  format(i5,1x,a5,a4,1x,500e12.4)
5000  format (i5,1x,i5,1x,500e12.4)
5001  format(2i5,500f12.4)
5100  format(1x,a5,a4,1x,i4,1x,i3,1x,250(e16.4,1x))
5200  format(i7,i9,i6,i5,1x,e9.4,f12.3,f7.1,f14.3)
!!6200  format(i5,13f7.2,2f5.2,1x,5f8.2)
6200  format(i5,15f8.2,1x,4f8.2)
      return
      end
