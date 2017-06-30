!!A Collection of User-Defined Functions & Subroutines
!!Author: Wang, Gangsheng
!!ORNL
!!wangg@ornl.gov


!!===================================================================
      subroutine Sec2HMS(tSec,tHMS)
          !!convert Seconds to Hours-Minutes-Seconds
          integer tSec
          integer, dimension(3) :: tHMS
          tHMS(1) = tSec/3600  !!hours
          tHMS(2) = (tSec - 3600*tHMS(1))/60
          tHMS(3) = tSec - 3600*tHMS(1) - 60*tHMS(2)
          return
      end subroutine
!!===================================================================
!!    subroutine: randomly select m distinct integers from the range [1..npg]      
      subroutine selectINT(npg,m,lcs)  
      IMPLICIT NONE
      integer npg, m, lcs(m)
      integer k,k1,lpos
      real randx
      if (m .ge. npg) then
        do k = 1, npg
          lcs(k) = k
        end do
      else  !!m < npg
        randx = rand() !rand = ran1(iseed1)
        lcs(1) = 1 + int(npg + 0.5 - sqrt( (npg+.5)**2 -
     &         npg * (npg+1) * randx ))
        do k = 2, m
  60    randx = rand() !rand = ran1(iseed1)
          lpos = 1 + int(npg + 0.5 - sqrt((npg+.5)**2 -
     &         npg * (npg+1) * randx ))
          do k1 = 1, k-1
            if (lpos .eq. lcs(k1)) go to 60
          end do
          lcs(k) = lpos
        end do
      end if
!      write(*,*)'subroutine lcs = ',lcs
      return
      end
!!===================================================================
      SUBROUTINE par_new(par,isub_cur,x3,ifopt,ivar3,nsub3,isub3)
!!update SWAT parameter (single value)
!!par: old value
!!x3: new value
!!isub_cur: curren subbasin ID in SWAT loop
!!ifopt: whether the parameter to be optimized (1) or not (0)
!!ivar3: variation method for parameter values: 1-absolute value; 2-relative value (old value multiplied by x3)
!!nsub3: # of subbasins whose parameters to be calibrated (changed)
!!isub3(nsub3): array to calibrated subbasins
!      USE parm
      IMPLICIT NONE
      INTEGER jua,isub_cur,ifopt,ivar3,nsub3 !!loop;current subbasin in SWAT simulation, if optimization, variation method, #of calibrated subbasins
      INTEGER isub3(nsub3) !!array of calibrated subbasins
      REAL par,par_old,x3
      par_old = par
      if(ifopt.gt.0) then
          do jua = 1, nsub3
              if(isub3(jua).eq.isub_cur) then
                  if(ivar3.eq.2) then  !relative value
                    par = par_old*x3  !relative value, multiplied by a factor
                  else
                    par = x3 !absolute value
                  end if
              end if
          end do
      end if
      return
      END
!!===================================================================
      SUBROUTINE par_bsn_new(par,x3,ifopt,ivar3)
!!update SWAT parameter (single value)
!!par: old value
!!x3: new value
!!isub_cur: curren subbasin ID in SWAT loop
!!ifopt: whether the parameter to be optimized (1) or not (0)
!!ivar3: variation method for parameter values: 1-absolute value; 2-relative value (old value multiplied by x3)
!!nsub3: # of subbasins whose parameters to be calibrated (changed)
!!isub3(nsub3): array to calibrated subbasins
!      USE parm
      IMPLICIT NONE
      INTEGER ifopt,ivar3  !!jua,isub_cur,nsub3 !!loop;current subbasin in SWAT simulation, if optimization, variation method, #of calibrated subbasins
!!      INTEGER isub3(nsub3) !!array of calibrated subbasins
      REAL par,par_old,x3
      par_old = par
      if(ifopt.gt.0) then
!!          do jua = 1, nsub3
!!              if(isub3(jua).eq.isub_cur) then
                  if(ivar3.eq.2) then  !relative value
                    par = par_old*x3  !relative value, multiplied by a factor
                  else
                    par = x3 !absolute value
                  end if
!!              end if
!!          end do
      end if
      return
      END
!!===================================================================
      SUBROUTINE par_res_new(par,ires_cur,x3,ifopt,ivar3,ires)
!!update SWAT reservoir parameter (single value)
!!par: old value
!!x3: new value
!!ires_cur: curren reservoir ID in SWAT loop
!!ifopt: whether the parameter to be optimized (1) or not (0)
!!ivar3: variation method for parameter values: 1-absolute value; 2-relative value (old value multiplied by x3)
!!ires: calibrated reservoir #
!      USE parm
      IMPLICIT NONE
      INTEGER ires_cur,ifopt,ivar3,ires !!loop;current subbasin in SWAT simulation, if optimization, variation method, #of calibrated subbasins
!!      INTEGER isub3(nsub3) !!array of calibrated subbasins
      REAL par,par_old,x3
      par_old = par
      if(ifopt.gt.0) then
!!          do jua = 1, nsub3
              if(ires.eq.ires_cur) then
                  if(ivar3.eq.2) then  !relative value
                    par = par_old*x3  !relative value, multiplied by a factor
                  else
                    par = x3 !absolute value
                  end if
              end if
!!          end do
      end if
      return
      END
!!===================================================================
      SUBROUTINE pars_new(pars,nly,isub_cur,x3,ifopt,ivar3,nsub3,isub3)
!!update SWAT parameter (multiple values, e.g.,SOL_AWC, SOL_K, the same parameter with different values at multiple soil layers)
!!pars: old values (e.g., at multiple soil layers)
!!x3: new value (single value, e.g., only for 1st soil layer), parameter values for other layers will be changed proportionally
!!isub_cur: curren subbasin ID in SWAT loop
!!ifopt: whether the parameter to be optimized (1) or not (0)
!!ivar3: variation method for parameter values: 1-absolute value; 2-relative value (old value multiplied by x3)
!!nsub3: # of subbasins whose parameters to be calibrated (changed)
!!isub3(nsub3): array to calibrated subbasins
      IMPLICIT NONE
      integer nly !!# of soil layers
      real,dimension(nly):: pars,pars_old
      INTEGER j,jua,isub_cur,ifopt,ivar3,nsub3 !!loop;current subbasin in SWAT simulation, if optimization, variation method, #of calibrated subbasins
      INTEGER isub3(nsub3) !!array of calibrated subbasins
      real x3
      pars_old = pars
      if(ifopt.gt.0) then
          do jua = 1, nsub3
              if(isub3(jua).eq.isub_cur) then
                  if(ivar3.eq.2) then  !relative value
                    do j = 1, nly
                        pars(j) = pars_old(j)*x3  !relative value, multiplied by a factor
                    end do
                  else  !absolute value
                    pars(1) = x3  !absolute value
                    do j = 2, nly
                        pars(j) = x3*pars_old(j)/max(1E-6,pars_old(1))  !absolute value
                    end do
                  end if
              end if
          end do
      end if
      RETURN 
      END
!!===================================================================	
	  real function fSUM(nArray,xx,dFill)
!!compute SUM of all elements in an Array. 
!        implicit real*8 (a-h,o-z)
        IMPLICIT NONE
        INTEGER nArray
        REAL, DIMENSION(nArray):: xx
        REAL dFill
        INTEGER jua
        fSUM = 0.0
        do jua = 1,nArray
            if(xx(jua).ne.dFill) then
                fSUM = fSUM + xx(jua)
            end if
        end do
        return
        end
!!===================================================================
	  real function fAVG(nArray,xx,dFill)
!!compute SUM of all elements in an Array. 
!        implicit real*8 (a-h,o-z)
        IMPLICIT NONE
        INTEGER nArray,k
        REAL, DIMENSION(nArray):: xx
        REAL dFill
        INTEGER jua
	    fAVG = 0.0
        k = 0
	    do jua = 1,nArray
            if(xx(jua).ne.dFill) then
                k = k + 1
		        fAVG = fAVG + xx(jua)
            end if
	    end do
        if(k<1) then
            fAVG = dFill
        else
            fAVG = fAVG/k
        end if
        return
        end
!!===================================================================	
	  real function f1RAVG(nn,dobs,dsim,dFill)
!!f1RAVG = abs(1.0 - fAVG_sim/fAVG_obs) 
!        implicit real*8 (a-h,o-z)
        IMPLICIT NONE
        INTEGER nn
        REAL, DIMENSION(nn):: dobs,dsim
        REAL dFill
        REAL fAVG  !!FUNCTION
        REAL fAVG_obs,fAVG_sim
        
        fAVG_obs = fAVG(nn,dobs,dFill)
        fAVG_sim = fAVG(nn,dsim,dFill)
        if(fAVG_obs.eq.0.or.fAVG_obs.eq.dFill) then
            f1RAVG = dFill
            return
        else 
            f1RAVG = abs(1.0 - fAVG_sim/fAVG_obs)
            return  
        end if
        end
!!===================================================================	
	  real function fSUM2(nArray,xx,iBegin,iEnd,dFill)
!!compute AVERAGE of selected elements (iBegin to iEnd) in an Array. 
!        implicit real*8 (a-h,o-z)
        IMPLICIT NONE
        INTEGER nArray, iBegin, iEnd
        REAL, DIMENSION(nArray):: xx
        REAL dFill
	    INTEGER jua
        
	    fSUM2 = 0.0
	    do jua = iBegin,iEnd
            if(xx(jua).ne.dFill) then
		        fSUM2 = fSUM2 + xx(jua)
            end if
	    end do
	    return
	    end
!!===================================================================	
	  real function fAVG2(nArray,xx,iBegin,iEnd,dFill)
!!compute AVERAGE of selected elements (iBegin to iEnd) in an Array. 
!        implicit real*8 (a-h,o-z)
        IMPLICIT NONE
        INTEGER nArray, iBegin, iEnd
        REAL, DIMENSION(nArray):: xx
        REAL dFill
	    INTEGER jua,k
	    fAVG2 = 0.0
        k = 0
	    do jua = iBegin,iEnd
            if(xx(jua).ne.dFill) then
                k = k + 1
		        fAVG2 = fAVG2 + xx(jua)
            end if
	    end do
        if(k.gt.0) then
            fAVG2 = fAVG2/k !!(iEnd - iBegin + 1)
        else
            fAVG2 = dFill
        end if
	    return
	    end
!!===================================================================
	
	  real function FuncLxy(nArray, xx, yy,dFill)
        IMPLICIT NONE
        INTEGER nArray
        REAL, DIMENSION(nArray):: xx,yy
        REAL, DIMENSION(nArray):: xx1,yy1
        INTEGER jua,k
        REAL dFill
        REAL sumxy, sumx, sumy
        REAL fSUM2 !!FUNCTION

        sumxy = 0.0
        k = 0
        do jua=1,nArray
            if(xx(jua).ne.dFill.and.yy(jua).ne.dFill) then
                k = k + 1
                xx1(k) = xx(jua)
                yy1(k) = yy(jua)
                sumxy = sumxy+xx(jua)*yy(jua)
            end if
	    end do
        if(k<1) then
            FuncLxy = dFill 
        else
            sumx = fSUM2(nArray,xx1,1,k,dFill)!!fSUM(nArray,xx,dFill)
            sumy = fSUM2(nArray,yy1,1,k,dFill)!!fSUM(nArray,yy,dFill)
            FuncLxy = sumxy - sumx*sumy/k !!FuncLxy = sumxy - sumx*sumy/nArray
        end if
        return
      end

        real function  fCORR(nArray, xx, yy,dFill)
!	implicit real*8 (a-h,o-z)
        IMPLICIT NONE
        INTEGER nArray
        REAL, DIMENSION(nArray):: xx,yy
        REAL dFill
        REAL aLxx, aLyy, aLxy
        REAL FuncLxy !!FUNCTION
        !!real aLxx,aLyy,aLxy
        aLxx = FuncLxy(nArray,xx,xx,dFill)
	    aLyy = FuncLxy(nArray,yy,yy,dFill)
        aLxy = FuncLxy(nArray,xx,yy,dFill)
        if(aLxx.eq.dFill.or.aLyy.eq.dFill) then
            fCORR = dFill
        else
            fCORR = aLxy/SQRT(aLxx*aLyy)
        end if
        return
      end 

!!===================================================================
!	real function fNSE(nArray, Dataobs,Datasim)
!!!Nash-Sutcliffe Efficiency
!!	implicit real*8 (a-h,o-z)
!        IMPLICIT NONE
!        INTEGER nArray
!        REAL, DIMENSION(nArray):: Dataobs,Datasim
!!	dimension Dataobs(1000),Datasim(1000)
!	INTEGER jua
!        REAL AVGobs,sum11,sum12
!        REAL fSUM!!FUNCTION
!        
!	AVGobs = fSUM(nArray,Dataobs)/nArray
!
!	sum11 = 0
!	sum12 = 0
!
!	do jua = 1,nArray
!		sum11 = sum11+(Datasim(jua) - Dataobs(jua))
!     $	*(Datasim(jua) - Dataobs(jua))
!		sum12 = sum12+(Dataobs(jua) - AVGobs)
!     $	*(Dataobs(jua) - AVGobs)
!      end do
!	fNSE=1 - sum11/sum12
!	return
!	end
!!===================================================================
      real function fNSE(nArray, Dataobs, Datasim, dFill)
!Nash-Sutcliffe Efficiency Coefficient; Determination Coefficient
!data excluded from calculation if Dataobs(i) = const_FillValue 
!    real(8), parameter:: const_FillValue = -999d0
        IMPLICIT NONE
        integer, intent(in):: nArray
        REAL dFill
        REAL, DIMENSION(nArray):: Dataobs,Datasim
        real AVGobs,sum11,sum12
        integer i,j, m
        INTEGER iData(nArray)

        m = 0
        sum11 = 0
        do i = 1, nArray
            if(Dataobs(i).ne.dFill) then
                m = m + 1
                iData(m) = i
                sum11 = sum11 + Dataobs(i)
            end if
        end do
        if (m.lt.2) then
            fNSE = dFill
            return
        end if
        AVGobs = sum11/real(m) !SUM(Dataobs)/nArray

        sum11 = 0
        sum12 = 0
        do i = 1, m
            j = iData(i)
            sum11 = sum11+(Datasim(j) - Dataobs(j))
     &                   *(Datasim(j) - Dataobs(j))
            sum12 = sum12+(Dataobs(j) - AVGobs)
     &                   *(Dataobs(j) - AVGobs)
        end do

        fNSE=1 - sum11/sum12
        return
      end  
!!===================================================================
      real function f1NSE(nArray, Dataobs, Datasim, dFill)
!f1NSE = 1.0 - fNSE
!NSE:Nash-Sutcliffe Efficiency Coefficient; Determination Coefficient
!data excluded from calculation if Dataobs(i) = const_FillValue 
!    real(8), parameter:: const_FillValue = -999d0
        IMPLICIT NONE
        integer, intent(in):: nArray
        REAL dFill
        REAL, DIMENSION(nArray):: Dataobs,Datasim
        real AVGobs,sum11,sum12
        integer i,j, m
        INTEGER iData(nArray)

        m = 0
        sum11 = 0
        do i = 1, nArray
            if(Dataobs(i).ne.dFill) then
                m = m + 1
                iData(m) = i
                sum11 = sum11 + Dataobs(i)
            end if
        end do
        if (m.lt.2) then
            f1NSE = dFill
            return
        end if
        AVGobs = sum11/real(m) !SUM(Dataobs)/nArray

        sum11 = 0
        sum12 = 0
        do i = 1, m
            j = iData(i)
            sum11 = sum11+(Datasim(j) - Dataobs(j))
     &                   *(Datasim(j) - Dataobs(j))
            sum12 = sum12+(Dataobs(j) - AVGobs)
     &                   *(Dataobs(j) - AVGobs)
        end do

        f1NSE = sum11/sum12
        return
      end   
!!===================================================================
        real function fMARE(nArray, Dataobs,Datasim,dFill)
!!Mean Absolute Relative Error
!	implicit real*8 (a-h,o-z)
!	dimension Dataobs(1000),Datasim(1000)
        IMPLICIT NONE
        INTEGER nArray
        REAL, DIMENSION(nArray):: Dataobs,Datasim
        REAL sum1,dFill
        integer i,j, m, iData(nArray)

        m = 0
        do i = 1, nArray
            if(Dataobs(i).ne.dFill) then
                m = m + 1
                iData(m) = i
            end if
        end do
        if (m.lt.1) then
            fMARE = dFill
            return
        end if
        sum1 = 0
	    do i = 1,m
            j = iData(i)
            sum1 = sum1+ABS(Datasim(j) - Dataobs(j))
     &       /max(ABS(Dataobs(j)),1.0E-6)
        end do
	    fMARE=sum1/real(m)
	    return
	    end
!!===================================================================
        FUNCTION fMAXarray(nn,dobs,dFill)
            IMPLICIT NONE
            REAL fMAXarray,dFill
            INTEGER nn,j
            REAL, DIMENSION(nn)::dobs
            fMAXarray = dFill
            do j = 1,nn
                if(dobs(j).ne.dFill.and.dobs(j).gt.fMAXarray) then
                    fMAXarray = dobs(j)
                end if
            end do
            return
        END FUNCTION
!!===================================================================
        FUNCTION fMINarray(nn,dobs,dFill)
            IMPLICIT NONE
            REAL fMINarray,dFill
            INTEGER nn,j
            REAL, DIMENSION(nn)::dobs
            fMINarray = 1.0E+10
            do j = 1,nn
                if(dobs(j).ne.dFill.and.dobs(j).lt.fMINarray) then
                    fMINarray = dobs(j)
                end if
            end do
            if(fMINarray.eq.1.0E+10) then
                fMINarray = dFill
            end if
            return
        END FUNCTION
!!===================================================================
        FUNCTION fRANGEarray(nn,dobs,dFill)
        !!Normalized Root-Mean-Square Error
            IMPLICIT NONE
            REAL fRANGEarray,fMAXarray,fMINarray,dFill
            INTEGER nn
            REAL, DIMENSION(nn)::dobs
            REAL dobs_max,dobs_min,dobs_range
            
            dobs_max = fMAXarray(nn,dobs,dFill)
            dobs_min = fMINarray(nn,dobs,dFill)
            if (dobs_max.gt.dobs_min)then
                fRANGEarray = dobs_max - dobs_min
            elseif(dobs_max.ne.dFill) then  
                fRANGEarray = dobs_max
            else !!dobs_max = dobs_min = dFill
                fRANGEarray = dFill
            end if
            return
        END FUNCTION
        
!!===================================================================
        FUNCTION fNRMSE(nn,dobs,dobs_sd,dsim,dFill)
        !!Normalized Root-Mean-Square Error
            IMPLICIT NONE
            REAL, PARAMETER::dtol = 1.0E-6
            REAL fNRMSE,fRANGEarray
            INTEGER nn,j,k
            REAL, DIMENSION(nn):: dobs,dobs_sd,dsim
            REAL dFill,sum1,dobs_range
            
            dobs_range = fRANGEarray(nn,dobs,dFill) 
            
            sum1 = 0
            k = 0
            DO j = 1,nn
                if(dobs_sd(j).eq.dFill.or.dobs_sd(j).le.dtol) then
                    dobs_sd(j) = dobs_range
                end if
                
                if(dobs(j).ne.dFill) then
                    if(dobs_sd(j).ne.dFill) then  
                        sum1 = sum1 + 
     &                      (dsim(j)-dobs(j))*(dsim(j)-dobs(j))
     &                      /(dobs_sd(j)*dobs_sd(j))  
                        k = k + 1
                    end if
                end if
                if(k.gt.0) then
                    fNRMSE = sqrt(sum1/k)
                else
                    fNRMSE = dFill
                end if
            end do
            return
        END FUNCTION
!!===================================================================
!	real function fMARE(nArray, Dataobs,Datasim)
!!!Mean Absolute Relative Error
!!	implicit real*8 (a-h,o-z)
!!	dimension Dataobs(1000),Datasim(1000)
!        IMPLICIT NONE
!        INTEGER nArray
!        REAL, DIMENSION(nArray):: Dataobs,Datasim
!	INTEGER jua
!        REAL sum1
!        sum1 = 0
!	do jua = 1,nArray
!            sum1 = sum1+ABS(Datasim(jua) - Dataobs(jua))
!     &       /max(ABS(Dataobs(jua)),1.0E-6)
!        end do
!	fMARE=sum1/nArray
!	return
!	end
!!===================================================================

	    Integer FUNCTION nDaysUPMon(iYear,iMon)
!!Julian day of the last day of a month (iMon) in a year (iYear)
        IMPLICIT NONE
	    integer i,iYear,iMon,nDOM1(12),nDOM2(12)
        INTEGER nDaysofYear !!FUNCTION
	    DATA nDOM1 /31,28,31,30,31,30,31,31,30,31,30,31/
	    Data nDOM2 /31,29,31,30,31,30,31,31,30,31,30,31/
	
	    SELECT CASE(iMON)
	    CASE(1)
		    nDaysUPMon = nDOM1(1)
	    CASE (2:12)
		    nDaysUPMon = 0
		    do i=1,iMON
			    nDaysUPMon = nDaysUPMon+nDOM1(i)
		    end do
		    if(nDaysofYear(iYear)==366) nDaysUPMon = nDaysUPMon+1
	    END SELECT

	    return
	    END FUNCTION nDaysUPMon

!!=====================================================================

	    Integer FUNCTION nDaysofMon(iYear,iMon)
!!# of days in a specific (iYear, iMon)
        IMPLICIT NONE
	    integer i,iYear,iMon,nDOM1(12),nDOM2(12)
        INTEGER nDaysofYear !!FUNCTION
	    DATA nDOM1 /31,28,31,30,31,30,31,31,30,31,30,31/
	    Data nDOM2 /31,29,31,30,31,30,31,31,30,31,30,31/
	
        do i=1,12
            nDaysofMon = nDOM1(iMon)
        end do
	
        if(nDaysofYear(iYear)==366.and.iMon==2) then
            nDaysofMon = nDOM2(iMon)
        end if

        return
        END FUNCTION nDaysofMon
!!===========================================================
	    Integer FUNCTION nDaysofYear(iyear)
!!# of days in a year
!c	calculate the days of the year
        IMPLICIT NONE
        integer iyear
        if(mod(iyear,100).eq.0) then
            if(mod(iyear,400).eq.0) then
                ndaysofyear = 366
            else
                ndaysofyear = 365
            end if
        else
            if(mod(iyear,4).eq.0) then
                ndaysofyear = 366
            else
                ndaysofyear = 365
            end if
        end if
        END FUNCTION ndaysofyear

!!===========================================================
        Integer FUNCTION nMonths(iYearMon_begin,iYearMon_end)
        !!# OF MONTHs between two Year_Month (boundaries included)
        IMPLICIT NONE
        INTEGER iYearMon_begin,iYearMon_end
        INTEGER iYear_begin, iYear_end, iMon_begin,iMon_end
        iYear_begin = iYearMon_begin/100
        iYear_end = iYearMon_end/100
        iMon_begin = iYearMon_begin - iYear_begin*100
        iMon_end = iYearMon_end - iYear_end*100
        nMonths = (iYear_end - iYear_begin - 1)*12 + 
     &    (12 - iMon_begin + 1) + iMon_end
        return
        END FUNCTION nMonths
!!===========================================================
        Subroutine Read_Month_Runoff_HUC8
     &        (datafile,sHUC8,iYearMon_begin,iYearMon_end,ndata,runoff)
        IMPLICIT NONE
    !!"mv01d_row_data.txt", 
    !!# of columns: 1357 = 1st column + 1356 (113 hydrological years*12 months); Oct-1900~Sept-2013  
    !!# of rows: 2111 = 1st row + 2110 HUC8
        INTEGER,PARAMETER::nRow = 2110,nCol = 1356
        INTEGER,PARAMETER::iYearMon1 = 190010, iYearMon2 = 201309
        character*100 datafile 
        character*8 sHUC8,sHUC8_read
        INTEGER iYearMon_begin,iYearMon_end,ndata
        INTEGER nMonths !!function call
        
        INTEGER i,iCol_begin,iCol_end
        REAL runoff(ndata)
        REAL runoff_all(nCol)
        INTEGER ndata0
        
        iCol_begin = nMonths(iYearMon1,iYearMon_begin)
        iCol_end = nMonths(iYearMon1,iYearMon_end)
        ndata0 = iCol_end - iCol_begin + 1
        if(ndata0.ne.ndata) then
            write(*,*)"ndata (No. of data) should be ",ndata0
            return
        end if
        open(unit=101,file = datafile,status='old')
        read(101,*)  !!head
        do i = 1,nRow
            read(101,*)sHUC8_read,runoff_all(1:nCol)
            if(sHUC8_read.eq.sHUC8) goto 200
        end do
200     runoff(1:ndata) = runoff_all(iCol_begin:iCol_end)
        close(101)
        return
        END
!!===========================================================
        Subroutine Read_Month_NPS
     &        (datafile,iYearMon_begin,iYearMon_end,mCOL,nMonth,dOBS)
        IMPLICIT NONE
    !!Read Monthly Nutrient
    !!e.g. "NPS_03609750.dat", 
    !!# of columns: 1357 = 1st column + 1356 (113 hydrological years*12 months); Oct-1900~Sept-2013  
    !!# of rows: 2111 = 1st row + 2110 HUC8
!        INTEGER,PARAMETER::nRow = 2110,nCol = 1356
!        INTEGER,PARAMETER::iYearMon1 = 190010, iYearMon2 = 201309
!        INTEGER, PARAMETER::mNPS = 5  !!# of NPS for NPS data:TN,TP,Sediment,NO2+NO3,NO3
!        INTEGER mNPS
        INTEGER mCOL !!= 1+2*mNPS  !!Mean Value + SD1st; 1st COLumn: Flow_cms,
        character*100 datafile 
        character*8 sHUC8,sHUC8_read
        INTEGER iYearMon1,iYearMon_begin,iYearMon_end,nMonth
        INTEGER nMonths !!function call
        
        INTEGER i,iRow_begin,iRow_end
        REAL,DIMENSION(mCOL,nMonth):: dOBS
!        REAL runoff_all(nCol)
        INTEGER eof,ivoid,nMon0
        
!        mCOL = 1+2*mNPS !!Mean Value + SD1st; 1st COLumn: Flow_cms,
        
        open(unit=101,file = datafile,status='old')
        read(101,*)  !!head
        read(101,*)iYearMon1
        read(101,*) !!head: Flow_cms	TN_ton	TP_ton 	SiO2_ton NO2+3_ton	NO3_ton	
        
        iRow_begin = nMonths(iYearMon1,iYearMon_begin)
        iRow_end = nMonths(iYearMon1,iYearMon_end)
        nMon0 = iRow_end - iRow_begin + 1
        if(nMon0.ne.nMonth) then
            write(*,*)"nMonth (No. of Months) should be ",nMon0
            return
        end if
        
        do i = 1,iRow_begin-1
            read(101,*)  !!skip data
        end do
        dOBS(1:mCOL,1:nMonth) = -9999
        do i = 1,nMonth
            read(101,*,iostat=eof)ivoid,dOBS(1:mCOL,i)
            if(eof<0) exit
!            write(*,*)ivoid,dOBS(1:mCOL,i)
        end do
        close(101)
        return
        END
!!===========================================================
        SUBROUTINE Read_Daily_Obs
     &       (file_obs,iyr0,nyr_spinup,mobs,nday,dobs,mobs_read,dFill)
            IMPLICIT NONE
            character*100 file_obs
            INTEGER iyr0, nyr_spinup  !!year_begin for SWAT simulation,# of years for spinup
            integer mobs,nday,mobs_read !!# of COL & ROW in dobs, mobs_read is the # of COL to be read
            REAL dFill  !!-9999
            REAL, DIMENSION(mobs,nday)::dobs
            INTEGER nDaysofYear  !!FUNCTION
            
            INTEGER iFobs,iyr0_obs,j,k,eof
            character*20 svoid

            iFobs = 1001
            dobs(:,:) = dFill
            open(unit=iFobs,file=file_obs,status='unknown')
            read(iFobs,*)svoid   !!yyyymmdd, start date
            svoid = trim(svoid)
            svoid = svoid(1:4)
            read(svoid,*)iyr0_obs  !!the beginning year of the observations
            do j=iyr0_obs,(iyr0+nyr_spinup - 1)  !!skip the data for warmup
                do k =1,nDaysofYear(j)
                    read(iFobs,*)
                end do
            end do
            
            do j = 1, nday  !!nbyr: # of simulation years; ATTENTION: please append lines of ANY data to meet the total # of nbyr*366 as not every year has 366 days
                read(iFobs,*,iostat=eof)dobs(1:mobs_read,j)!!q: streamflow
                if(eof<0) exit
            end do
            close(iFobs)
            return
        END SUBROUTINE
!!===========================================================        
        SUBROUTINE Array_Normalize(n,xx,dFill)
        !!Normalize an Array to make the sum = 1.0
            IMPLICIT NONE
            INTEGER n
            REAL, DIMENSION(n)::xx
            REAL dFill
            INTEGER j
            REAL sum1
            REAL fSUM  !!function
            sum1 = fSUM(n,xx,dFill)
            do j = 1,n
                if(xx(j).ne.dFill) then
                    xx(j) = xx(j)/sum1
                else
                    xx(j) = 0
                end if
            end do
            return
        END 
!!===========================================================
        FUNCTION fWAVG(n,ww,xx,dFill)
        !!compute weighting average
            IMPLICIT NONE
            REAL fWAVG, dFill,sum1
            INTEGER n
            REAL, DIMENSION(n)::ww,xx
            INTEGER j,k
            INTEGER jj(n)
            REAL, DIMENSION(:), ALLOCATABLE::ww2
            k = 0
            do j = 1, n
                if(xx(j).ne.dFill) then  !!has a value different than dFill (e.g., -9999)
                    k = k + 1
                    jj(k) = j
                end if
            end do
            
            if(k<1) then
                fWAVG = dFill
            else
                ALLOCATE(ww2(k))
                do j=1,k
                    ww2(j) = ww(jj(j))
                end do
                call Array_Normalize(k,ww2,dFill)
                fWAVG = 0
                do j = 1,k
                    fWAVG = fWAVG + ww2(j)*xx(jj(j))
                end do
            end if
            return
        END FUNCTION
!!===========================================================
        SUBROUTINE WOBJ_INI(nobj,wobj,nobj0,wobj0)
            !!assign value to wobj based on wobj0
            IMPLICIT NONE
            INTEGER nobj,nobj0
            REAL,DIMENSION(nobj)::wobj
            REAL,DIMENSION(nobj0)::wobj0
            if(nobj.lt.1) then
                return
            elseif(nobj.le.nobj0) then
                wobj(1:nobj) = wobj0(1:nobj)
            else
                wobj(1:nobj0) = wobj0(1:nobj0)
                wobj(nobj0+1:nobj) = wobj0(nobj0)
            end if
            return
        END SUBROUTINE
!!===========================================================
        SUBROUTINE Write_filecio(dir_swatio,nbyr1,iyear1,iprint1)
            IMPLICIT NONE
            CHARACTER(len=*) dir_swatio
            INTEGER nbyr1,iyear1,iprint1
            INTEGER j,nline,eof,idata
            CHARACTER(len=200) dir_filecio
!            INTEGER,DIMENSION(:),ALLOCATABLE ::idata
            CHARACTER*150,DIMENSION(:),ALLOCATABLE ::sdata
            dir_filecio = trim(dir_swatio)//'file.cio'
            print*,">>>UPDATE Variables in: ",trim(dir_filecio),":"
            print*,"   NBYR(Line 8),IYR(Line 9),IPRINT(Line 59)"
            open(unit=101,file=dir_filecio,status='old')
            nline=0
            read(101,*,iostat=eof)
            do while(.not.(eof<0))
               nline = nline+1 
               read(101,*,iostat=eof)
            enddo
            close(101)
!            ALLOCATE(idata(nline))
            ALLOCATE(sdata(nline))
            
            open(unit=101,
     &       file=trim(dir_swatio)//'file.cio',status='old')
            do j=1,7
                read(101,'(a150)')sdata(j)
            end do
            read(101,'(I16,a150)')idata,sdata(8)
            read(101,'(I16,a150)')idata,sdata(9)
            do j=10,58
                read(101,'(a150)')sdata(j)
            end do
            read(101,'(I16,a150)')idata,sdata(59)
            do j=60,nline
                read(101,'(a150)')sdata(j)
            end do
            close(101)
            
            open(unit=101,
     &       file=trim(dir_swatio)//'file.cio')
            do j=1,7
                write(101,'(a150)')sdata(j)
            end do
            write(101,'(I16,a150)')nbyr1,sdata(8)
            write(101,'(I16,a150)')iyear1,sdata(9)
            do j=10,58
                write(101,'(a150)')sdata(j)
            end do
            write(101,'(I16,a150)')iprint1,sdata(59)
            do j=60,nline
                write(101,'(a150)')sdata(j)
            end do
            close(101)
            RETURN
        END SUBROUTINE
!!===========================================================
        SUBROUTINE DIR_ADD_SLASH(dir_inp,dir_out)
!            INTEGER, PARAMETER:: n = 100
            character(len=*) dir_inp,dir_out
            character(len=1) str1
            integer n
            
            dir_out = trim(dir_inp)
            dir_inp = ADJUSTR(dir_inp)
            n = LEN(dir_inp)
            str1 = dir_inp(n:n)
            if(str1.ne."/".and.str1.ne."\") then
                dir_inp = ADJUSTL(dir_inp)
                dir_out = trim(dir_inp)//"/"
            end if
            return      
        END SUBROUTINE
!!===========================================================
        SUBROUTINE SGLB_INI_VAR()
            USE parm
            sGLB%dir_swatio     = ""                             !!dir for swat in & out files
            sGLB%itype_opt      = 0                              !!type of data for calibration; 0-subbasin with flow,1-floodgate,2-Reservoir with volume, 3-subbasin with SWC !!original: iRESFLG
            sGLB%nsub_opt       = 0                              !!# of subbasins included in calibration !!noptsub,icodesub,icoderes,ndaysim,idaysim
            sGLB%icode_sub      = 0                              !!code of the subbasin with flow gage/reservoir, etc.
            sGLB%icode_res      = 0                              !!code of the reservoir in SWAT, ATTENTION, different from the code of corresponding subbasin
            sGLB%nyear_warmup   = 0                              !!# of years for SWAT warm-up
            sGLB%nday_sim       = 0                              !!# of days for simulation, particularly, for calibration, counting through SWAT simulations (should be= nday_sim0, but computed by different ways)
            sGLB%iday_sim       = 0                              !!the ith day in nday_sim
            sGLB%imon_sim       = 0                              !!the ith day in nday_sim
            sGLB%iyear0         = 0                              !!Beginning year of simulation
            sGLB%nyear_sim      = 0                              !!# of years simulated, excluding 'nyear_warmup'
            sGLB%nday_sim0      = 0                              !!# of days simulated, computed by iyear0,nyear_sim, nyear_warmup
            sGLB%nmon_sim       = 0                              !!# of months simulated,e xcluding warmup
            sGLB%iSWAT          = 0                              !!the ith SWAT simulation run
            sGLB%iModel         = 0                              !!0: run SWAT; >0: SCEUA optimization
            sGLB%iOBJ          = 0                               !!sGLB%iOBJ  = 0-NSE; 1-MARE; 2-NRMSE
            sGLB%wOBJ(:)        = 0                              !!wOBJ(1:3),weighting factors for 1st 3 objectives, wOBJ(>3) = wOBJ(3)
            return
        END SUBROUTINE
!!===========================================================
        SUBROUTINE SGLB_INI_ARRAY()
            USE parm
            sGLB%ivar_par(:) = 0 !!variation method: 1-absolute value; 2-relative value, multiplied by a factor
            sGLB%iopt_par(:) = 0 !!optimize or not: 1-optimize, 0-keep original value
            sGLB%isub_opt(:) = 0 !!subbasin IDs to be calibrated 
            sGLB%xua(:) = 0         !!parameter values 
            sGLB%subsurlag(:) = 4.0   !!surlag for each subbasin 
            sGLB%par_min(:) = 0    !!min and max of absolute parameter values
            sGLB%par_max(:) = 0     !!min and max of absolute parameter values
            sGLB%sub_area(:) = 0   !!subbasin area (km2)
            sGLB%iGateDam(:) = 0 !!0-SubBasin,1-FloodGate,2-Res/Dam
            sGLB%cwus(:) = 0        !!wateruse
            sGLB%crwus(:) = 0       !!wateruse coef
!            sGLB%data_sim(:,:) = 0 !!simulated time series, may include multiple variables, e.g., multiple subbasins within a HUC8 
!            sGLB%data_obs(:,:) = 0 !!observed time series for calibration
            return
        END SUBROUTINE
!!===========================================================
        
        SUBROUTINE SGLB_WRITE(sGLB1)
          USE parm
          TYPE(sSCE_GLOBAL) sGLB1
          PARAMETER(n = 5)
          write(*,*)"itype_opt = ",sGLB1%itype_opt                               !!type of data for calibration; 0-subbasin with flow,1-floodgate,2-Reservoir with volume, 3-subbasin with SWC !!original: iRESFLG
          write(*,*)"nsub_opt = ", sGLB1%nsub_opt                                !!# of subbasins included in calibration !!noptsub,icodesub,icoderes,ndaysim,idaysim
          write(*,*)"icode_sub = ", sGLB1%icode_sub                               !!code of the subbasin with flow gage/reservoir, etc.
          write(*,*)"icode_res = ", sGLB1%icode_res                               !!code of the reservoir in SWAT, ATTENTION, different from the code of corresponding subbasin
          write(*,*)"nyear_warmup  = ", sGLB1%nyear_warmup                            !!# of years for SWAT warm-up
          write(*,*)"nday_sim = ", sGLB1%nday_sim                                !!# of days for simulation, particularly, for calibration, counting through SWAT simulations (should be= nday_sim0, but computed by different ways)
          write(*,*)"iday_sim = ", sGLB1%iday_sim                                !!the ith day in nday_sim
          write(*,*)"imon_sim = ", sGLB1%imon_sim                                !!the ith day in nday_sim
          write(*,*)"iyear0 = ", sGLB1%iyear0                                 !!Beginning year of simulation
          write(*,*)"nyear_sim = ", sGLB1%nyear_sim                              !!# of years simulated, excluding 'nyear_warmup'
          write(*,*)"nday_sim0 = ", sGLB1%nday_sim0                              !!# of days simulated, computed by iyear0,nyear_sim, nyear_warmup
          write(*,*)"nmon_sim = ", sGLB1%nmon_sim                             !!# of months simulated,e xcluding warmup
          write(*,*)"iSWAT = ", sGLB1%iSWAT                                  !!the ith SWAT simulation run
          write(*,*)"iOBJ = ", sGLB1%iOBJ                                     !!reference OBJ, e.g., observed runoff coefficient (RC)
          write(*,*)"wOBJ = ",sGLB1%wOBJ
          write(*,*)"ivar_par = ", sGLB1%ivar_par !!variation method: 1-absolute value; 2-relative value, multiplied by a factor
          write(*,*)"iopt_par = ", sGLB1%iopt_par !!optimize or not: 1-optimize, 0-keep original value
          write(*,*)"isub_opt = ", sGLB1%isub_opt !!subbasin IDs to be calibrated 
          write(*,*)"iGateDam = ", sGLB1%iGateDam(1:n) !!0-SubBasin,1-FloodGate,2-Res/Dam
          write(*,*)"xua = ", sGLB1%xua         !!parameter values 
          write(*,*)"subsurlag = ", sGLB1%subsurlag(1:n)   !!surlag for each subbasin 
          write(*,*)"cwus = ", sGLB1%cwus(1:n)        !!wateruse
          write(*,*)"crwus = ", sGLB1%crwus(1:n)       !!wateruse coef
          write(*,*)"par_min = ", sGLB1%par_min     !!min and max of absolute parameter values
          write(*,*)"par_max = ", sGLB1%par_max     !!min and max of absolute parameter values
          write(*,*)"sub_area = ", sGLB1%sub_area(1:n)   !!subbasin area (km2)
          write(*,*)"data_sim = ", sGLB1%data_sim(1,1:n) !!simulated time series, may include multiple variables, e.g., multiple subbasins within a HUC8 
          write(*,*)"data_obs = ", sGLB1%data_obs(1,1:n) !!observed time series for calibration
        END
!!===========================================================
!        Subroutine Read_Sub_Area
!     &        (datafile,sub_area)
!        IMPLICIT NONE
!        character*100 datafile
!        INTEGER nSub,i,itemp
!        REAL,DIMENSION(:), ALLOCATABLE :: sub_area
!        open(unit=101,file = datafile,status='old')
!        read(101,*)  !!"No. of Subbasins"
!        read(101,*)nSub
!        ALLOCATE(sub_area(nSub))
!        read(101,*)  !!head
!        do i = 1, nSub
!            read(101,*)itemp, sub_area(i)
!        end do
!        close(101)
!        RETURN
!        END
!=======================================================================
!      real function fHMLE(ndata, qobs,q)
!!!Heteroscedastic Maximum Likelihood Error
!!      implicit real*8 (a-h,o-z)
!!!      common /fnblk/ rlamda, ad
!!!      common /block1/ ndata, ns, iobj
!!!      common /block2/ p(1000), qobs(1000), s0(2)
!!!      common /block3/ q(1000), r(1000), b(1000), s(1000)
!      IMPLICIT NONE
!      real rlamda, ad, a, ex, rd,rn,w
!      integer ndata,i,ict,isign,lcount
!      real q(ndata), qobs(ndata)
!      REAL ra(2)
!      REAL, PARAMETER::eps=5.d-02, del=5.d-02
!      INTEGER iflag
!      DATA iflag /0/
!
!!c  COMPUTE THE MEAN OF LOGARITHM OF OBSERVED FLOWS
!      if (iflag .eq. 0) then
!        ad = 0.d0
!        do 10 i = 1, ndata
!          ad = ad + dlog(dble(qobs(i)))
!   10   continue
!        ad = ad / dble(ndata)
!        rlamda = 1.d0
!        iflag = 1
!      end if
!
!!c  ESTIMATE THE LAMDA VALUE
!      lcount = 0
!      ict = 1
!      ra(1) = 0.d0
!      ra(2) = 0.d0
!   25 continue
!      lcount = lcount + 1
!      if(lcount .gt. 40) then
!        write(*,*) 'LAMDA ITERATION GO OVER 40', rlamda, ra(1), ra(2)
!        go to 50
!      end if
!      rd = 0.d0
!      rn = 0.d0
!      do 30 i = 1, ndata
!        a = dlog(dble(qobs(i))) / ad
!        w = qobs(i)**(2*(rlamda-1.d0))
!        rd = rd + w*(qobs(i) - q(i))**2
!        rn = rn + w*(qobs(i) - q(i))**2 * a
!   30 continue
!      ra(ict) = rn / rd - 1.d0
!      if (dabs(dble(ra(ict))) .le. eps) go to 50
!      isign = -1
!      if (ra(ict) .lt. 0.d0) isign = 1
!      rlamda = rlamda + isign * del
!      if (ict .eq. 2) go to 35
!      ict = 2
!      go to 25
!
!   35 continue
!      if (ra(1)*ra(2) .lt. 0.d0) go to 40
!      ra(1) = ra(2)
!      go to 25
!
!   40 continue
!      rlamda = rlamda - isign * del / 2.d0
!
!!c  COMPUTE HMLE
!   50 continue
!      fHMLE = 0.d0
!      ex = 2. * (rlamda - 1.)
!      do 60 i = 1, ndata
!        fHMLE = fHMLE + qobs(i)**ex * (qobs(i) -q(i))**2
!   60 continue
!      fHMLE = fHMLE / dble(ndata)
!      fHMLE = fHMLE / dexp(dble(ex * a))
!
!	if(fHMLE.gt.1.0E20) fHMLE = 1.0E20
!      return
!      end
!
!!!=======================================================================
!      real function fHMLE1(ndata, qobs,q)
!!      implicit real*8 (a-h,o-z)
!!!      common /fnblk/ rlamda, ad
!!!      common /block1/ ndata, ns, iobj
!!!      common /block2/ p(1000), qobs(1000), s0(2)
!!!      common /block3/ q(1000), r(1000), b(1000), s(1000)
!      IMPLICIT NONE
!      real rlamda, ad, ex, a
!      integer ndata,i
!      real q(ndata), qobs(ndata)
!      real ra(2)
!      REAL, PARAMETER:: eps=5.d-02, del=5.d-02
!      INTEGER iflag
!      data iflag /0/
!
!!c  COMPUTE THE MEAN OF LOGARITHM OF OBSERVED FLOWS
!      if (iflag .eq. 0) then
!        ad = 0.d0
!        do 10 i = 1, ndata
!          ad = ad + dlog(dble(qobs(i)))
!   10   continue
!        ad = ad / dble(ndata)
!        rlamda = 0
!!!        iflag = 1
!      end if
!
!!c  COMPUTE HMLE
!   50 continue
!      fHMLE1 = 0.d0
!      ex = 2. * (rlamda - 1.)
!      do 60 i = 1, ndata
!        fHMLE1 = fHMLE1 + qobs(i)**ex * (qobs(i) -q(i))**2
!   60 continue
!      fHMLE1 = fHMLE1 / dble(ndata)
!      fHMLE1 = fHMLE1 / dexp(dble(ex * a))  !!PROBLEM: how to compute 'a'???
!
!	if(fHMLE1.gt.1.0E20) fHMLE1 = 1.0E20
!      return
!      end

!!========================================================
