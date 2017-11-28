      subroutine sceua(sSCE)
      USE MPI
      USE parm
      USE scemod
!$debug
!c
!c
!c  SHUFFLED COMPLEX EVOLUTION METHOD FOR GLOBAL OPTIMIZATION
!c     -- Version 2.1
!c
!c  by QINGYUN DUAN
!c  DEPARTMENT OF HYDROLOGY & WATER RESOURCES
!c  UNIVERSITY OF ARIZONA, TUCSON, AZ 85721
!c  (602) 621-9360, email: duan@hwr.arizona.edu
!c
!c  WRITTEN IN OCTOBER 1990.
!c  REVISED IN AUGUST 1991
!c  REVISED IN APRIL 1992
      
!   MODIFIED BY GANGSHENG WANG - ORNL, MARCH 2015
      
!c  STATEMENT BY AUTHOR:
!c  --------------------
!c
!c     This general purpose global optimization program is developed at
!c     the Department of Hydrology & Water Resources of the University
!c     of Arizona.  Further information regarding the SCE-UA method can
!c     be obtained from Dr. Q. Duan, Dr. S. Sorooshian or Dr. V.K. Gupta
!c     at the address and phone number listed above.  We request all
!c     users of this program make proper reference to the paper entitled
!c     'Effective and Efficient Global Optimization for Conceptual
!c     Rainfall-runoff Models' by Duan, Q., S. Sorooshian, and V.K. Gupta,
!c     Water Resources Research, Vol 28(4), pp.1015-1031, 1992.
!c
!c
!c  LIST OF INPUT ARGUEMENT VARIABLES
!c
!c     a(.) = initial parameter set
!c     bl(.) = lower bound on parameters
!c     bu(.) = upper bound on parameters
!c     nopt = number of parameters to be optimized
!c
!c
!c  LIST OF SCE ALGORITHMIC CONTROL PARAMETERS:
!c
!c     ngs = number of complexes in the initial population
!c     npg = number of points in each complex
!c     npt = total number of points in initial population (npt=ngs*npg)
!c     nps = number of points in a sub-complex
!c     nspl = number of evolution steps allowed for each complex before
!c         complex shuffling
!c     mings = minimum number of complexes required, if the number of
!c         complexes is allowed to reduce as the optimization proceeds
!c     iseed = initial random seed
!c     iniflg = flag on whether to include the initial point in population
!c         = 0, not included
!c         = 1, included
!c     iprint1 = flag for controlling print-out after each shuffling loop
!c         = 0, print information on the best point of the population
!c         = 1, print information on every point of the population
!c
!c
!c  CONVERGENCE CHECK PARAMETERS
!c
!c     maxn = max no. of trials allowed before optimization is terminated
!c     kstop = number of shuffling loops in which the criterion value must
!c         chang by the given percentage before optimization is terminated
!c     pcento = percentage by which the criterion value must change in
!c         given number of shuffling loops
!c     ipcnvg = flag indicating whether parameter convergence is reached
!c         (i.e., check if gnrng is less than 0.001)
!c         = 0, parameter convergence not satisfied
!c         = 1, parameter convergence satisfied
!c
!c
!c  LIST OF LOCAL VARIABLES
!c     x(.,.) = coordinates of points in the population
!c     xf(.) = function values of x(.,.)
!c     xx(.) = coordinates of a single point in x
!c     cx(.,.) = coordinates of points in a complex
!c     cf2(.) = function values of cx(.,.)
!c     s(.,.) = coordinates of points in the current simplex
!c     sf(.) = function values of s(.,.)
!c     bestx(.) = best point at current shuffling loop
!c     bestf = function value of bestx(.)
!c     worstx(.) = worst point at current shuffling loop
!c     worstf = function value of worstx(.)
!c     xnstd(.) = standard deviation of parameters in the population
!c     gnrng = normalized geometric mean of parameter ranges
!c     lcs(.) = indices locating position of s(.,.) in x(.,.)
!c     bound(.) = bound on ith variable being optimized
!c     ngs1 = number of complexes in current population
!c     ngs2 = number of complexes in last population
!c     iseed1 = current random seed
!c     criter(.) = vector containing the best criterion values of the last
!c         10 shuffling loops
!c
!!wgs      implicit real*8 (a-h,o-z)
!c
!c  ARRAYS FROM THE INPUT DATA
      implicit none
      
      integer ierr, np, pid, mpi_seed
      TYPE(sSCE_PAR) sSCE
      
      real parx_val(sSCE%npar),parx_min(sSCE%npar),parx_max(sSCE%npar)
      real iOpt(sSCE%nopt)
      integer npar, nopt,maxn, kstop ! iseed,iseed1
      integer ngs,npg,nps,nspl,mings,iniflg,ifprint
      integer k,l,k1, k2,j,isce,icall,ipcnvg,igs, jua
      integer loop,nloop, lpos,ngs1,ngs2,npt,npt1 !,nopt1,nopt2
      real pcento, bestf,worstf,denomi,fa,timeou,gnrng
!c  LOCAL ARRAYS
      real x(2000,sSCE%npar),xf(2000)
      real xx(sSCE%npar),bestx(sSCE%npar),worstx(sSCE%npar)
      real s(50,sSCE%npar),sf(50),cx(2000,sSCE%npar),cf2(2000)
      real xnstd(sSCE%npar),bound(sSCE%npar),criter(20),unit(sSCE%npar)
      integer lcs(sSCE%nps)  !should be integer, but sort1(nps, lcs) declares "lcs" as real; lcs(50)
      real randx
      real functn  !objective function
      integer ipr !!output file
!      common /iopar/ in,ipr

      character*10 xname(sSCE%npar)
      character*256 format510,format520,format521
      character*256 format610,format630,format660
!      data xname /'CN2','ESCO','EPCO','aBF','CHN2','CHK2',
!     &	'sLAG','cWUS',
!     &'  X9',' X10',' X11',' X12',' X13',' X14',' X15',' X16'/
      
!     parx_val,parx_min,parx_max,nopt,maxn,kstop,pcento,iseed
!     &                 ,ngs,npg,nps,nspl,mings,iniflg,iprint1
!!ASSIGN VARIABLE VALUES FROM STRUCT_OPT
!        CALL SCEPAR_WRITE(sSCE)
      npar = sSCE%npar
      nopt = sSCE%nopt
      maxn = sSCE%maxn
      kstop = sSCE%kstop
      pcento = sSCE%pcento
      ngs = sSCE%ngs
      npg = sSCE%npg
      npt = sSCE%npt
      nps = sSCE%nps
      nspl = sSCE%nspl
      mings  = sSCE%mings
      iniflg  = sSCE%iniflg
      ifprint = sSCE%ifprint
      ipr = sSCE%iFO1 
      
      parx_val = sSCE%parINI
      parx_min = sSCE%parMIN
      parx_max = sSCE%parMAX
      xname = sSCE%parName
      iOpt = sSCE%iOpt
      
      write (*,*) ' ENTER THE SCEUA SUBROUTINE --- '     
!!WGS_BEGIN
      write(format510,*)"(/,' CRITERION',",npar,"(a10),/1x,",  !(6x,a4)
     &                   (npar+1)*10,"(1h-))"
      write(format520,*)"(f10.4,",npar,"f10.4)"
      write(format610,*)"(/,1x,'LOOP',1x,'TRIALS',1x,'COMPLXS',2x,",
     &       "'BESTF',3x,'WORSTF',3x,'PAR-RNG',1x,",npar,"(a10))"
      write(format630,*)"(i5,1x,i5,3x,i5,3g10.3,",npar,"(f10.4))"
      write(format660,*)"(15x,g10.3,20x,",npar,"(e10.3))"
!!WGS_END      

!c  INITIALIZE VARIABLES
      nloop = 0
      loop = 0
      igs = 0
!      nopt1 = 12 !!nopt1=8
!      if (nopt.lt.nopt1) nopt1 = nopt
!      nopt2 = 12
!      if (nopt.lt.12) nopt2 = nopt

!c  INITIALIZE RANDOM SEED TO A NEGATIVE INTEGER
!      iseed1 = iseed!-abs(iseed)

!c  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPUALTION
      npt = ngs * npg
      ngs1 = ngs
      npt1 = npt

      write(ipr,400)
      write (*,*) ' ***  Evolution Loop Number ',nloop

!c  COMPUTE THE BOUND FOR PARAMETERS BEING OPTIMIZED
      do j = 1, npar
        bound(j) = parx_max(j) - parx_min(j)
        unit(j) = 1.0
        xx(j) = sSCE%parINI(j)
      end do

!c  COMPUTE THE FUNCTION VALUE OF THE INITIAL POINT
	fa = functn(npar,parx_val)

!c  PRINT THE INITIAL POINT AND ITS CRITERION VALUE
      write(ipr,500)
      write(ipr,format510) xname!(xname(j),j=1,nopt2)  !!510
      write(ipr,format520) fa,parx_val!(parx_val(j),j=1,nopt2)
!      if (nopt.lt.13) go to 101
!      write(ipr,530) (xname(j),j=13,nopt)
!      write(ipr,540) (parx_val(j),j=13,nopt)
!  101 continue

!c  GENERATE AN INITIAL SET OF npt1 POINTS IN THE PARAMETER SPACE
!c  IF iniflg IS EQUAL TO 1, SET x(1,.) TO INITIAL POINT a(.)
!!      if (iniflg .eq. 1) then
!!        do j = 1, npar
!!          x(1,j) = parx_val(j)
!!        end do
!!        xf(1) = fa
       
!c  ELSE, GENERATE A POINT RANDOMLY AND SET IT EQUAL TO x(1,.)
!!      else
!!        call getpnt(sSCE,1,xx,unit,parx_min)
!        call getpnt(nopt,1,iseed1,xx,parx_min,parx_max,unit,parx_min)
!!        do j=1, npar
!!          x(1,j) = xx(j)
!!        end do
!!        xf(1) = functn(npar,xx)
!!      end if
!!      icall = 1
!!      if (icall .ge. maxn) go to 9000

!c  GENERATE npt1-1 RANDOM POINTS DISTRIBUTED UNIFORMLY IN THE PARAMETER
!c  SPACE, AND COMPUTE THE CORRESPONDING FUNCTION VALUES
!!----------------------------------------------------
!! wgs: 11/24/2017: beg
      icall = 0
      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, np, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, pid, ierr)
     
      mpi_seed = 123456789 + pid*100
      call srand(mpi_seed) 
      do isce = pid+1, npt1, np
        call getpnt(sSCE,1,xx,unit,parx_min)
        if (iniflg .eq. 1) then
          do j = 1, npar
            xx(j) = parx_val(j)
          end do
        end if

        do j = 1, npar
          x(isce,j) = xx(j)
        end do
!        write(*,*)xx
        xf(isce) = functn(npar,xx)
        
!        write(*,*)isce,x(isce,1:npar),"<>",xf(isce)
        
!!        if(pid == 0) then
           write(*,*)'PARALLEL TEST: npt1/np/pid/isce/mpi_seed=',
     &                npt1,np,pid,isce,mpi_seed     
!!        end if
      end do
      call MPI_FINALIZE(ierr)
!! wgs: 11/24/2017: end
!!-----------------------------------------------------
      icall = icall + npt1
      if (icall.ge.maxn) then
          npt1 = npt1 - (icall - maxn)
      end if


!c  ARRANGE THE POINTS IN ORDER OF INCREASING FUNCTION VALUE
   45 call sort(npt1,npar,x,xf)

!c  RECORD THE BEST AND WORST POINTS
      do j = 1, npar
        bestx(j) = x(1,j)
        worstx(j) = x(npt1,j)
      end do
      bestf = xf(1)
      worstf = xf(npt1)

!c  COMPUTE THE PARAMETER RANGE FOR THE INITIAL POPULATION
      call parstt(sSCE,npt1,x,xnstd,bound,gnrng,ipcnvg)

!c  PRINT THE RESULTS FOR THE INITIAL POPULATION
      write(ipr,600)
      write(ipr,format610) xname(1:npar)!(xname(j),j=1,nopt1)
!      if (nopt .le. nopt1) go to 201
!      write(ipr,620) (xname(j),j=nopt1+1,nopt)
!  201 continue
      write(ipr,format630) nloop,icall,ngs1,bestf,worstf,gnrng,
     &               bestx(1:npar)!(bestx(j),j=1,nopt1)
!      if (nopt .le. nopt1) go to 301
!      write(ipr,640) (bestx(j),j=nopt1+1,nopt)
!  301 continue
      if (ifprint .eq. 1) then
        write(ipr,650) nloop
        do isce = 1, npt1
          write(ipr,format660) xf(isce),x(isce,1:npar)!(x(isce,j),j=1,nopt1)
!          if (nopt .le. nopt1) go to 401
!          write(ipr,640) (x(isce,j),j=nopt1+1,nopt)
  401   end do
      end if

      if (icall .ge. maxn) go to 9000
      if (ipcnvg .eq. 1) go to 9200

!c  BEGIN THE MAIN LOOP ----------------
 1000 continue
      nloop = nloop + 1

      write (*,*) ' ***  Evolution Loop Number ',nloop

!c  BEGIN LOOP ON COMPLEXES
      do 3000 igs = 1, ngs1

!c  ASSIGN POINTS INTO COMPLEXES
      do k1 = 1, npg
        k2 = (k1-1) * ngs1 + igs
        do j = 1, npar
          cx(k1,j) = x(k2,j)
        end do
        cf2(k1) = xf(k2)
      end do

!        write(*,*)"ALL:"
!        do isce = 1, npt1
!            write(*,*)isce,x(isce,1:nopt),"<>",xf(isce)
!        end do
!c  BEGIN INNER LOOP - RANDOM SELECTION OF SUB-COMPLEXES ---------------
      do 2000 loop = 1, nspl

!c  CHOOSE A SUB-COMPLEX (nps points) ACCORDING TO A LINEAR
!c  PROBABILITY DISTRIBUTION
      call selectINT(npg,nps,lcs) 
      call sort1_int(nps,lcs)
!      write(*,*)"lcs = ",lcs
!c  CREATE THE SUB-COMPLEX ARRAYS
   85 do k = 1, nps
        do j = 1, npar
          s(k,j) = cx(lcs(k),j)
        end do
        sf(k) = cf2(lcs(k))
      end do
!!WGS_BEGIN
!        write(*,*)"SUB-COMPLEX, BEFORE CCE:",npg
!        do k = 1, npg
!            write(*,*)k,cx(k,1:nopt),"<>",cf2(k)
!        end do
!        write(*,*)"SUB-COMPLEX, BEFORE CCE:"
!        do k = 1, nps
!            write(*,*)k,s(k,1:nopt),"<>",sf(k)
!        end do
 !!WGS_END
!c  USE THE SUB-COMPLEX TO GENERATE NEW POINT(S)
      call cce(sSCE,s,sf,xnstd,icall)
!      call cce(nopt,nps,s,sf,parx_min,parx_max,xnstd,icall,maxn,iseed1)

!c  IF THE SUB-COMPLEX IS ACCEPTED, REPLACE THE NEW SUB-COMPLEX
!c  INTO THE COMPLEX
      do k = 1, nps
        do j = 1, npar
          cx(lcs(k),j) = s(k,j)
        end do
        cf2(lcs(k)) = sf(k)
      end do
!!WGS_BEGIN
!        write(*,*)"SUB-COMPLEX, AFTER CCE:"
!        do k = 1, nps
!            write(*,*)k,s(k,1:nopt),"<>",sf(k)
!        end do
!        write(*,*)"SUB-COMPLEX, AFTER CCE:",npg
!        do k = 1, npg
!            write(*,*)k,cx(k,1:nopt),"<>",cf2(k)
!        end do

 !!WGS_END
!c  RE-SORT THE POINTS
!c  SORT THE POINTS
      call sort(npg,npar,cx,cf2)
!        write(*,*)"SUB-COMPLEX, AFTER CCE & SORT:",npg
!        do k = 1, npg
!            write(*,*)k,cx(k,1:nopt),"<>",cf2(k)
!        end do
!c  IF MAXIMUM NUMBER OF RUNS EXCEEDED, BREAK OUT OF THE LOOP
      if (icall .ge. maxn) go to 2222

!c  END OF INNER LOOP ------------
 2000 continue !!do 2000 loop = 1, nspl
 2222 continue

!c  REPLACE THE NEW COMPLEX INTO ORIGINAL ARRAY x(.,.)
      do k1 = 1, npg
        k2 = (k1-1) * ngs1 + igs
        do j = 1, npar
          x(k2,j) = cx(k1,j)
        end do
        xf(k2) = cf2(k1)
      end do
      if (icall .ge. maxn) go to 3333

!c  END LOOP ON COMPLEXES
 3000 continue  !!do 3000 igs = 1, ngs1

!c  RE-SORT THE POINTS
 3333 call sort(npt1,npar,x,xf)

        
!c  RECORD THE BEST AND WORST POINTS
      do j = 1, npar
        bestx(j) = x(1,j)
        worstx(j) = x(npt1,j)
      end do
      bestf = xf(1)
      worstf = xf(npt1)

!c  TEST THE POPULATION FOR PARAMETER CONVERGENCE
      call parstt(sSCE,npt1,x,xnstd,bound,gnrng,ipcnvg)

!c  PRINT THE RESULTS FOR CURRENT POPULATION
      if (mod(nloop,5) .ne. 0) go to 501
      write(ipr,format610) xname(1:npar)!(xname(j),j=1,nopt1)
!      if (nopt .le. nopt1) go to 501
!      write(ipr,620) (xname(j),j=nopt1+1,nopt)
  501 continue
      write(ipr,format630) nloop,icall,ngs1,bestf,worstf,gnrng,bestx

!      if (nopt.le.nopt1) go to 601
!      write(ipr,640) (bestx(j),j=nopt1+1,nopt)
!  601 continue
      if (ifprint .eq. 1) then
        write(ipr,650) nloop
        do isce = 1, npt1
          write(ipr,format660) xf(isce),x(isce,1:npar)!(x(isce,j),j=1,nopt1)
!          if (nopt .le.nopt1) go to 701
!          write(ipr,640) (x(isce,j),j=nopt1+1,nopt)
  701   end do
      end if

!c  TEST IF MAXIMUM NUMBER OF FUNCTION EVALUATIONS EXCEEDED
      if (icall .ge. maxn) go to 9000

!c  COMPUTE THE COUNT ON SUCCESSIVE LOOPS W/O FUNCTION IMPROVEMENT
      criter(20) = bestf
      if (nloop .lt. (kstop+1)) go to 132
      denomi = abs(criter(20-kstop) + criter(20)) / 2.
      timeou = abs(criter(20-kstop) - criter(20)) / denomi
      if (timeou .lt. pcento) go to 9100
  132 continue
      do l = 1, 19
        criter(l) = criter(l+1)
      end do

!c  IF POPULATION IS CONVERGED INTO A SUFFICIENTLY SMALL SPACE
      if (ipcnvg .eq. 1) go to 9200

!c  NONE OF THE STOPPING CRITERIA IS SATISFIED, CONTINUE SEARCH

!c  CHECK FOR COMPLEX NUMBER REDUCTION
      if (ngs1 .gt .mings) then
        ngs2 = ngs1
        ngs1 = ngs1 - 1
        npt1 = ngs1 * npg
        call comp(npar,npt1,ngs1,ngs2,npg,x,xf,cx,cf)
      end if

!c  END OF MAIN LOOP -----------
      go to 1000

!c  SEARCH TERMINATED
 9000 continue
      write(ipr,800) maxn,loop,igs,nloop
      go to 9999
 9100 continue
      write(ipr,810) pcento*100.,kstop
      go to 9999
 9200 write(ipr,820) gnrng*100.
 9999 continue

!c  PRINT THE FINAL PARAMETER ESTIMATE AND ITS FUNCTION VALUE
      sSCE%objOPT = bestf !best_OBF = bestf
      sSCE%parOPT = bestx !best_PAR = bestx(1:nopt)
!      write(*,*)best_OBF, "<>",best_PAR
      write(ipr,830)
      write(ipr,format510) xname!(xname(j),j=1,nopt2)
      write(ipr,format520) bestf,bestx!(bestx(j),j=1,nopt2)
      write(sSCE%iFO2,format520) bestf,bestx
!      if (nopt.lt.13) go to 801
!      write(ipr,530) (xname(j),j=13,nopt)
!      write(ipr,540) (bestx(j),j=13,nopt)
!  801 continue

!c  END OF SUBROUTINE SCEUA
      return
  400 format(//,2x,50(1h=),/,2x,'ENTER THE SHUFFLED COMPLEX EVOLUTION',
     &       ' GLOBAL SEARCH',/,2x,50(1h=))
  500 format(//,'*** PRINT THE INITIAL POINT AND ITS CRITERION ',
     &       'VALUE ***')
  510 format(/,' CRITERION',12(6x,a4),/1x,60(1h-))
  520 format(g10.3,12f10.3)
  530 format(10x,12(6x,a4))
  540 format(10x,12f10.3)
  600 format(//,1x,'*** PRINT THE RESULTS OF THE SCE SEARCH ***')
  610 format(/,1x,'LOOP',1x,'TRIALS',1x,'COMPLXS',2x,'BEST F',3x,
     &       'WORST F',3x,'PAR RNG',1x,12(6x,a4))			   !!8(6x, a4)
  620 format(49x,12(6x,a4))
  630 format(i5,1x,i5,3x,i5,3g10.3,12(f10.3))
  640 format(49x,12(f10.3))
  650 format(/,1x,'POPULATION AT LOOP ',i3,/,1x,22(1h-))
  660 format(15x,g10.3,20x,12(f10.3))
  800 format(//,1x,'*** OPTIMIZATION SEARCH TERMINATED BECAUSE THE',
     &       ' LIMIT ON THE MAXIMUM',/,5x,'NUMBER OF TRIALS ',i5,
     &       ' EXCEEDED.  SEARCH WAS STOPPED AT',/,5x,'SUB-COMPLEX ',
     &       i3,' OF COMPLEX ',i3,' IN SHUFFLING LOOP ',i3,' ***')
  810 format(//,1x,'*** OPTIMIZATION TERMINATED BECAUSE THE CRITERION',
     &       ' VALUE HAS NOT CHANGED ',/,5x,f5.2,' PERCENT IN',i3,
     &       ' SHUFFLING LOOPS ***')
  820 format(//,1x,'*** OPTIMIZATION TERMINATED BECAUSE THE POPULATION',
     &       ' HAS CONVERGED INTO ',/,4x,f5.2,' PERCENT OF THE',
     &       ' FEASIBLE SPACE ***')
  830 format(//,'*** PRINT THE FINAL PARAMETER ESTIMATE AND ITS',
     &       ' CRITERION VALUE ***')
      end  !SCEUA

      
!c====================================================================
      subroutine cce(sSCE,s,sf,xnstd,icall)
!$debug

!c  ALGORITHM GENERATE A NEW POINT(S) FROM A SUB-COMPLEX

!c  SUB-COMPLEX VARIABLES
      !!wgs    implicit real*8 (a-h,o-z)
      USE scemod
      IMPLICIT NONE
      TYPE(sSCE_PAR) sSCE
      integer npar,n,m,maxn,icall,j,k,isce,ibound
!      parameter (c1=0.8,c2=0.4)
!      real s(50,16),sf(50),parx_max(16),parx_min(16),xnstd(16)
      real s(50,sSCE%npar),sf(50),iOpt(sSCE%nopt)
      real parx_max(sSCE%npar),parx_min(sSCE%npar),xnstd(sSCE%npar)
      real alpha,beta,fw,fnew
      real functn

!c  LIST OF LOCAL VARIABLES
!c    sb(.) = the best point of the simplex
!c    sw(.) = the worst point of the simplex
!c    w2(.) = the second worst point of the simplex
!c    fw = function value of the worst point
!c    ce(.) = the centroid of the simplex excluding wo
!c    snew(.) = new point generated from the simplex
!c    iviol = flag indicating if constraints are violated
!c          = 1 , yes
!c          = 0 , no

      real sw(sSCE%npar),sb(sSCE%npar),ce(sSCE%npar),snew(sSCE%npar)

!c  EQUIVALENCE OF VARIABLES FOR READABILTY OF CODE
      n = sSCE%nps
      m = sSCE%nopt
      npar = sSCE%npar
      iOpt = sSCE%iOpt
      maxn = sSCE%maxn
      parx_min = sSCE%parMIN
      parx_max = sSCE%parMAX
      
      alpha = 1.0
      beta = 0.5
      
      do k = 1, npar
          snew(k) = s(1, k)
          sb(k) = s(1, k)
      end do
!c  IDENTIFY THE WORST POINT wo OF THE SUB-COMPLEX s
!c  COMPUTE THE CENTROID ce OF THE REMAINING POINTS
!c  COMPUTE step, THE VECTOR BETWEEN wo AND ce
!c  IDENTIFY THE WORST FUNCTION VALUE fw
      do k = 1, m
        j = iOpt(k)
        sb(j) = s(1,j)
        sw(j) = s(n,j)
        ce(j) = 0.0
        do isce = 1, n-1
          ce(j) = ce(j) + s(isce,j)
        end do
        ce(j) = ce(j)/dble(n-1)  !!mean value of the 1st (n-1) values
      end do
      fw = sf(n) !!worst function value

!c  COMPUTE THE NEW POINT snew

!c  FIRST TRY A REFLECTION STEP
      do k = 1, m  !!for each parameter
        j = iOpt(k)
        snew(j) = ce(j) + alpha * (ce(j) - sw(j))
      end do

!c  CHECK IF snew SATISFIES ALL CONSTRAINTS
      call chkcst(npar,snew,parx_min,parx_max,ibound)
!      call chkcst(nopt,nopt,0,snew,parx_min,parx_max,ibound)


!c  snew IS OUTSIDE THE BOUND,
!c  CHOOSE A POINT AT RANDOM WITHIN FEASIBLE REGION ACCORDING TO
!c  A NORMAL DISTRIBUTION WITH BEST POINT OF THE SUB-COMPLEX
!c  AS MEAN AND STANDARD DEVIATION OF THE POPULATION AS STD
      if (ibound .ge. 1) then
	call getpnt(sSCE,2,snew,xnstd,sb)
!        call getpnt(nopt,2,iseed,snew,parx_min,parx_max,xnstd,sb)
      end if


!c  COMPUTE THE FUNCTION VALUE AT snew
      fnew = functn(npar,snew)
      icall = icall + 1

!c  COMPARE fnew WITH THE WORST FUNCTION VALUE fw
!c
!c  fnew IS LESS THAN fw, ACCEPT THE NEW POINT snew AND RETURN
      if (fnew .le. fw) go to 2000
      if (icall .ge. maxn) go to 3000


!c  fnew IS GREATER THAN fw, SO TRY A CONTRACTION STEP
      do k = 1, m
        j = iOpt(k)
        snew(j) = ce(j) - beta * (ce(j) - sw(j))
      end do

!c  COMPUTE THE FUNCTION VALUE OF THE CONTRACTED POINT
      fnew = functn(npar,snew)
      icall = icall + 1

!c  COMPARE fnew TO THE WORST VALUE fw
!c  IF fnew IS LESS THAN OR EQUAL TO fw, THEN ACCEPT THE POINT AND RETURN
      if (fnew .le. fw) go to 2000
      if (icall .ge. maxn) go to 3000


!c  IF BOTH REFLECTION AND CONTRACTION FAIL, CHOOSE ANOTHER POINT
!c  ACCORDING TO A NORMAL DISTRIBUTION WITH BEST POINT OF THE SUB-COMPLEX
!c  AS MEAN AND STANDARD DEVIATION OF THE POPULATION AS STD
 1000 call getpnt(sSCE,2,snew,xnstd,sb)
! 1000 call getpnt(npar,nopt,iOpt,2,snew,parx_min,parx_max,xnstd,sb)
! 1000 call getpnt(nopt,2,iseed,snew,parx_min,parx_max,xnstd,sb)

!c  COMPUTE THE FUNCTION VALUE AT THE RANDOM POINT
      fnew = functn(npar,snew)
      icall = icall + 1


!c  REPLACE THE WORST POINT BY THE NEW POINT
 2000 continue
      do j = 1, m
        s(n,j) = snew(j)
      end do
      sf(n) = fnew
 3000 continue

!c  END OF SUBROUTINE CCE
      return
      end



!c===================================================================
      subroutine getpnt(sSCE,idist,x,std,xi)
!          subroutine getpnt(npar,nopt,iOpt,idist,x,parx_min,parx_max,std,xi)
!      subroutine getpnt(nopt,idist,iseed,x,parx_min,parx_max,std,xi)
!$debug      
!c
!c     This subroutine generates a new point within feasible region
!c
!c     x(.) = new point
!c     xi(.) = focal point
!c     bl(.) = lower bound
!c     bu(.) = upper bound
!c     std(.) = standard deviation of probability distribution
!c     idist = probability flag
!c           = 1 - uniform distribution
!c           = 2 - Gaussian distribution
!c
      !!wgs    implicit real*8 (a-h,o-z)
          
        USE scemod
        IMPLICIT NONE
        TYPE(sSCE_PAR) sSCE
        integer npar,nopt,k,j,idist,ibound
        integer iOpt(sSCE%nopt)
        real randx
!      real x(16),parx_min(16),parx_max(16),std(16),xi(16)
        real x(sSCE%npar),std(sSCE%npar),xi(sSCE%npar)
        real parx_min(sSCE%npar),parx_max(sSCE%npar)
        real gasdev !function

        npar = sSCE%npar
        nopt = sSCE%nopt
        iOpt = sSCE%iOpt
        parx_min = sSCE%parMIN
        parx_max = sSCE%parMAX
      
!        write(*,*)nopt
!        write(*,*)iOpt
!        do k = 1,npar
!            write(*,*)k,parx_min(k),parx_max(k),x(k),std(k),xi(k)
!        end do

    1   do k=1, nopt
            j = iOpt(k)
    2       if (idist .eq. 1) randx = rand() !rand = ran1(iseed)
            if (idist .eq. 2) randx = gasdev() !gasdev(iseed)
            x(j) = xi(j) + std(j) * randx * (parx_max(j) - parx_min(j))

!c     Check explicit constraints
            call chkcst(1,x(j),parx_min(j),parx_max(j),ibound)
!        call chkcst(nopt,1,j,x,parx_min,parx_max,ibound)
            if (ibound .ge. 1) go to 2
        end do
        
!        do k = 1,npar
!            write(*,*)k,x(k),parx_min(k),parx_max(k)
!        end do
!c     Check implicit constraints
        call chkcst(npar,x,parx_min,parx_max,ibound)
        if (ibound .ge. 1) go to 1

        return
      end



!c===================================================================
      subroutine parstt(sSCE,npt1,x,xnstd,bound,gnrng,ipcnvg)
        USE scemod
!          subroutine parstt(npar,nopt,iOpt,npt,x,xnstd,bound,gnrng,ipcnvg)
!$debug      

!c  SUBROUTINE CHECKING FOR PARAMETER CONVERGENCE
      !!wgs    implicit real*8 (a-h,o-z)
!      real x(2000,16),xmax(16),xmin(16)
!      real xmean(16),xnstd(16),bound(16)
        TYPE(sSCE_PAR) sSCE
        integer npar, nopt,j,k,isce,npt1
        real iOpt(sSCE%nopt)
        real x(2000,sSCE%npar),xmax(sSCE%npar),xmin(sSCE%npar)
        real xmean(sSCE%npar),xnstd(sSCE%npar),bound(sSCE%npar)
        parameter (delta = 1.0d-20,peps=1.0d-3)
      
        npar = sSCE%npar
        nopt = sSCE%nopt
        iOpt = sSCE%iOpt
        npt = npt1

!c  COMPUTE MAXIMUM, MINIMUM AND STANDARD DEVIATION OF PARAMETER VALUES
        gsum = 0.d0
        do j = 1, nopt
            k = iOpt(j)
            xmax(k) = -1.0d+20
            xmin(k) = 1.0d+20
            xsum1 = 0.d0
            xsum2 = 0.d0
            do isce = 1, npt
                xmax(k) = max(x(isce,k), xmax(k)) !!dmax1
                xmin(k) = min(x(isce,k), xmin(k)) !!dmin1
                xsum1 = xsum1 + x(isce,k)
                xsum2 = xsum2 + x(isce,k)*x(isce,k)
            end do
            xmean(k) = xsum1 / dble(npt)
            xnstd(k) = (xsum2 / dble(npt) - xmean(k)*xmean(k))
            if (xnstd(k) .le. delta) xnstd(k) = delta
            xnstd(k) = sqrt(xnstd(k))
            xnstd(k) = xnstd(k) / bound(k)
            gsum = gsum + log( delta + (xmax(k)-xmin(k))/bound(k) )
        end do
        gnrng = dexp(gsum/dble(nopt))

!c  CHECK IF NORMALIZED STANDARD DEVIATION OF PARAMETER IS <= eps
        ipcnvg = 0
        if (gnrng .le. peps) then
            ipcnvg = 1
        end if

!c  END OF SUBROUTINE PARSTT
        return
      end



!c====================================================================
      subroutine comp(n,npt,ngs1,ngs2,npg,a,af,b,bf)
!$debug      
!c
!c
!c  THIS SUBROUTINE REDUCE INPUT MATRIX a(n,ngs2*npg) TO MATRIX
!c  b(n,ngs1*npg) AND VECTOR af(ngs2*npg) TO VECTOR bf(ngs1*npg)
      !!wgs    implicit real*8 (a-h,o-z)
!      real a(2000,16),af(2000),b(2000,16),bf(2000)
      real a(2000,n),af(2000),b(2000,n),bf(2000)
      do igs=1, ngs1
        do ipg=1, npg
          k1=(ipg-1)*ngs2 + igs
          k2=(ipg-1)*ngs1 + igs
          do isce=1, n
            b(k2,isce) = a(k1,isce)
          end do
          bf(k2) = af(k1)
        end do
      end do

      do j=1, npt
        do isce=1, n
          a(j,isce) = b(j,isce)
        end do
        af(j) = bf(j)
      end do

!c  END OF SUBROUTINE COMP
      return
      end



!c===================================================================
      subroutine sort(n,m,rb,ra)
!$debug      


!c  SORTING SUBROUTINE ADAPTED FROM "NUMERICAL RECIPES"
!c  BY W.H. PRESS ET AL., pp. 233-234
!c
!c  LIST OF VARIABLES
!c     ra(.) = array to be sorted
!c     rb(.,.) = arrays ordered corresponding to rearrangement of ra(.)
!c     wk(.,.), iwk(.) = local varibles
!c
      !!wgs    implicit real*8 (a-h,o-z)
!      real ra(2000),rb(2000,16),wk(2000,16)
      real ra(2000),rb(2000,m),wk(2000,m)
      integer iwk(2000)

      call indexx(n, ra, iwk)
      do 11 isce = 1, n
      wk(isce,1) = ra(isce)
   11 continue
      do 12 isce = 1, n
      ra(isce) = wk(iwk(isce),1)
   12 continue
      do 14 j = 1, m
      do 13 isce = 1, n
      wk(isce,j) = rb(isce,j)
   13 continue
   14 continue
      do 16 j = 1, m
      do 15 isce = 1, n
      rb(isce,j) = wk(iwk(isce),j)
   15 continue
   16 continue

!c  END OF SUBROUTINE SORT
      return
      end


!c===========================================================
      subroutine sort1(n,ra)
!$debug      


!c  SORTING SUBROUTINE ADAPTED FROM "NUMERICAL RECIPES"
!c  BY W.H. PRESS ET AL., pp. 231
!c
!c  LIST OF VARIABLES
!c     ra(.) = integer array to be sorted
!c
      !!wgs    implicit real*8 (a-h,o-z)
      real ra(n)

      real rra

      l = (n / 2) + 1
      ir = n
   10 continue
      if (l .gt. 1) then
      l = l - 1
      rra = ra(l)
      else
      rra = ra(ir)
      ra(ir) = ra(1)
      ir = ir - 1
      if (ir .eq. 1) then
      ra(1) = rra
      return
      end if
      end if
      isce = l
      j = l + l
   20 if (j .le. ir) then
      if (j .lt. ir) then
      if (ra(j) .lt. ra(j + 1)) j = j + 1
      end if
      if (rra .lt. ra(j)) then
      ra(isce) = ra(j)
      isce = j
      j = j + j
      else
      j = ir + 1
      end if
      goto 20
      end if
      ra(isce) = rra
      goto 10

!c  END OF SUBROUTINE SORT1
      end

      subroutine sort1_int(n,ra)
!$debug      


!c  SORTING SUBROUTINE ADAPTED FROM "NUMERICAL RECIPES"
!c  BY W.H. PRESS ET AL., pp. 231
!c
!c  LIST OF VARIABLES
!c     ra(.) = integer array to be sorted
!c
      !!wgs    implicit real*8 (a-h,o-z)
      integer ra(n)

      integer rra,l,ir,j,isce

      l = (n / 2) + 1
      ir = n
   10 continue
      if (l .gt. 1) then
      l = l - 1
      rra = ra(l)
      else
      rra = ra(ir)
      ra(ir) = ra(1)
      ir = ir - 1
      if (ir .eq. 1) then
      ra(1) = rra
      return
      end if
      end if
      isce = l
      j = l + l
   20 if (j .le. ir) then
      if (j .lt. ir) then
      if (ra(j) .lt. ra(j + 1)) j = j + 1
      end if
      if (rra .lt. ra(j)) then
      ra(isce) = ra(j)
      isce = j
      j = j + j
      else
      j = ir + 1
      end if
      goto 20
      end if
      ra(isce) = rra
      goto 10

!c  END OF SUBROUTINE SORT1
      end


!c=======================================================
      subroutine indexx(n, arrin, indx)
!$debug      


!c  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
      !!wgs    implicit real*8 (a-h,o-z)
      real arrin(n)
      integer indx(n)

      do 11 j = 1, n
      indx(j) = j
   11 continue
      l = (n / 2) + 1
      ir = n
   10 continue
      if (l .gt. 1) then
      l = l - 1
      indxt = indx(l)
      q = arrin(indxt)
      else
      indxt = indx(ir)
      q = arrin(indxt)
      indx(ir) = indx(1)
      ir = ir - 1
      if (ir .eq. 1) then
      indx(1) = indxt
      return
      end if
      end if
      isce = l
      j = l + l
   20 if (j .le. ir) then
      if (j .lt. ir) then
      if (arrin(indx(j)) .lt. arrin(indx(j + 1))) j = j + 1
      end if
      if (q .lt. arrin(indx(j))) then
      indx(isce) = indx(j)
      isce = j
      j = j + j
      else
      j = ir + 1
      end if
      goto 20
      end if
      indx(isce) = indxt
      goto 10

!c  END OF SUBROUTINE INDEXX
      end



!c==============================================================
!see "ran1.f"
!      function ran1(idum)
!!$debug
!
!
!!c  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
!      !!wgs    implicit real*8 (a-h,o-z)
!      real r(97)
!      common /rancom/ ix1,ix2,ix3
!      parameter (m1 = 259200, ia1 = 7141, ic1 = 54773, rm1 =
!     &3.8580247e-6)
!      parameter (m2 = 134456, ia2 = 8121, ic2 = 28411, rm2 =
!     &7.4373773e-6)
!      parameter (m3 = 243000, ia3 = 4561, ic3 = 51349)
!      data iff / 0 /
!      if ((idum .lt. 0) .or. (iff .eq. 0)) then
!      iff = 1
!      ix1 = mod(ic1 - idum,m1)
!      ix1 = mod((ia1 * ix1) + ic1,m1)
!      ix2 = mod(ix1,m2)
!      ix1 = mod((ia1 * ix1) + ic1,m1)
!      ix3 = mod(ix1,m3)
!      do 11 j = 1, 97
!      ix1 = mod((ia1 * ix1) + ic1,m1)
!      ix2 = mod((ia2 * ix2) + ic2,m2)
!      r(j) = (dble(ix1) + (dble(ix2) * rm2)) * rm1
!   11 continue
!      idum = 1
!      end if
!      ix1 = mod((ia1 * ix1) + ic1,m1)
!      ix2 = mod((ia2 * ix2) + ic2,m2)
!      ix3 = mod((ia3 * ix3) + ic3,m3)
!      j = 1 + ((97 * ix3) / m3)
!      if ((j .gt. 97) .or. (j .lt. 1)) pause
!      ran1 = r(j)
!      r(j) = (dble(ix1) + (dble(ix2) * rm2)) * rm1
!
!!c  END OF SUBROUTINE RAN1
!      return
!      end



!c===============================================================
!      real function gasdev(idum)	  !!real*8
!!TEST_gasdev()_BEGIN       
!        iseed = IRAND(0)
!        call srand(iseed)
!        do iwgs = 1, 1000
!            fa0 = gasdev()
!            write(319,*)fa0
!        end do
!        close(319)
!!TEST_gasdev()_END 
      
      real function gasdev()
!$debug
!c  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
      !!wgs    implicit real*8 (a-h,o-z)
      save iset,gset
      data iset / 0 /
      if (iset .eq. 0) then
    1   v1 = (2. * rand()) - 1. !v1 = (2. * ran1(idum)) - 1.
        v2 = (2. * rand()) - 1. !v2 = (2. * ran1(idum)) - 1.
        r = (v1 ** 2) + (v2 ** 2)
        if (r .ge. 1.) goto 1
        fac = sqrt(- ((2. * log(r)) / r))
        gset = v1 * fac
        gasdev = v2 * fac
        iset = 1
      else
        gasdev = gset
        iset = 0
      end if

!c  END OF SUBROUTINE GASDEV
      return
      end

      SUBROUTINE SCEPAR_WRITE(sSCE)
          USE scemod
          TYPE(sSCE_PAR) sSCE
        write(*,*)"SCE STRUCT:"
        write(*,*)"npar = ", sSCE%npar                                    !# of parameters
        write(*,*)"nopt = ", sSCE%nopt                                !# of optimized parameters
        write(*,*)"nSCE = ", sSCE%nSCE                                    !# of SCE runs  
        write(*,*)"maxn = ", sSCE%maxn                                    !max no. of trials allowed before optimization is terminated
        write(*,*) "kstop = ",sSCE%kstop                                   !number of shuffling loops in which the criterion value must change by the given percentage before optimization is terminated
        write(*,*) "ngs = ", sSCE%ngs                                     !number of complexes in the initial population
        write(*,*) "npt = ",sSCE%npt                                     !total number of points in initial population (npt=ngs*npg)
        write(*,*) "npg = ",sSCE%npg                                     !number of points in each complex
        write(*,*) "nps = ",sSCE%nps                                     !number of points in a sub-complex
        write(*,*) "nspl = ",sSCE%nspl                                    !number of evolution steps allowed for each complex before complex shuffling
        write(*,*) "mings = ",sSCE%mings                                   !minimum number of complexes required, if the number of complexes is allowed to reduce as the optimization proceeds
        write(*,*) "ideflt = ",sSCE%ideflt                                  !dIF ideflt IS EQUAL TO 0, SET THE SCE CONTROL PARAMETERS TO THE DEFAULT VALUES
        write(*,*) "iniflg = ",sSCE%iniflg                                  !flag on whether to include the initial point in population; = 0, not included;  = 1, included
        write(*,*) "ifprint = ",sSCE%ifprint                                  !flag for controlling print-out after each shuffling loop;= 0, print information on the best point of the population; = 1, print information on every point of the population
        write(*,*) "iFO1 = ", sSCE%iFO1                                    !output file1: print the results of SCEUA
        write(*,*) "iFO2 = ",sSCE%iFO2                                    !output file2: summarize optimal parameter values from nRun
        write(*,*) "iFO3 = ",sSCE%iFO3                                    !output file3: other text output
        write(*,*) "pcento = ",sSCE%pcento                                  !percentage by which the criterion value must change in given number of shuffling loops
        write(*,*) "objOPT = ",sSCE%objOPT                                 !best objective function value
        write(*,*) "iOpt[] = ",sSCE%iOpt                  !index of optimized parameters
        write(*,*) "parINI[] = ",sSCE%parINI                !initial values
        write(*,*) "parMIN[] = ",sSCE%parMIN               !lower bound values
        write(*,*) "parMAX[] = ",sSCE%parMAX                !upper bound values
        write(*,*) "parOPT[] = ",sSCE%parOPT               !best parameter values (nPar)
        write(*,*) "parName[] = ",sSCE%parName  !parameter names
      RETURN
      END
