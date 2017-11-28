#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
GREP=grep
NM=nm
CCADMIN=CCadmin
RANLIB=ranlib
CC=mpicc
CCC=mpicxx
CXX=mpicxx
FC=mpif90
AS=as

# Macros
CND_PLATFORM=GNU-MacOSX
CND_DLIB_EXT=dylib
CND_CONF=Debug
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/swatsrc/MAIN.o \
	${OBJECTDIR}/swatsrc/MODparm.o \
	${OBJECTDIR}/swatsrc/MODsce.o \
	${OBJECTDIR}/swatsrc/SCEIN.o \
	${OBJECTDIR}/swatsrc/SCEUA.o \
	${OBJECTDIR}/swatsrc/SCEfunctn.o \
	${OBJECTDIR}/swatsrc/USERfunc.o \
	${OBJECTDIR}/swatsrc/addh.o \
	${OBJECTDIR}/swatsrc/albedo.o \
	${OBJECTDIR}/swatsrc/allocate_parms.o \
	${OBJECTDIR}/swatsrc/alph.o \
	${OBJECTDIR}/swatsrc/anfert.o \
	${OBJECTDIR}/swatsrc/apex_day.o \
	${OBJECTDIR}/swatsrc/apply.o \
	${OBJECTDIR}/swatsrc/ascrv.o \
	${OBJECTDIR}/swatsrc/atri.o \
	${OBJECTDIR}/swatsrc/aunif.o \
	${OBJECTDIR}/swatsrc/autoirr.o \
	${OBJECTDIR}/swatsrc/aveval.o \
	${OBJECTDIR}/swatsrc/bacteria.o \
	${OBJECTDIR}/swatsrc/biofilm.o \
	${OBJECTDIR}/swatsrc/biozone.o \
	${OBJECTDIR}/swatsrc/bmp_det_pond.o \
	${OBJECTDIR}/swatsrc/bmp_ri_pond.o \
	${OBJECTDIR}/swatsrc/bmp_sand_filter.o \
	${OBJECTDIR}/swatsrc/bmp_sed_pond.o \
	${OBJECTDIR}/swatsrc/bmp_wet_pond.o \
	${OBJECTDIR}/swatsrc/bmpfixed.o \
	${OBJECTDIR}/swatsrc/bmpinit.o \
	${OBJECTDIR}/swatsrc/buffer.o \
	${OBJECTDIR}/swatsrc/burnop.o \
	${OBJECTDIR}/swatsrc/canopyint.o \
	${OBJECTDIR}/swatsrc/caps.o \
	${OBJECTDIR}/swatsrc/carbon_new.o \
	${OBJECTDIR}/swatsrc/carbon_zhang2.o \
	${OBJECTDIR}/swatsrc/cfactor.o \
	${OBJECTDIR}/swatsrc/chkcst.o \
	${OBJECTDIR}/swatsrc/clgen.o \
	${OBJECTDIR}/swatsrc/clicon.o \
	${OBJECTDIR}/swatsrc/command.o \
	${OBJECTDIR}/swatsrc/conapply.o \
	${OBJECTDIR}/swatsrc/confert.o \
	${OBJECTDIR}/swatsrc/crackflow.o \
	${OBJECTDIR}/swatsrc/crackvol.o \
	${OBJECTDIR}/swatsrc/curno.o \
	${OBJECTDIR}/swatsrc/dailycn.o \
	${OBJECTDIR}/swatsrc/decay.o \
	${OBJECTDIR}/swatsrc/depstor.o \
	${OBJECTDIR}/swatsrc/distrib_bmps.o \
	${OBJECTDIR}/swatsrc/dormant.o \
	${OBJECTDIR}/swatsrc/drains.o \
	${OBJECTDIR}/swatsrc/dstn1.o \
	${OBJECTDIR}/swatsrc/ee.o \
	${OBJECTDIR}/swatsrc/eiusle.o \
	${OBJECTDIR}/swatsrc/enrsb.o \
	${OBJECTDIR}/swatsrc/erfc.o \
	${OBJECTDIR}/swatsrc/estimate_ksat.o \
	${OBJECTDIR}/swatsrc/etact.o \
	${OBJECTDIR}/swatsrc/etpot.o \
	${OBJECTDIR}/swatsrc/expo.o \
	${OBJECTDIR}/swatsrc/fert.o \
	${OBJECTDIR}/swatsrc/filter.o \
	${OBJECTDIR}/swatsrc/filtw.o \
	${OBJECTDIR}/swatsrc/finalbal.o \
	${OBJECTDIR}/swatsrc/gcycl.o \
	${OBJECTDIR}/swatsrc/getallo.o \
	${OBJECTDIR}/swatsrc/grass_wway.o \
	${OBJECTDIR}/swatsrc/graze.o \
	${OBJECTDIR}/swatsrc/grow.o \
	${OBJECTDIR}/swatsrc/gw_no3.o \
	${OBJECTDIR}/swatsrc/gwmod.o \
	${OBJECTDIR}/swatsrc/gwmod_deep.o \
	${OBJECTDIR}/swatsrc/gwnutr.o \
	${OBJECTDIR}/swatsrc/h2omgt_init.o \
	${OBJECTDIR}/swatsrc/harvestop.o \
	${OBJECTDIR}/swatsrc/harvgrainop.o \
	${OBJECTDIR}/swatsrc/harvkillop.o \
	${OBJECTDIR}/swatsrc/header.o \
	${OBJECTDIR}/swatsrc/headout.o \
	${OBJECTDIR}/swatsrc/hhnoqual.o \
	${OBJECTDIR}/swatsrc/hhwatqual.o \
	${OBJECTDIR}/swatsrc/hmeas.o \
	${OBJECTDIR}/swatsrc/hruaa.o \
	${OBJECTDIR}/swatsrc/hruallo.o \
	${OBJECTDIR}/swatsrc/hruday.o \
	${OBJECTDIR}/swatsrc/hrumon.o \
	${OBJECTDIR}/swatsrc/hrupond.o \
	${OBJECTDIR}/swatsrc/hrupondhr.o \
	${OBJECTDIR}/swatsrc/hruyr.o \
	${OBJECTDIR}/swatsrc/hydroinit.o \
	${OBJECTDIR}/swatsrc/icl.o \
	${OBJECTDIR}/swatsrc/impnd_init.o \
	${OBJECTDIR}/swatsrc/impndaa.o \
	${OBJECTDIR}/swatsrc/impndday.o \
	${OBJECTDIR}/swatsrc/impndmon.o \
	${OBJECTDIR}/swatsrc/impndyr.o \
	${OBJECTDIR}/swatsrc/irr_rch.o \
	${OBJECTDIR}/swatsrc/irr_res.o \
	${OBJECTDIR}/swatsrc/irrigate.o \
	${OBJECTDIR}/swatsrc/irrsub.o \
	${OBJECTDIR}/swatsrc/jdt.o \
	${OBJECTDIR}/swatsrc/killop.o \
	${OBJECTDIR}/swatsrc/lakeq.o \
	${OBJECTDIR}/swatsrc/latsed.o \
	${OBJECTDIR}/swatsrc/layersplit.o \
	${OBJECTDIR}/swatsrc/lwqdef.o \
	${OBJECTDIR}/swatsrc/nCsed_leach.o \
	${OBJECTDIR}/swatsrc/ndenit.o \
	${OBJECTDIR}/swatsrc/newtillmix.o \
	${OBJECTDIR}/swatsrc/nfix.o \
	${OBJECTDIR}/swatsrc/nitvol.o \
	${OBJECTDIR}/swatsrc/nlch.o \
	${OBJECTDIR}/swatsrc/nminrl.o \
	${OBJECTDIR}/swatsrc/noqual.o \
	${OBJECTDIR}/swatsrc/npup.o \
	${OBJECTDIR}/swatsrc/nrain.o \
	${OBJECTDIR}/swatsrc/nup.o \
	${OBJECTDIR}/swatsrc/nuts.o \
	${OBJECTDIR}/swatsrc/openwth.o \
	${OBJECTDIR}/swatsrc/operatn.o \
	${OBJECTDIR}/swatsrc/orgn.o \
	${OBJECTDIR}/swatsrc/orgncswat.o \
	${OBJECTDIR}/swatsrc/origtile.o \
	${OBJECTDIR}/swatsrc/ovr_sed.o \
	${OBJECTDIR}/swatsrc/percmacro.o \
	${OBJECTDIR}/swatsrc/percmain.o \
	${OBJECTDIR}/swatsrc/percmicro.o \
	${OBJECTDIR}/swatsrc/pestlch.o \
	${OBJECTDIR}/swatsrc/pestw.o \
	${OBJECTDIR}/swatsrc/pesty.o \
	${OBJECTDIR}/swatsrc/pgen.o \
	${OBJECTDIR}/swatsrc/pgenhr.o \
	${OBJECTDIR}/swatsrc/pkq.o \
	${OBJECTDIR}/swatsrc/plantmod.o \
	${OBJECTDIR}/swatsrc/plantop.o \
	${OBJECTDIR}/swatsrc/pmeas.o \
	${OBJECTDIR}/swatsrc/pminrl.o \
	${OBJECTDIR}/swatsrc/pminrl2.o \
	${OBJECTDIR}/swatsrc/pond.o \
	${OBJECTDIR}/swatsrc/pondhr.o \
	${OBJECTDIR}/swatsrc/pothole.o \
	${OBJECTDIR}/swatsrc/potholehr.o \
	${OBJECTDIR}/swatsrc/print_hyd.o \
	${OBJECTDIR}/swatsrc/psed.o \
	${OBJECTDIR}/swatsrc/qman.o \
	${OBJECTDIR}/swatsrc/ran1.o \
	${OBJECTDIR}/swatsrc/rchaa.o \
	${OBJECTDIR}/swatsrc/rchday.o \
	${OBJECTDIR}/swatsrc/rchinit.o \
	${OBJECTDIR}/swatsrc/rchmon.o \
	${OBJECTDIR}/swatsrc/rchuse.o \
	${OBJECTDIR}/swatsrc/rchyr.o \
	${OBJECTDIR}/swatsrc/readatmodep.o \
	${OBJECTDIR}/swatsrc/readbsn.o \
	${OBJECTDIR}/swatsrc/readchm.o \
	${OBJECTDIR}/swatsrc/readcnst.o \
	${OBJECTDIR}/swatsrc/readfcst.o \
	${OBJECTDIR}/swatsrc/readfert.o \
	${OBJECTDIR}/swatsrc/readfig.o \
	${OBJECTDIR}/swatsrc/readfile.o \
	${OBJECTDIR}/swatsrc/readgw.o \
	${OBJECTDIR}/swatsrc/readhru.o \
	${OBJECTDIR}/swatsrc/readinpt.o \
	${OBJECTDIR}/swatsrc/readlup.o \
	${OBJECTDIR}/swatsrc/readlwq.o \
	${OBJECTDIR}/swatsrc/readmgt.o \
	${OBJECTDIR}/swatsrc/readmon.o \
	${OBJECTDIR}/swatsrc/readops.o \
	${OBJECTDIR}/swatsrc/readpest.o \
	${OBJECTDIR}/swatsrc/readplant.o \
	${OBJECTDIR}/swatsrc/readpnd.o \
	${OBJECTDIR}/swatsrc/readres.o \
	${OBJECTDIR}/swatsrc/readrte.o \
	${OBJECTDIR}/swatsrc/readru.o \
	${OBJECTDIR}/swatsrc/readsdr.o \
	${OBJECTDIR}/swatsrc/readsepticbz.o \
	${OBJECTDIR}/swatsrc/readseptwq.o \
	${OBJECTDIR}/swatsrc/readsno.o \
	${OBJECTDIR}/swatsrc/readsol.o \
	${OBJECTDIR}/swatsrc/readsub.o \
	${OBJECTDIR}/swatsrc/readswq.o \
	${OBJECTDIR}/swatsrc/readtill.o \
	${OBJECTDIR}/swatsrc/readurban.o \
	${OBJECTDIR}/swatsrc/readwgn.o \
	${OBJECTDIR}/swatsrc/readwus.o \
	${OBJECTDIR}/swatsrc/readwwq.o \
	${OBJECTDIR}/swatsrc/readyr.o \
	${OBJECTDIR}/swatsrc/reccnst.o \
	${OBJECTDIR}/swatsrc/recday.o \
	${OBJECTDIR}/swatsrc/rechour.o \
	${OBJECTDIR}/swatsrc/recmon.o \
	${OBJECTDIR}/swatsrc/recyear.o \
	${OBJECTDIR}/swatsrc/regres.o \
	${OBJECTDIR}/swatsrc/res.o \
	${OBJECTDIR}/swatsrc/resbact.o \
	${OBJECTDIR}/swatsrc/resetlu.o \
	${OBJECTDIR}/swatsrc/reshr.o \
	${OBJECTDIR}/swatsrc/resinit.o \
	${OBJECTDIR}/swatsrc/resnut.o \
	${OBJECTDIR}/swatsrc/rewind_init.o \
	${OBJECTDIR}/swatsrc/rhgen.o \
	${OBJECTDIR}/swatsrc/rootfr.o \
	${OBJECTDIR}/swatsrc/route.o \
	${OBJECTDIR}/swatsrc/routels.o \
	${OBJECTDIR}/swatsrc/routeunit.o \
	${OBJECTDIR}/swatsrc/routres.o \
	${OBJECTDIR}/swatsrc/rsedaa.o \
	${OBJECTDIR}/swatsrc/rseday.o \
	${OBJECTDIR}/swatsrc/rsedmon.o \
	${OBJECTDIR}/swatsrc/rsedyr.o \
	${OBJECTDIR}/swatsrc/rtbact.o \
	${OBJECTDIR}/swatsrc/rtday.o \
	${OBJECTDIR}/swatsrc/rteinit.o \
	${OBJECTDIR}/swatsrc/rthmusk.o \
	${OBJECTDIR}/swatsrc/rthpest.o \
	${OBJECTDIR}/swatsrc/rthr.o \
	${OBJECTDIR}/swatsrc/rthsed.o \
	${OBJECTDIR}/swatsrc/rtmusk.o \
	${OBJECTDIR}/swatsrc/rtout.o \
	${OBJECTDIR}/swatsrc/rtpest.o \
	${OBJECTDIR}/swatsrc/rtsed.o \
	${OBJECTDIR}/swatsrc/rtsed_Molinas_Wu.o \
	${OBJECTDIR}/swatsrc/rtsed_bagnold.o \
	${OBJECTDIR}/swatsrc/rtsed_kodatie.o \
	${OBJECTDIR}/swatsrc/rtsed_yangsand.o \
	${OBJECTDIR}/swatsrc/sat_excess.o \
	${OBJECTDIR}/swatsrc/save.o \
	${OBJECTDIR}/swatsrc/saveconc.o \
	${OBJECTDIR}/swatsrc/sched_mgt.o \
	${OBJECTDIR}/swatsrc/schedule_ops.o \
	${OBJECTDIR}/swatsrc/sim_initday.o \
	${OBJECTDIR}/swatsrc/sim_inityr.o \
	${OBJECTDIR}/swatsrc/simulate.o \
	${OBJECTDIR}/swatsrc/slrgen.o \
	${OBJECTDIR}/swatsrc/smeas.o \
	${OBJECTDIR}/swatsrc/snom.o \
	${OBJECTDIR}/swatsrc/soil_chem.o \
	${OBJECTDIR}/swatsrc/soil_par.o \
	${OBJECTDIR}/swatsrc/soil_phys.o \
	${OBJECTDIR}/swatsrc/soil_write.o \
	${OBJECTDIR}/swatsrc/solp.o \
	${OBJECTDIR}/swatsrc/solt.o \
	${OBJECTDIR}/swatsrc/std1.o \
	${OBJECTDIR}/swatsrc/std2.o \
	${OBJECTDIR}/swatsrc/std3.o \
	${OBJECTDIR}/swatsrc/stdaa.o \
	${OBJECTDIR}/swatsrc/storeinitial.o \
	${OBJECTDIR}/swatsrc/structure.o \
	${OBJECTDIR}/swatsrc/sub_subbasin.o \
	${OBJECTDIR}/swatsrc/subaa.o \
	${OBJECTDIR}/swatsrc/subbasin.o \
	${OBJECTDIR}/swatsrc/subday.o \
	${OBJECTDIR}/swatsrc/submon.o \
	${OBJECTDIR}/swatsrc/substor.o \
	${OBJECTDIR}/swatsrc/subwq.o \
	${OBJECTDIR}/swatsrc/subyr.o \
	${OBJECTDIR}/swatsrc/sumhyd.o \
	${OBJECTDIR}/swatsrc/sumv.o \
	${OBJECTDIR}/swatsrc/surface.o \
	${OBJECTDIR}/swatsrc/surfst_h2o.o \
	${OBJECTDIR}/swatsrc/surfstor.o \
	${OBJECTDIR}/swatsrc/surq_daycn.o \
	${OBJECTDIR}/swatsrc/surq_greenampt.o \
	${OBJECTDIR}/swatsrc/swbl.o \
	${OBJECTDIR}/swatsrc/sweep.o \
	${OBJECTDIR}/swatsrc/swu.o \
	${OBJECTDIR}/swatsrc/tair.o \
	${OBJECTDIR}/swatsrc/tgen.o \
	${OBJECTDIR}/swatsrc/theta.o \
	${OBJECTDIR}/swatsrc/tillfactor.o \
	${OBJECTDIR}/swatsrc/tillmix.o \
	${OBJECTDIR}/swatsrc/tmeas.o \
	${OBJECTDIR}/swatsrc/tran.o \
	${OBJECTDIR}/swatsrc/transfer.o \
	${OBJECTDIR}/swatsrc/tstr.o \
	${OBJECTDIR}/swatsrc/ttcoef.o \
	${OBJECTDIR}/swatsrc/ttcoef_wway.o \
	${OBJECTDIR}/swatsrc/urb_bmp.o \
	${OBJECTDIR}/swatsrc/urban.o \
	${OBJECTDIR}/swatsrc/urbanhr.o \
	${OBJECTDIR}/swatsrc/varinit.o \
	${OBJECTDIR}/swatsrc/vbl.o \
	${OBJECTDIR}/swatsrc/virtual.o \
	${OBJECTDIR}/swatsrc/volq.o \
	${OBJECTDIR}/swatsrc/washp.o \
	${OBJECTDIR}/swatsrc/watbal.o \
	${OBJECTDIR}/swatsrc/water_hru.o \
	${OBJECTDIR}/swatsrc/watqual.o \
	${OBJECTDIR}/swatsrc/watqual2.o \
	${OBJECTDIR}/swatsrc/wattable.o \
	${OBJECTDIR}/swatsrc/watuse.o \
	${OBJECTDIR}/swatsrc/weatgn.o \
	${OBJECTDIR}/swatsrc/wetlan.o \
	${OBJECTDIR}/swatsrc/wmeas.o \
	${OBJECTDIR}/swatsrc/wndgen.o \
	${OBJECTDIR}/swatsrc/writea.o \
	${OBJECTDIR}/swatsrc/writeaa.o \
	${OBJECTDIR}/swatsrc/writed.o \
	${OBJECTDIR}/swatsrc/writem.o \
	${OBJECTDIR}/swatsrc/xmon.o \
	${OBJECTDIR}/swatsrc/ysed.o \
	${OBJECTDIR}/swatsrc/zero0.o \
	${OBJECTDIR}/swatsrc/zero1.o \
	${OBJECTDIR}/swatsrc/zero2.o \
	${OBJECTDIR}/swatsrc/zero_urbn.o \
	${OBJECTDIR}/swatsrc/zeroini.o


# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=-finit-local-zero

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/swatopt

${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/swatopt: ${OBJECTFILES}
	${MKDIR} -p ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}
	${LINK.f} -o ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/swatopt ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/swatsrc/MAIN.o: swatsrc/MAIN.f ${OBJECTDIR}/swatsrc/MODparm.o ${OBJECTDIR}/swatsrc/MODsce.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/MAIN.o swatsrc/MAIN.f

${OBJECTDIR}/swatsrc/MODparm.o: swatsrc/MODparm.f
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/MODparm.o swatsrc/MODparm.f

${OBJECTDIR}/swatsrc/MODsce.o: swatsrc/MODsce.f
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/MODsce.o swatsrc/MODsce.f

${OBJECTDIR}/swatsrc/SCEIN.o: swatsrc/SCEIN.f ${OBJECTDIR}/swatsrc/MODparm.o ${OBJECTDIR}/swatsrc/MODsce.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/SCEIN.o swatsrc/SCEIN.f

${OBJECTDIR}/swatsrc/SCEUA.o: swatsrc/SCEUA.f ${OBJECTDIR}/swatsrc/MODparm.o ${OBJECTDIR}/swatsrc/MODsce.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/SCEUA.o swatsrc/SCEUA.f

${OBJECTDIR}/swatsrc/SCEfunctn.o: swatsrc/SCEfunctn.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/SCEfunctn.o swatsrc/SCEfunctn.f

${OBJECTDIR}/swatsrc/USERfunc.o: swatsrc/USERfunc.f
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/USERfunc.o swatsrc/USERfunc.f

${OBJECTDIR}/swatsrc/addh.o: swatsrc/addh.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/addh.o swatsrc/addh.f

${OBJECTDIR}/swatsrc/albedo.o: swatsrc/albedo.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/albedo.o swatsrc/albedo.f

${OBJECTDIR}/swatsrc/allocate_parms.o: swatsrc/allocate_parms.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/allocate_parms.o swatsrc/allocate_parms.f

${OBJECTDIR}/swatsrc/alph.o: swatsrc/alph.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/alph.o swatsrc/alph.f

${OBJECTDIR}/swatsrc/anfert.o: swatsrc/anfert.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/anfert.o swatsrc/anfert.f

${OBJECTDIR}/swatsrc/apex_day.o: swatsrc/apex_day.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/apex_day.o swatsrc/apex_day.f

${OBJECTDIR}/swatsrc/apply.o: swatsrc/apply.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/apply.o swatsrc/apply.f

${OBJECTDIR}/swatsrc/ascrv.o: swatsrc/ascrv.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/ascrv.o swatsrc/ascrv.f

${OBJECTDIR}/swatsrc/atri.o: swatsrc/atri.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/atri.o swatsrc/atri.f

${OBJECTDIR}/swatsrc/aunif.o: swatsrc/aunif.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/aunif.o swatsrc/aunif.f

${OBJECTDIR}/swatsrc/autoirr.o: swatsrc/autoirr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/autoirr.o swatsrc/autoirr.f

${OBJECTDIR}/swatsrc/aveval.o: swatsrc/aveval.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/aveval.o swatsrc/aveval.f

${OBJECTDIR}/swatsrc/bacteria.o: swatsrc/bacteria.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/bacteria.o swatsrc/bacteria.f

${OBJECTDIR}/swatsrc/biofilm.o: swatsrc/biofilm.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/biofilm.o swatsrc/biofilm.f

${OBJECTDIR}/swatsrc/biozone.o: swatsrc/biozone.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/biozone.o swatsrc/biozone.f

${OBJECTDIR}/swatsrc/bmp_det_pond.o: swatsrc/bmp_det_pond.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/bmp_det_pond.o swatsrc/bmp_det_pond.f

${OBJECTDIR}/swatsrc/bmp_ri_pond.o: swatsrc/bmp_ri_pond.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/bmp_ri_pond.o swatsrc/bmp_ri_pond.f

${OBJECTDIR}/swatsrc/bmp_sand_filter.o: swatsrc/bmp_sand_filter.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/bmp_sand_filter.o swatsrc/bmp_sand_filter.f

${OBJECTDIR}/swatsrc/bmp_sed_pond.o: swatsrc/bmp_sed_pond.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/bmp_sed_pond.o swatsrc/bmp_sed_pond.f

${OBJECTDIR}/swatsrc/bmp_wet_pond.o: swatsrc/bmp_wet_pond.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/bmp_wet_pond.o swatsrc/bmp_wet_pond.f

${OBJECTDIR}/swatsrc/bmpfixed.o: swatsrc/bmpfixed.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/bmpfixed.o swatsrc/bmpfixed.f

${OBJECTDIR}/swatsrc/bmpinit.o: swatsrc/bmpinit.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/bmpinit.o swatsrc/bmpinit.f

${OBJECTDIR}/swatsrc/buffer.o: swatsrc/buffer.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/buffer.o swatsrc/buffer.f

${OBJECTDIR}/swatsrc/burnop.o: swatsrc/burnop.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/burnop.o swatsrc/burnop.f

${OBJECTDIR}/swatsrc/canopyint.o: swatsrc/canopyint.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/canopyint.o swatsrc/canopyint.f

${OBJECTDIR}/swatsrc/caps.o: swatsrc/caps.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/caps.o swatsrc/caps.f

${OBJECTDIR}/swatsrc/carbon_new.o: swatsrc/carbon_new.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/carbon_new.o swatsrc/carbon_new.f

${OBJECTDIR}/swatsrc/carbon_zhang2.o: swatsrc/carbon_zhang2.f90 ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/carbon_zhang2.o swatsrc/carbon_zhang2.f90

${OBJECTDIR}/swatsrc/cfactor.o: swatsrc/cfactor.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/cfactor.o swatsrc/cfactor.f

${OBJECTDIR}/swatsrc/chkcst.o: swatsrc/chkcst.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/chkcst.o swatsrc/chkcst.f

${OBJECTDIR}/swatsrc/clgen.o: swatsrc/clgen.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/clgen.o swatsrc/clgen.f

${OBJECTDIR}/swatsrc/clicon.o: swatsrc/clicon.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/clicon.o swatsrc/clicon.f

${OBJECTDIR}/swatsrc/command.o: swatsrc/command.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/command.o swatsrc/command.f

${OBJECTDIR}/swatsrc/conapply.o: swatsrc/conapply.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/conapply.o swatsrc/conapply.f

${OBJECTDIR}/swatsrc/confert.o: swatsrc/confert.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/confert.o swatsrc/confert.f

${OBJECTDIR}/swatsrc/crackflow.o: swatsrc/crackflow.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/crackflow.o swatsrc/crackflow.f

${OBJECTDIR}/swatsrc/crackvol.o: swatsrc/crackvol.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/crackvol.o swatsrc/crackvol.f

${OBJECTDIR}/swatsrc/curno.o: swatsrc/curno.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/curno.o swatsrc/curno.f

${OBJECTDIR}/swatsrc/dailycn.o: swatsrc/dailycn.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/dailycn.o swatsrc/dailycn.f

${OBJECTDIR}/swatsrc/decay.o: swatsrc/decay.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/decay.o swatsrc/decay.f

${OBJECTDIR}/swatsrc/depstor.o: swatsrc/depstor.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/depstor.o swatsrc/depstor.f

${OBJECTDIR}/swatsrc/distrib_bmps.o: swatsrc/distrib_bmps.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/distrib_bmps.o swatsrc/distrib_bmps.f

${OBJECTDIR}/swatsrc/dormant.o: swatsrc/dormant.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/dormant.o swatsrc/dormant.f

${OBJECTDIR}/swatsrc/drains.o: swatsrc/drains.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/drains.o swatsrc/drains.f

${OBJECTDIR}/swatsrc/dstn1.o: swatsrc/dstn1.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/dstn1.o swatsrc/dstn1.f

${OBJECTDIR}/swatsrc/ee.o: swatsrc/ee.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/ee.o swatsrc/ee.f

${OBJECTDIR}/swatsrc/eiusle.o: swatsrc/eiusle.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/eiusle.o swatsrc/eiusle.f

${OBJECTDIR}/swatsrc/enrsb.o: swatsrc/enrsb.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/enrsb.o swatsrc/enrsb.f

${OBJECTDIR}/swatsrc/erfc.o: swatsrc/erfc.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/erfc.o swatsrc/erfc.f

${OBJECTDIR}/swatsrc/estimate_ksat.o: swatsrc/estimate_ksat.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/estimate_ksat.o swatsrc/estimate_ksat.f

${OBJECTDIR}/swatsrc/etact.o: swatsrc/etact.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/etact.o swatsrc/etact.f

${OBJECTDIR}/swatsrc/etpot.o: swatsrc/etpot.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/etpot.o swatsrc/etpot.f

${OBJECTDIR}/swatsrc/expo.o: swatsrc/expo.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/expo.o swatsrc/expo.f

${OBJECTDIR}/swatsrc/fert.o: swatsrc/fert.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/fert.o swatsrc/fert.f

${OBJECTDIR}/swatsrc/filter.o: swatsrc/filter.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/filter.o swatsrc/filter.f

${OBJECTDIR}/swatsrc/filtw.o: swatsrc/filtw.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/filtw.o swatsrc/filtw.f

${OBJECTDIR}/swatsrc/finalbal.o: swatsrc/finalbal.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/finalbal.o swatsrc/finalbal.f

${OBJECTDIR}/swatsrc/gcycl.o: swatsrc/gcycl.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/gcycl.o swatsrc/gcycl.f

${OBJECTDIR}/swatsrc/getallo.o: swatsrc/getallo.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/getallo.o swatsrc/getallo.f

${OBJECTDIR}/swatsrc/grass_wway.o: swatsrc/grass_wway.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/grass_wway.o swatsrc/grass_wway.f

${OBJECTDIR}/swatsrc/graze.o: swatsrc/graze.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/graze.o swatsrc/graze.f

${OBJECTDIR}/swatsrc/grow.o: swatsrc/grow.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/grow.o swatsrc/grow.f

${OBJECTDIR}/swatsrc/gw_no3.o: swatsrc/gw_no3.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/gw_no3.o swatsrc/gw_no3.f

${OBJECTDIR}/swatsrc/gwmod.o: swatsrc/gwmod.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/gwmod.o swatsrc/gwmod.f

${OBJECTDIR}/swatsrc/gwmod_deep.o: swatsrc/gwmod_deep.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/gwmod_deep.o swatsrc/gwmod_deep.f

${OBJECTDIR}/swatsrc/gwnutr.o: swatsrc/gwnutr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/gwnutr.o swatsrc/gwnutr.f

${OBJECTDIR}/swatsrc/h2omgt_init.o: swatsrc/h2omgt_init.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/h2omgt_init.o swatsrc/h2omgt_init.f

${OBJECTDIR}/swatsrc/harvestop.o: swatsrc/harvestop.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/harvestop.o swatsrc/harvestop.f

${OBJECTDIR}/swatsrc/harvgrainop.o: swatsrc/harvgrainop.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/harvgrainop.o swatsrc/harvgrainop.f

${OBJECTDIR}/swatsrc/harvkillop.o: swatsrc/harvkillop.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/harvkillop.o swatsrc/harvkillop.f

${OBJECTDIR}/swatsrc/header.o: swatsrc/header.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/header.o swatsrc/header.f

${OBJECTDIR}/swatsrc/headout.o: swatsrc/headout.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/headout.o swatsrc/headout.f

${OBJECTDIR}/swatsrc/hhnoqual.o: swatsrc/hhnoqual.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hhnoqual.o swatsrc/hhnoqual.f

${OBJECTDIR}/swatsrc/hhwatqual.o: swatsrc/hhwatqual.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hhwatqual.o swatsrc/hhwatqual.f

${OBJECTDIR}/swatsrc/hmeas.o: swatsrc/hmeas.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hmeas.o swatsrc/hmeas.f

${OBJECTDIR}/swatsrc/hruaa.o: swatsrc/hruaa.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hruaa.o swatsrc/hruaa.f

${OBJECTDIR}/swatsrc/hruallo.o: swatsrc/hruallo.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hruallo.o swatsrc/hruallo.f

${OBJECTDIR}/swatsrc/hruday.o: swatsrc/hruday.f90 ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hruday.o swatsrc/hruday.f90

${OBJECTDIR}/swatsrc/hrumon.o: swatsrc/hrumon.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hrumon.o swatsrc/hrumon.f

${OBJECTDIR}/swatsrc/hrupond.o: swatsrc/hrupond.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hrupond.o swatsrc/hrupond.f

${OBJECTDIR}/swatsrc/hrupondhr.o: swatsrc/hrupondhr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hrupondhr.o swatsrc/hrupondhr.f

${OBJECTDIR}/swatsrc/hruyr.o: swatsrc/hruyr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hruyr.o swatsrc/hruyr.f

${OBJECTDIR}/swatsrc/hydroinit.o: swatsrc/hydroinit.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/hydroinit.o swatsrc/hydroinit.f

${OBJECTDIR}/swatsrc/icl.o: swatsrc/icl.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/icl.o swatsrc/icl.f

${OBJECTDIR}/swatsrc/impnd_init.o: swatsrc/impnd_init.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/impnd_init.o swatsrc/impnd_init.f

${OBJECTDIR}/swatsrc/impndaa.o: swatsrc/impndaa.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/impndaa.o swatsrc/impndaa.f

${OBJECTDIR}/swatsrc/impndday.o: swatsrc/impndday.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/impndday.o swatsrc/impndday.f

${OBJECTDIR}/swatsrc/impndmon.o: swatsrc/impndmon.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/impndmon.o swatsrc/impndmon.f

${OBJECTDIR}/swatsrc/impndyr.o: swatsrc/impndyr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/impndyr.o swatsrc/impndyr.f

${OBJECTDIR}/swatsrc/irr_rch.o: swatsrc/irr_rch.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/irr_rch.o swatsrc/irr_rch.f

${OBJECTDIR}/swatsrc/irr_res.o: swatsrc/irr_res.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/irr_res.o swatsrc/irr_res.f

${OBJECTDIR}/swatsrc/irrigate.o: swatsrc/irrigate.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/irrigate.o swatsrc/irrigate.f

${OBJECTDIR}/swatsrc/irrsub.o: swatsrc/irrsub.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/irrsub.o swatsrc/irrsub.f

${OBJECTDIR}/swatsrc/jdt.o: swatsrc/jdt.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/jdt.o swatsrc/jdt.f

${OBJECTDIR}/swatsrc/killop.o: swatsrc/killop.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/killop.o swatsrc/killop.f

${OBJECTDIR}/swatsrc/lakeq.o: swatsrc/lakeq.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/lakeq.o swatsrc/lakeq.f

${OBJECTDIR}/swatsrc/latsed.o: swatsrc/latsed.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/latsed.o swatsrc/latsed.f

${OBJECTDIR}/swatsrc/layersplit.o: swatsrc/layersplit.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/layersplit.o swatsrc/layersplit.f

${OBJECTDIR}/swatsrc/lwqdef.o: swatsrc/lwqdef.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/lwqdef.o swatsrc/lwqdef.f

${OBJECTDIR}/swatsrc/nCsed_leach.o: swatsrc/nCsed_leach.f90 ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/nCsed_leach.o swatsrc/nCsed_leach.f90

${OBJECTDIR}/swatsrc/ndenit.o: swatsrc/ndenit.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/ndenit.o swatsrc/ndenit.f

${OBJECTDIR}/swatsrc/newtillmix.o: swatsrc/newtillmix.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/newtillmix.o swatsrc/newtillmix.f

${OBJECTDIR}/swatsrc/nfix.o: swatsrc/nfix.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/nfix.o swatsrc/nfix.f

${OBJECTDIR}/swatsrc/nitvol.o: swatsrc/nitvol.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/nitvol.o swatsrc/nitvol.f

${OBJECTDIR}/swatsrc/nlch.o: swatsrc/nlch.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/nlch.o swatsrc/nlch.f

${OBJECTDIR}/swatsrc/nminrl.o: swatsrc/nminrl.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/nminrl.o swatsrc/nminrl.f

${OBJECTDIR}/swatsrc/noqual.o: swatsrc/noqual.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/noqual.o swatsrc/noqual.f

${OBJECTDIR}/swatsrc/npup.o: swatsrc/npup.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/npup.o swatsrc/npup.f

${OBJECTDIR}/swatsrc/nrain.o: swatsrc/nrain.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/nrain.o swatsrc/nrain.f

${OBJECTDIR}/swatsrc/nup.o: swatsrc/nup.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/nup.o swatsrc/nup.f

${OBJECTDIR}/swatsrc/nuts.o: swatsrc/nuts.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/nuts.o swatsrc/nuts.f

${OBJECTDIR}/swatsrc/openwth.o: swatsrc/openwth.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/openwth.o swatsrc/openwth.f

${OBJECTDIR}/swatsrc/operatn.o: swatsrc/operatn.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/operatn.o swatsrc/operatn.f

${OBJECTDIR}/swatsrc/orgn.o: swatsrc/orgn.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/orgn.o swatsrc/orgn.f

${OBJECTDIR}/swatsrc/orgncswat.o: swatsrc/orgncswat.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/orgncswat.o swatsrc/orgncswat.f

${OBJECTDIR}/swatsrc/origtile.o: swatsrc/origtile.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/origtile.o swatsrc/origtile.f

${OBJECTDIR}/swatsrc/ovr_sed.o: swatsrc/ovr_sed.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/ovr_sed.o swatsrc/ovr_sed.f

${OBJECTDIR}/swatsrc/percmacro.o: swatsrc/percmacro.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/percmacro.o swatsrc/percmacro.f

${OBJECTDIR}/swatsrc/percmain.o: swatsrc/percmain.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/percmain.o swatsrc/percmain.f

${OBJECTDIR}/swatsrc/percmicro.o: swatsrc/percmicro.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/percmicro.o swatsrc/percmicro.f

${OBJECTDIR}/swatsrc/pestlch.o: swatsrc/pestlch.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pestlch.o swatsrc/pestlch.f

${OBJECTDIR}/swatsrc/pestw.o: swatsrc/pestw.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pestw.o swatsrc/pestw.f

${OBJECTDIR}/swatsrc/pesty.o: swatsrc/pesty.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pesty.o swatsrc/pesty.f

${OBJECTDIR}/swatsrc/pgen.o: swatsrc/pgen.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pgen.o swatsrc/pgen.f

${OBJECTDIR}/swatsrc/pgenhr.o: swatsrc/pgenhr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pgenhr.o swatsrc/pgenhr.f

${OBJECTDIR}/swatsrc/pkq.o: swatsrc/pkq.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pkq.o swatsrc/pkq.f

${OBJECTDIR}/swatsrc/plantmod.o: swatsrc/plantmod.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/plantmod.o swatsrc/plantmod.f

${OBJECTDIR}/swatsrc/plantop.o: swatsrc/plantop.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/plantop.o swatsrc/plantop.f

${OBJECTDIR}/swatsrc/pmeas.o: swatsrc/pmeas.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pmeas.o swatsrc/pmeas.f

${OBJECTDIR}/swatsrc/pminrl.o: swatsrc/pminrl.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pminrl.o swatsrc/pminrl.f

${OBJECTDIR}/swatsrc/pminrl2.o: swatsrc/pminrl2.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pminrl2.o swatsrc/pminrl2.f

${OBJECTDIR}/swatsrc/pond.o: swatsrc/pond.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pond.o swatsrc/pond.f

${OBJECTDIR}/swatsrc/pondhr.o: swatsrc/pondhr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pondhr.o swatsrc/pondhr.f

${OBJECTDIR}/swatsrc/pothole.o: swatsrc/pothole.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/pothole.o swatsrc/pothole.f

${OBJECTDIR}/swatsrc/potholehr.o: swatsrc/potholehr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/potholehr.o swatsrc/potholehr.f

${OBJECTDIR}/swatsrc/print_hyd.o: swatsrc/print_hyd.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/print_hyd.o swatsrc/print_hyd.f

${OBJECTDIR}/swatsrc/psed.o: swatsrc/psed.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/psed.o swatsrc/psed.f

${OBJECTDIR}/swatsrc/qman.o: swatsrc/qman.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/qman.o swatsrc/qman.f

${OBJECTDIR}/swatsrc/ran1.o: swatsrc/ran1.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/ran1.o swatsrc/ran1.f

${OBJECTDIR}/swatsrc/rchaa.o: swatsrc/rchaa.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rchaa.o swatsrc/rchaa.f

${OBJECTDIR}/swatsrc/rchday.o: swatsrc/rchday.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rchday.o swatsrc/rchday.f

${OBJECTDIR}/swatsrc/rchinit.o: swatsrc/rchinit.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rchinit.o swatsrc/rchinit.f

${OBJECTDIR}/swatsrc/rchmon.o: swatsrc/rchmon.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rchmon.o swatsrc/rchmon.f

${OBJECTDIR}/swatsrc/rchuse.o: swatsrc/rchuse.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rchuse.o swatsrc/rchuse.f

${OBJECTDIR}/swatsrc/rchyr.o: swatsrc/rchyr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rchyr.o swatsrc/rchyr.f

${OBJECTDIR}/swatsrc/readatmodep.o: swatsrc/readatmodep.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readatmodep.o swatsrc/readatmodep.f

${OBJECTDIR}/swatsrc/readbsn.o: swatsrc/readbsn.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readbsn.o swatsrc/readbsn.f

${OBJECTDIR}/swatsrc/readchm.o: swatsrc/readchm.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readchm.o swatsrc/readchm.f

${OBJECTDIR}/swatsrc/readcnst.o: swatsrc/readcnst.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readcnst.o swatsrc/readcnst.f

${OBJECTDIR}/swatsrc/readfcst.o: swatsrc/readfcst.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readfcst.o swatsrc/readfcst.f

${OBJECTDIR}/swatsrc/readfert.o: swatsrc/readfert.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readfert.o swatsrc/readfert.f

${OBJECTDIR}/swatsrc/readfig.o: swatsrc/readfig.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readfig.o swatsrc/readfig.f

${OBJECTDIR}/swatsrc/readfile.o: swatsrc/readfile.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readfile.o swatsrc/readfile.f

${OBJECTDIR}/swatsrc/readgw.o: swatsrc/readgw.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readgw.o swatsrc/readgw.f

${OBJECTDIR}/swatsrc/readhru.o: swatsrc/readhru.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readhru.o swatsrc/readhru.f

${OBJECTDIR}/swatsrc/readinpt.o: swatsrc/readinpt.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readinpt.o swatsrc/readinpt.f

${OBJECTDIR}/swatsrc/readlup.o: swatsrc/readlup.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readlup.o swatsrc/readlup.f

${OBJECTDIR}/swatsrc/readlwq.o: swatsrc/readlwq.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readlwq.o swatsrc/readlwq.f

${OBJECTDIR}/swatsrc/readmgt.o: swatsrc/readmgt.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readmgt.o swatsrc/readmgt.f

${OBJECTDIR}/swatsrc/readmon.o: swatsrc/readmon.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readmon.o swatsrc/readmon.f

${OBJECTDIR}/swatsrc/readops.o: swatsrc/readops.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readops.o swatsrc/readops.f

${OBJECTDIR}/swatsrc/readpest.o: swatsrc/readpest.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readpest.o swatsrc/readpest.f

${OBJECTDIR}/swatsrc/readplant.o: swatsrc/readplant.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readplant.o swatsrc/readplant.f

${OBJECTDIR}/swatsrc/readpnd.o: swatsrc/readpnd.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readpnd.o swatsrc/readpnd.f

${OBJECTDIR}/swatsrc/readres.o: swatsrc/readres.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readres.o swatsrc/readres.f

${OBJECTDIR}/swatsrc/readrte.o: swatsrc/readrte.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readrte.o swatsrc/readrte.f

${OBJECTDIR}/swatsrc/readru.o: swatsrc/readru.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readru.o swatsrc/readru.f

${OBJECTDIR}/swatsrc/readsdr.o: swatsrc/readsdr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readsdr.o swatsrc/readsdr.f

${OBJECTDIR}/swatsrc/readsepticbz.o: swatsrc/readsepticbz.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readsepticbz.o swatsrc/readsepticbz.f

${OBJECTDIR}/swatsrc/readseptwq.o: swatsrc/readseptwq.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readseptwq.o swatsrc/readseptwq.f

${OBJECTDIR}/swatsrc/readsno.o: swatsrc/readsno.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readsno.o swatsrc/readsno.f

${OBJECTDIR}/swatsrc/readsol.o: swatsrc/readsol.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readsol.o swatsrc/readsol.f

${OBJECTDIR}/swatsrc/readsub.o: swatsrc/readsub.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readsub.o swatsrc/readsub.f

${OBJECTDIR}/swatsrc/readswq.o: swatsrc/readswq.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readswq.o swatsrc/readswq.f

${OBJECTDIR}/swatsrc/readtill.o: swatsrc/readtill.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readtill.o swatsrc/readtill.f

${OBJECTDIR}/swatsrc/readurban.o: swatsrc/readurban.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readurban.o swatsrc/readurban.f

${OBJECTDIR}/swatsrc/readwgn.o: swatsrc/readwgn.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readwgn.o swatsrc/readwgn.f

${OBJECTDIR}/swatsrc/readwus.o: swatsrc/readwus.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readwus.o swatsrc/readwus.f

${OBJECTDIR}/swatsrc/readwwq.o: swatsrc/readwwq.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readwwq.o swatsrc/readwwq.f

${OBJECTDIR}/swatsrc/readyr.o: swatsrc/readyr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/readyr.o swatsrc/readyr.f

${OBJECTDIR}/swatsrc/reccnst.o: swatsrc/reccnst.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/reccnst.o swatsrc/reccnst.f

${OBJECTDIR}/swatsrc/recday.o: swatsrc/recday.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/recday.o swatsrc/recday.f

${OBJECTDIR}/swatsrc/rechour.o: swatsrc/rechour.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rechour.o swatsrc/rechour.f

${OBJECTDIR}/swatsrc/recmon.o: swatsrc/recmon.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/recmon.o swatsrc/recmon.f

${OBJECTDIR}/swatsrc/recyear.o: swatsrc/recyear.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/recyear.o swatsrc/recyear.f

${OBJECTDIR}/swatsrc/regres.o: swatsrc/regres.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/regres.o swatsrc/regres.f

${OBJECTDIR}/swatsrc/res.o: swatsrc/res.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/res.o swatsrc/res.f

${OBJECTDIR}/swatsrc/resbact.o: swatsrc/resbact.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/resbact.o swatsrc/resbact.f

${OBJECTDIR}/swatsrc/resetlu.o: swatsrc/resetlu.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/resetlu.o swatsrc/resetlu.f

${OBJECTDIR}/swatsrc/reshr.o: swatsrc/reshr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/reshr.o swatsrc/reshr.f

${OBJECTDIR}/swatsrc/resinit.o: swatsrc/resinit.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/resinit.o swatsrc/resinit.f

${OBJECTDIR}/swatsrc/resnut.o: swatsrc/resnut.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/resnut.o swatsrc/resnut.f

${OBJECTDIR}/swatsrc/rewind_init.o: swatsrc/rewind_init.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rewind_init.o swatsrc/rewind_init.f

${OBJECTDIR}/swatsrc/rhgen.o: swatsrc/rhgen.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rhgen.o swatsrc/rhgen.f

${OBJECTDIR}/swatsrc/rootfr.o: swatsrc/rootfr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rootfr.o swatsrc/rootfr.f

${OBJECTDIR}/swatsrc/route.o: swatsrc/route.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/route.o swatsrc/route.f

${OBJECTDIR}/swatsrc/routels.o: swatsrc/routels.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/routels.o swatsrc/routels.f

${OBJECTDIR}/swatsrc/routeunit.o: swatsrc/routeunit.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/routeunit.o swatsrc/routeunit.f

${OBJECTDIR}/swatsrc/routres.o: swatsrc/routres.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/routres.o swatsrc/routres.f

${OBJECTDIR}/swatsrc/rsedaa.o: swatsrc/rsedaa.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rsedaa.o swatsrc/rsedaa.f

${OBJECTDIR}/swatsrc/rseday.o: swatsrc/rseday.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rseday.o swatsrc/rseday.f

${OBJECTDIR}/swatsrc/rsedmon.o: swatsrc/rsedmon.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rsedmon.o swatsrc/rsedmon.f

${OBJECTDIR}/swatsrc/rsedyr.o: swatsrc/rsedyr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rsedyr.o swatsrc/rsedyr.f

${OBJECTDIR}/swatsrc/rtbact.o: swatsrc/rtbact.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rtbact.o swatsrc/rtbact.f

${OBJECTDIR}/swatsrc/rtday.o: swatsrc/rtday.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rtday.o swatsrc/rtday.f

${OBJECTDIR}/swatsrc/rteinit.o: swatsrc/rteinit.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rteinit.o swatsrc/rteinit.f

${OBJECTDIR}/swatsrc/rthmusk.o: swatsrc/rthmusk.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rthmusk.o swatsrc/rthmusk.f

${OBJECTDIR}/swatsrc/rthpest.o: swatsrc/rthpest.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rthpest.o swatsrc/rthpest.f

${OBJECTDIR}/swatsrc/rthr.o: swatsrc/rthr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rthr.o swatsrc/rthr.f

${OBJECTDIR}/swatsrc/rthsed.o: swatsrc/rthsed.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rthsed.o swatsrc/rthsed.f

${OBJECTDIR}/swatsrc/rtmusk.o: swatsrc/rtmusk.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rtmusk.o swatsrc/rtmusk.f

${OBJECTDIR}/swatsrc/rtout.o: swatsrc/rtout.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rtout.o swatsrc/rtout.f

${OBJECTDIR}/swatsrc/rtpest.o: swatsrc/rtpest.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rtpest.o swatsrc/rtpest.f

${OBJECTDIR}/swatsrc/rtsed.o: swatsrc/rtsed.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rtsed.o swatsrc/rtsed.f

${OBJECTDIR}/swatsrc/rtsed_Molinas_Wu.o: swatsrc/rtsed_Molinas_Wu.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rtsed_Molinas_Wu.o swatsrc/rtsed_Molinas_Wu.f

${OBJECTDIR}/swatsrc/rtsed_bagnold.o: swatsrc/rtsed_bagnold.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rtsed_bagnold.o swatsrc/rtsed_bagnold.f

${OBJECTDIR}/swatsrc/rtsed_kodatie.o: swatsrc/rtsed_kodatie.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rtsed_kodatie.o swatsrc/rtsed_kodatie.f

${OBJECTDIR}/swatsrc/rtsed_yangsand.o: swatsrc/rtsed_yangsand.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/rtsed_yangsand.o swatsrc/rtsed_yangsand.f

${OBJECTDIR}/swatsrc/sat_excess.o: swatsrc/sat_excess.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/sat_excess.o swatsrc/sat_excess.f

${OBJECTDIR}/swatsrc/save.o: swatsrc/save.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/save.o swatsrc/save.f

${OBJECTDIR}/swatsrc/saveconc.o: swatsrc/saveconc.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/saveconc.o swatsrc/saveconc.f

${OBJECTDIR}/swatsrc/sched_mgt.o: swatsrc/sched_mgt.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/sched_mgt.o swatsrc/sched_mgt.f

${OBJECTDIR}/swatsrc/schedule_ops.o: swatsrc/schedule_ops.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/schedule_ops.o swatsrc/schedule_ops.f

${OBJECTDIR}/swatsrc/sim_initday.o: swatsrc/sim_initday.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/sim_initday.o swatsrc/sim_initday.f

${OBJECTDIR}/swatsrc/sim_inityr.o: swatsrc/sim_inityr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/sim_inityr.o swatsrc/sim_inityr.f

${OBJECTDIR}/swatsrc/simulate.o: swatsrc/simulate.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/simulate.o swatsrc/simulate.f

${OBJECTDIR}/swatsrc/slrgen.o: swatsrc/slrgen.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/slrgen.o swatsrc/slrgen.f

${OBJECTDIR}/swatsrc/smeas.o: swatsrc/smeas.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/smeas.o swatsrc/smeas.f

${OBJECTDIR}/swatsrc/snom.o: swatsrc/snom.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/snom.o swatsrc/snom.f

${OBJECTDIR}/swatsrc/soil_chem.o: swatsrc/soil_chem.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/soil_chem.o swatsrc/soil_chem.f

${OBJECTDIR}/swatsrc/soil_par.o: swatsrc/soil_par.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/soil_par.o swatsrc/soil_par.f

${OBJECTDIR}/swatsrc/soil_phys.o: swatsrc/soil_phys.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/soil_phys.o swatsrc/soil_phys.f

${OBJECTDIR}/swatsrc/soil_write.o: swatsrc/soil_write.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/soil_write.o swatsrc/soil_write.f

${OBJECTDIR}/swatsrc/solp.o: swatsrc/solp.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/solp.o swatsrc/solp.f

${OBJECTDIR}/swatsrc/solt.o: swatsrc/solt.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/solt.o swatsrc/solt.f

${OBJECTDIR}/swatsrc/std1.o: swatsrc/std1.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/std1.o swatsrc/std1.f

${OBJECTDIR}/swatsrc/std2.o: swatsrc/std2.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/std2.o swatsrc/std2.f

${OBJECTDIR}/swatsrc/std3.o: swatsrc/std3.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/std3.o swatsrc/std3.f

${OBJECTDIR}/swatsrc/stdaa.o: swatsrc/stdaa.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/stdaa.o swatsrc/stdaa.f

${OBJECTDIR}/swatsrc/storeinitial.o: swatsrc/storeinitial.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/storeinitial.o swatsrc/storeinitial.f

${OBJECTDIR}/swatsrc/structure.o: swatsrc/structure.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/structure.o swatsrc/structure.f

${OBJECTDIR}/swatsrc/sub_subbasin.o: swatsrc/sub_subbasin.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/sub_subbasin.o swatsrc/sub_subbasin.f

${OBJECTDIR}/swatsrc/subaa.o: swatsrc/subaa.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/subaa.o swatsrc/subaa.f

${OBJECTDIR}/swatsrc/subbasin.o: swatsrc/subbasin.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/subbasin.o swatsrc/subbasin.f

${OBJECTDIR}/swatsrc/subday.o: swatsrc/subday.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/subday.o swatsrc/subday.f

${OBJECTDIR}/swatsrc/submon.o: swatsrc/submon.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/submon.o swatsrc/submon.f

${OBJECTDIR}/swatsrc/substor.o: swatsrc/substor.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/substor.o swatsrc/substor.f

${OBJECTDIR}/swatsrc/subwq.o: swatsrc/subwq.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/subwq.o swatsrc/subwq.f

${OBJECTDIR}/swatsrc/subyr.o: swatsrc/subyr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/subyr.o swatsrc/subyr.f

${OBJECTDIR}/swatsrc/sumhyd.o: swatsrc/sumhyd.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/sumhyd.o swatsrc/sumhyd.f

${OBJECTDIR}/swatsrc/sumv.o: swatsrc/sumv.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/sumv.o swatsrc/sumv.f

${OBJECTDIR}/swatsrc/surface.o: swatsrc/surface.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/surface.o swatsrc/surface.f

${OBJECTDIR}/swatsrc/surfst_h2o.o: swatsrc/surfst_h2o.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/surfst_h2o.o swatsrc/surfst_h2o.f

${OBJECTDIR}/swatsrc/surfstor.o: swatsrc/surfstor.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/surfstor.o swatsrc/surfstor.f

${OBJECTDIR}/swatsrc/surq_daycn.o: swatsrc/surq_daycn.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/surq_daycn.o swatsrc/surq_daycn.f

${OBJECTDIR}/swatsrc/surq_greenampt.o: swatsrc/surq_greenampt.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/surq_greenampt.o swatsrc/surq_greenampt.f

${OBJECTDIR}/swatsrc/swbl.o: swatsrc/swbl.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/swbl.o swatsrc/swbl.f

${OBJECTDIR}/swatsrc/sweep.o: swatsrc/sweep.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/sweep.o swatsrc/sweep.f

${OBJECTDIR}/swatsrc/swu.o: swatsrc/swu.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/swu.o swatsrc/swu.f

${OBJECTDIR}/swatsrc/tair.o: swatsrc/tair.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/tair.o swatsrc/tair.f

${OBJECTDIR}/swatsrc/tgen.o: swatsrc/tgen.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/tgen.o swatsrc/tgen.f

${OBJECTDIR}/swatsrc/theta.o: swatsrc/theta.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/theta.o swatsrc/theta.f

${OBJECTDIR}/swatsrc/tillfactor.o: swatsrc/tillfactor.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/tillfactor.o swatsrc/tillfactor.f

${OBJECTDIR}/swatsrc/tillmix.o: swatsrc/tillmix.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/tillmix.o swatsrc/tillmix.f

${OBJECTDIR}/swatsrc/tmeas.o: swatsrc/tmeas.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/tmeas.o swatsrc/tmeas.f

${OBJECTDIR}/swatsrc/tran.o: swatsrc/tran.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/tran.o swatsrc/tran.f

${OBJECTDIR}/swatsrc/transfer.o: swatsrc/transfer.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/transfer.o swatsrc/transfer.f

${OBJECTDIR}/swatsrc/tstr.o: swatsrc/tstr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/tstr.o swatsrc/tstr.f

${OBJECTDIR}/swatsrc/ttcoef.o: swatsrc/ttcoef.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/ttcoef.o swatsrc/ttcoef.f

${OBJECTDIR}/swatsrc/ttcoef_wway.o: swatsrc/ttcoef_wway.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/ttcoef_wway.o swatsrc/ttcoef_wway.f

${OBJECTDIR}/swatsrc/urb_bmp.o: swatsrc/urb_bmp.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/urb_bmp.o swatsrc/urb_bmp.f

${OBJECTDIR}/swatsrc/urban.o: swatsrc/urban.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/urban.o swatsrc/urban.f

${OBJECTDIR}/swatsrc/urbanhr.o: swatsrc/urbanhr.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/urbanhr.o swatsrc/urbanhr.f

${OBJECTDIR}/swatsrc/varinit.o: swatsrc/varinit.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/varinit.o swatsrc/varinit.f

${OBJECTDIR}/swatsrc/vbl.o: swatsrc/vbl.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/vbl.o swatsrc/vbl.f

${OBJECTDIR}/swatsrc/virtual.o: swatsrc/virtual.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/virtual.o swatsrc/virtual.f

${OBJECTDIR}/swatsrc/volq.o: swatsrc/volq.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/volq.o swatsrc/volq.f

${OBJECTDIR}/swatsrc/washp.o: swatsrc/washp.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/washp.o swatsrc/washp.f

${OBJECTDIR}/swatsrc/watbal.o: swatsrc/watbal.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/watbal.o swatsrc/watbal.f

${OBJECTDIR}/swatsrc/water_hru.o: swatsrc/water_hru.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/water_hru.o swatsrc/water_hru.f

${OBJECTDIR}/swatsrc/watqual.o: swatsrc/watqual.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/watqual.o swatsrc/watqual.f

${OBJECTDIR}/swatsrc/watqual2.o: swatsrc/watqual2.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/watqual2.o swatsrc/watqual2.f

${OBJECTDIR}/swatsrc/wattable.o: swatsrc/wattable.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/wattable.o swatsrc/wattable.f

${OBJECTDIR}/swatsrc/watuse.o: swatsrc/watuse.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/watuse.o swatsrc/watuse.f

${OBJECTDIR}/swatsrc/weatgn.o: swatsrc/weatgn.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/weatgn.o swatsrc/weatgn.f

${OBJECTDIR}/swatsrc/wetlan.o: swatsrc/wetlan.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/wetlan.o swatsrc/wetlan.f

${OBJECTDIR}/swatsrc/wmeas.o: swatsrc/wmeas.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/wmeas.o swatsrc/wmeas.f

${OBJECTDIR}/swatsrc/wndgen.o: swatsrc/wndgen.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/wndgen.o swatsrc/wndgen.f

${OBJECTDIR}/swatsrc/writea.o: swatsrc/writea.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/writea.o swatsrc/writea.f

${OBJECTDIR}/swatsrc/writeaa.o: swatsrc/writeaa.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/writeaa.o swatsrc/writeaa.f

${OBJECTDIR}/swatsrc/writed.o: swatsrc/writed.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/writed.o swatsrc/writed.f

${OBJECTDIR}/swatsrc/writem.o: swatsrc/writem.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/writem.o swatsrc/writem.f

${OBJECTDIR}/swatsrc/xmon.o: swatsrc/xmon.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/xmon.o swatsrc/xmon.f

${OBJECTDIR}/swatsrc/ysed.o: swatsrc/ysed.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/ysed.o swatsrc/ysed.f

${OBJECTDIR}/swatsrc/zero0.o: swatsrc/zero0.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/zero0.o swatsrc/zero0.f

${OBJECTDIR}/swatsrc/zero1.o: swatsrc/zero1.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/zero1.o swatsrc/zero1.f

${OBJECTDIR}/swatsrc/zero2.o: swatsrc/zero2.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/zero2.o swatsrc/zero2.f

${OBJECTDIR}/swatsrc/zero_urbn.o: swatsrc/zero_urbn.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/zero_urbn.o swatsrc/zero_urbn.f

${OBJECTDIR}/swatsrc/zeroini.o: swatsrc/zeroini.f ${OBJECTDIR}/swatsrc/MODparm.o
	${MKDIR} -p ${OBJECTDIR}/swatsrc
	$(COMPILE.f) -g -o ${OBJECTDIR}/swatsrc/zeroini.o swatsrc/zeroini.f

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} *.mod

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
