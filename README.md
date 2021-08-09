# SWATopt: Auto-Calibration Tools for SWAT (2012 Rev.627) Model
# Author: Gangsheng Wang
# Contact: wang.gangsheng@gmail.com
Environmental Sciences Division, Oak Ridge National Laboratory, Oak Ridge Tennessee 37831-6301

  We incorporated the Shuffled Complex Evolution (SCE) algorithm (Duan et al., 1992) into the Fortran source code of SWAT2012 (Rev.627) model to implement auto-calibration. SCE is a stochastic optimization algorithm that has been widely used in calibration of hydrological models including SWAT. We can calibrate 39 parameters governing the hydrologic (i.e., water quantity) and water quality processes in SWAT (Wang et al., 2018). The 39 parameters were selected based on the sensitivity analyses in previous studies. 
  Fifteen types of calibrations with regard to various response variables were defined in SWATopt. The first five types correspond to five hydrologic response variables: daily streamflow, monthly streamflow, daily reservoir storage, daily soil water content, and monthly runoff on subbasin or HUC8; the next five types include monthly nutrient (sediment, nitrogen, phosphorus) fluxes (metric tons per month); and the last five types refer to instream monthly nutrient concentration (mg/L). Other response variables could also be defined and added to this calibration framework.

# References:
# Wang G, Jager HI, Baskaran LM, Brandt CC. Hydrologic and water quality responses to biomass production in the Tennessee river basin. GCB Bioenergy. 2018;10:877–893. https://doi.org/10.1111/gcbb.12537
