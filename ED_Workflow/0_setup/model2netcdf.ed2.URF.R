#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


##' Modified from Code to convert ED2.1's HDF5 output into the NACP Intercomparison format (ALMA using netCDF)
##'
##' @name model2netcdf.ED2
##' @title Code to convert ED2's -I- HDF5 output into netCDF format
##'
##' @param ed.dir Location of ED model output
##' @param outdir Location of extracted & syntehsized output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param pft_names Names of PFTs used in the run, vector
##' @param ed.freq Frequency of files to be analyzed:
##'                I = instaneous (set in ED2IN), 
##'                E = monthly means,
##'                Y = annual means
##'                
##' @export
##'
##' @author Michael Dietze, Shawn Serbin, Rob Kooper, Toni Viskari, Istem Fer
## modified M. Dietze 07/08/12 modified S. Serbin 05/06/13
## refactored by Istem Fer on 03/2018
model2netcdf.ED2.URF <- function(ed.dir, outdir, sitelat, sitelon, start_date, end_date, pft_names = NULL, ed.freq=c("I", "E", "Y")) {
  
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date) 
  
  flist <- list()
  for(FREQ in ed.freq){
    flist[[FREQ]] <- dir(ed.dir, paste0("-", FREQ, "-"))
  }
  # flist[["-I-"]] <- dir(outdir, "-I-") # tower files
  # flist[["-E-"]] <- dir(outdir, "-E-") # monthly files
  
  # check if there are files
  file.check <- sapply(flist, function (f) length(f) != 0)
  
  if (!any(file.check)) {
    
    # no output files
    stop("WARNING: No output files found for :", outdir)
    return(NULL)
    
  }else{ 
    
    # which output files are there
    ed.res.flag <- names(flist)[file.check]
    
    # extract year info from the file names
    ylist <-lapply(ed.res.flag, function(f) {
      yr <- rep(NA, length(flist[[f]]))
      for (i in seq_along(flist[[f]])) {
        index <- gregexpr(f, flist[[f]][i])[[1]] # Find where our time stamp flag is
        index <- index[1]
        yr[i] <- as.numeric(substr(flist[[f]][i], index + 2, index + 5)) # The year starts 2 characters after our timestamp
      }
      return(yr)
    })
    
    names(ylist) <- ed.res.flag
  }
  
  # prepare list to collect outputs
  out_list <- vector("list", length(ed.res.flag)) 
  names(out_list) <- ed.res.flag
  
  # if run failed there might be less years, no output case is handled above
  # we can process whatever is there
  # but of course this upsets ensemble.ts because the outputs are not of same length now
  # two options:
  # (i)  don't process anything
  #      return(NULL)
  # (ii) check whether this is an ensemble run, then return null, otherwise process whatever there is
  #      for now I'm going with this, do failed runs also provide information on parameters?
  year.check <- unique(unlist(ylist))
  if(max(year.check) < end_year){
    warning("Run failed with some outputs.")
    # rundir <- gsub("/out/", "/run/", outdir)
    # readme <- file(paste0(rundir,"/README.txt"))
    # runtype <- readLines(readme, n=1)
    # close(readme)
    # if(grepl("ensemble", runtype)){
    #    PEcAn.logger::logger.info("This is an ensemble run. Not processing anything.")
    #    return(NULL)
    # }else{
    #   PEcAn.logger::logger.info("This is not an ensemble run. Processing existing outputs.")
    #    end_year <- max(year.check)
    # }
  }
  
  # ----- start loop over years
  for(y in start_year:end_year){
    
    print(paste0("----- Processing year: ", y))
    
    outfile <- file.path(outdir, paste("ED2", y, "nc", sep = "."))
    if(file.exists(outfile)) next # Skip to next one if already done
    
    # ----- read values from ED output files
    for(j in seq_along(out_list)){
      rflag <- ed.res.flag[j]
      fcnx  <- paste0("read_", gsub("-", "", rflag), "_files")
      fcn   <- match.fun(fcnx)
      out_list[[rflag]] <- fcn(yr = y, yfiles=ylist[[rflag]], tfiles=flist[[rflag]], 
                               outdir=ed.dir, start_date=start_date, end_date=end_date, 
                               pft_names)
    }
    
    
    if (y == strftime(start_date, "%Y")) {
      begins <- as.numeric(strftime(start_date, "%j")) - 1
    } else {
      begins <- 0
    }
    
    if (y == strftime(end_date, "%Y")) {
      ends <- as.numeric(strftime(end_date, "%j"))
    } else {
      ends <- as.numeric(strftime(paste0(y, "-12-31"), "%j")) 
    }
    
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east",  vals = as.numeric(sitelon), longname = "station_longitude")
    
    # ----- put values to nc_var list   
    nc_var <- list()
    for(j in seq_along(out_list)){
      rflag   <- ed.res.flag[j]
      fcnx    <- paste0("put_", gsub("-", "", rflag), "_values")
      fcn     <- match.fun(fcnx)
      put_out <- fcn(yr = y, nc_var = nc_var, out = out_list[[rflag]], lat = lat, lon = lon, 
                     begins = begins, ends = ends, pft_names)
      
      nc_var            <- put_out$nc_var
      out_list[[rflag]] <- put_out$out
    }
    
    # SLZ specific hack until I figure that out
    if(!is.null(out_list[["-E-"]]$SLZ)){
      out_list[["-E-"]]$SLZ <- NULL
    }
    
    # ----- write ncdf files
    
    print("*** Writing netCDF file ***")
    
    out <- unlist(out_list, recursive = FALSE)
    nc <- ncdf4::nc_create(outfile, nc_var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (VAR in names(nc_var)) {
      ncdf4::ncvar_put(nc, nc_var[[VAR]], out_list[["E"]][[VAR]])
      cat(paste(nc_var[[VAR]]$name, nc_var[[VAR]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
    rm(nc, varfile)
    
  } # end year-loop
  
  
} # model2netcdf.ED2
##-------------------------------------------------------------------------------------------------#

##-------------------------------------------------------------------------------------------------#

##' Function for reading -I- files
##'
##' @details
##'  e.g.    yr = 1999
##'      yfiles = 1999 2000
##'      tfiles = "analysis-I-1999-00-00-000000-g01.h5" "analysis-I-2000-00-00-000000-g01.h5"
##'
##' @param yr the year being processed
##' @param yfiles the years on the filenames, will be used to matched tfiles for that year
##' @export
read_E_files <- function(yr, yfiles, tfiles, outdir, start_date, end_date, ...){
  
  print(paste0("*** Reading -E- file ***"))
  PFTs <- data.frame(Name=c("C4 grass", "Early tropical", "Mid Tropical", "Late tropical", "Temperate C3 Grass", "North Pine", "South Pine", "Late conifer", "Early hardwood", "Mid hardwood", "Late hardwood", "C3 crop", "C3 pasture", "C4 crop", "C4 pasture", "C3 grass", "Araucaria"))
  disturbance <- data.frame(Type=c("Clear cut (crop/pasture)", "forest planation", "tree fall", "fire", "forest regrowth", "logged forest"))
  # 1 = clear cut (crop & pasture)
  # 2 = forest plantation
  # 3 = tree fall
  # 4 = fire
  # 5 = forest regrowth
  # 6 = logged forest
  
  
  # add
  add <- function(dat, var.name) {
    
    dims <- dim(dat)
    
    if (is.null(dims)) {
      if (length(dat) == 1) {
        if (!var.name %in% names(out)) {
          out[[var.name]] <- array(dat, dim = 1)
        } else {
          out[[var.name]] <- abind::abind(out[[var.name]], array(dat, dim = 1), along = 1)
        }
      } else {
        warning("expected a single value")
      }
    } else if(length(dims)==1){
      if(!var.name %in% names(out)){
        out[[var.name]] <- array(dat)
      } else {
        # Check to see if we need to add rows so we can store cohort-level data
        if(dim(out[[var.name]])[1] == length(dat)){
          out[[var.name]] <- cbind(out[[var.name]], array(dat))
        } else {
          row.fill <- abs(length(dat) - nrow(out[[var.name]]))
          if(nrow(out[[var.name]])>length(dat)){
            dat.fill <- array(0, dim=row.fill)
            out[[var.name]] <- cbind(out[[var.name]], c(array(dat), dat.fill))
          } else {
            if(length(dim(out[[var.name]]))==1){
              dat.fill <- array(0, dim=row.fill)
              out[[var.name]] <- cbind(c(out[[var.name]], dat.fill), array(dat))
            } else {  
              col.fill <- ncol(out[[var.name]])
              dat.fill <- array(0, dim=c(row.fill, col.fill))
              out[[var.name]] <- cbind(rbind(out[[var.name]], dat.fill), array(dat))
            }
          }
          

        }
        
      }
    } else if (length(dims)==2) {
      dat <- t(dat)
      dims <- dim(dat)
      # dat <- dat[1:(nrow(dat)), ]
      if (! var.name %in% names(out)) {
        out[[var.name]] <- dat
      } else {
        out[[var.name]] <- abind::abind(out[[var.name]], dat, along = 1)
      }
    } else {
      stop("can't handle arrays with >2 dimensions yet")
    }
    return(out)
  
    ## finally make sure we use -999 for invalid values
    out[[var.name]][is.null(out[[var.name]])] <- -999
    out[[var.name]][is.na(out[[var.name]])] <- -999
    
    return(out)
  } # end add-function
  
  
  getHdf5Data <- function(nc, var) {
    if (var %in% names(nc$var)) {
      return(ncdf4::ncvar_get(nc, var))
    } else {
      warning("Could not find", var, "in ed hdf5 output.")
      return(-999)
    }
  }
  

  # note that there is always one Iower file per year
  ysel <- which(yr == yfiles)
  
  if (yr < strftime(start_date, "%Y")) {
    warning(yr, "<", strftime(start_date, "%Y"))
    next
  }
  
  if (yr > strftime(end_date, "%Y")) {
    warning(yr, ">", strftime(end_date, "%Y"))
    next
  }
  
  n <- length(ysel)
  out <- list()
  row <- 1
  
  out[["PFT_Name"]] <- PFTs$Name
  out[["Disturbance_Name"]] <- disturbance$Type
  
  # note that there is always one Tower file per year
  # pb <- txtProgressBar(min=0, max=length(ysel), style=3)
  for(i in seq_along(ysel)){
    # setTxtProgressBar(pb, i)
    ncT <- ncdf4::nc_open(file.path(outdir, tfiles[ysel[i]]))
    
    ## determine timestep from HDF5 file
    # block <- 60*60 # We presecribed 1 hour
    # block <- ncT$dim$phony_dim_0$len
    block=1
    dat.blank <- array(rep(-999, block)) # This is a little different from Pecan, but makes sure everything has same dims
    
    # block = 86400/(60*60)
    # PEcAn.logger::logger.info(paste0("Output interval: ", 86400/block, " sec"))
    
    
    if ("SLZ" %in% names(ncT$var)) {
      slzdata <- getHdf5Data(ncT, "SLZ")
    } else {
      warning("Could not find SLZ in Y file, making a crude assumpution.")
      slzdata <- array(c(-2, -1.5, -1, -0.8, -0.6, -0.4, -0.2, -0.1, -0.05))
    }
    
    ## Check for which version of ED2 we are using.
    # ED2vc <- CheckED2Version(ncT)
    
    ## store for later use, will only use last data
    dz <- diff(slzdata)
    dz <- dz[dz != 0]
    
    # --------------------------
    # Site-Scale drivers (1 per timestep)
    # --------------------------
    # -------
    # Drivers
    # -------
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_CO2_PY"), "CO2air")  ## CO2air
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_RLONG_PY"), "LWdown")  ## Lwdown
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_PRSS_PY"), "Psurf")  ## Psurf
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_SHV_PY"), "Qair")  ## Qair
    out <- add(getHdf5Data(ncT, "MMEAN_PCPG_PY"), "Rainf")  ## Rainf
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_PAR_PY"), "SWdown")  ## Swdown
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_TEMP_PY"), "Tair")  ## Tair
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_VELS_PY"), "Wind")  ## Wind
    out <- add(getHdf5Data(ncT, 'MMEAN_ATM_RLONG_PY')-getHdf5Data(ncT, 'MMEAN_RLONGUP_PY'), "LWnet") ## Lwnet
    # -------
    
    # -------
    # Site-Level Disturbance information
    # -------
    # Site-level
    out <-  add(getHdf5Data(ncT, "IGNITION_RATE")*12, "Fire_flux")  ## Fire_flux -- *12 = per month to per year
    # -------
    
    # -------
    # Site-Level Carbon & Water Fluxes
    # -------
    out$SLZ <- slzdata
    
    out <- add(getHdf5Data(ncT, "MMEAN_PLRESP_PY"), "AutoResp")  ## AutoResp
    out <- add(getHdf5Data(ncT, "MMEAN_CAN_CO2_PY"), "CO2CAS")  ## CO2CAS
    out <- add(getHdf5Data(ncT, "MMEAN_GPP_PY"), "GPP")  ## GPP
    out <- add(getHdf5Data(ncT, "MMEAN_RH_PY"), "HeteroResp")  ## HeteroResp
    out <- add(-getHdf5Data(ncT, "MMEAN_GPP_PY") + getHdf5Data(ncT, "MMEAN_PLRESP_PY") + 
                 getHdf5Data(ncT, "MMEAN_RH_PY"), "NEE")  ## NEE
    out <- add(getHdf5Data(ncT, "MMEAN_GPP_PY") - getHdf5Data(ncT, "MMEAN_PLRESP_PY"), "NPP")  ## NPP
    out <- add(getHdf5Data(ncT, "MMEAN_RH_PY") + getHdf5Data(ncT, "MMEAN_PLRESP_PY"), "TotalResp")  ## TotalResp
    
    out <- add(getHdf5Data(ncT, "MMEAN_TRANSP_PY"), "Tveg")  ## Tveg
    out <- add(getHdf5Data(ncT, "MMEAN_VAPOR_LC_PY") + getHdf5Data(ncT, "MMEAN_VAPOR_WC_PY") + 
                 getHdf5Data(ncT, "MMEAN_VAPOR_GC_PY") + getHdf5Data(ncT, "MMEAN_TRANSP_PY"), "Evap")  ## Evap

    # Site-level variables related to water & carbon stress
    out <- add(getHdf5Data(ncT, "MMEAN_AVAILABLE_WATER_PY"), "WaterAvail")  ## WaterAvail=Available Water kg/m2
    out <- add(getHdf5Data(ncT, "MMEAN_A_CLOSED_PY"), "A_Closed")  ## Minimum assimilation rate; umol/m2l/s
    out <- add(getHdf5Data(ncT, "MMEAN_A_CO2_PY"), "A_CO2")  ## CO2-limited assimilation rate; umol/m2l/s
    out <- add(getHdf5Data(ncT, "MMEAN_A_LIGHT_PY"), "A_Light")  ## Light-limited assimilation rate; umol/m2l/s
    out <- add(getHdf5Data(ncT, "MMEAN_A_NET_PY"), "A_Net")  ## Actual assimilation rate; umol/m2l/s
    out <- add(getHdf5Data(ncT, "MMEAN_A_OPEN_PY"), "A_Open")  ##  Assimilation rate (no soil moist. stress); umol/m2l/s
    out <- add(getHdf5Data(ncT, "MMEAN_FSW_PY"), "MoistStress")  ## Moisture stress
    out <- add(getHdf5Data(ncT, "MMEAN_FS_OPEN_PY"), "NetStress")  ## Net stress factor
    out <- add(getHdf5Data(ncT, "MMEAN_PSI_OPEN_PY"), "Transp_StressMin")  ## Transpiration with no stress
    out <- add(getHdf5Data(ncT, "MMEAN_PSI_CLOSED_PY"), "Transp_StressMax")  ## Transpiration at maximum stress
    # -------
    
    # -------
    # Site-Level Soil Characteristics by depth
    # -------
    out <- add(getHdf5Data(ncT, "MMEAN_SOIL_WATER_PY"), "SoilMoist")  ## SoilWater  **********
    out <- add(getHdf5Data(ncT, "MMEAN_SOIL_TEMP_PY"), "SoilTemp")  ## SoilTemp
    out <- add(getHdf5Data(ncT, "MMEAN_SOIL_MSTPOT_PY"), "SoilMstPot")  ## SoilMstPot = Soil Matric Potential (m); not MsTMIP
    # -------
    
    # --------------------------
    
    
    
    # --------------------------
    # Patch-scale output
    # --------------------------
    # -------
    # Patch-level info
    # -------
    out <- add(getHdf5Data(ncT, "NPATCHES_GLOBAL"), "NPatch")  ## Number Patches
    out <- add(getHdf5Data(ncT, "PACO_ID"), "Patch_N_Cohort")  ## Number of cohorts in each patch
    out <- add(getHdf5Data(ncT, "AREA"), "Patch_Area")  ## Area of each Patches
  
    # Patch-level
    # out <- add(getHdf5Data(ncT, "DISTURBANCE_RATES"), "Disturb_Rate") # Disturbance matrix to/from
    out <- add(getHdf5Data(ncT, "DIST_TYPE"), "Disturb_Type") # Disturbance type per patch
    out <- add(getHdf5Data(ncT, "AVG_MONTHLY_WATERDEF"), "WaterDef") # Average Water Deficit; kg/m2 (Fire ignit)
    # 1 = clear cut (crop & pasture)
    # 2 = forest plantation
    # 3 = tree fall
    # 4 = fire
    # 5 = forest regrowth
    # 6 = logged forest
    
    # -------
    # --------------------------
    
    # --------------------------
    # Cohort-scale output
    # --------------------------
    # -------
    # Setting up a bunch of information at the co-hort level
    # -------
    ncohort <- ncdf4::ncvar_get(ncT, "NCOHORTS_GLOBAL")
    patch.n <- ncdf4::ncvar_get(ncT, 'PACO_N') 
    patch.start <- ncdf4::ncvar_get(ncT, 'PACO_ID')
    patch.area <- ncdf4::ncvar_get(ncT, "AREA")
    nplant <- getHdf5Data(ncT, "NPLANT")

    patch.co <- array(vector(length=ncohort))
    patch.area.co <- vector(length=ncohort)
    for(p in 1:length(patch.start)){
      if(patch.n[p]<=0) next
      
      patch.co[patch.start[p]:(patch.start[p]+patch.n[p]-1)] <- p
      patch.area.co[patch.start[p]:(patch.start[p]+patch.n[p]-1)] <- patch.area[p]
    }
    
    out <- add(nplant, "Cohort_Density") # Density for each cohort; stems/m2; need to weight by area
    out <- add(getHdf5Data(ncT, "PFT"), "Cohort_PFT") # PFT ID for each COHORT
    out <- add(patch.co, "Cohort_PatchID") # Patch ID for each cohort
    # -------


    # -------
    # PFT-Level Veg information
    # -------
    # Basics:
    out <- add(getHdf5Data(ncT, "DBH"), "Cohort_DBH") #cm
    out <- add(getHdf5Data(ncT, "BA_CO"), "Cohort_BasalArea") # cm2/m2
    out <- add(getHdf5Data(ncT, "AGB_CO"), "Cohort_AbvGrndBiom") #kgC/m2
    out <- add(getHdf5Data(ncT, "BALIVE"), "Cohort_TotLivBiom") # kgC /m2
    out <- add(getHdf5Data(ncT, "BDEAD"), "Cohort_TotDeadBiom")
    out <- add(getHdf5Data(ncT, "LAI_CO"), "Cohort_LAI")
    out <- add(getHdf5Data(ncT, "HITE"), "Cohort_Height") # Height in m

    # Water Stress
    # out <- add(array(getHdf5Data(ncT, "CB")[13,]), "Cohort_CB") # kgC/pl
    # out <- add(array(getHdf5Data(ncT, "CB_MOISTMAX")[13,]), "Cohort_CB_MoistMax") # kgC/pl
    # out <- add(array(getHdf5Data(ncT, "CB_LIGHTMAX")[13,]), "Cohort_CB_LightMax") # kgC/pl
    # out <- add(array(getHdf5Data(ncT, "CB_MLMAX")[13,]), "Cohort_CB_MLMax") # kgC/pl
    out <- add(getHdf5Data(ncT, "CBR_BAR"), "Cohort_CB_MeanRel") # kgC/pl
    # CB= CarbonBalance
    # CB_MoistMax = Carbon Balance with Full water availability
    # CB_LightMax = Carbon Balance with Full light availability
    # -------
    # --------------------------

    ncdf4::nc_close(ncT)
    rm(ncT)
    
  }
  
  return(out)
  
} # read_I_files


##-------------------------------------------------------------------------------------------------#

##' Function for put -I- values to nc_var list
##' @export
put_E_values <- function(yr, nc_var, out, lat, lon, begins, ends, ...){
  
  s <- length(nc_var)
  
  ## Conversion factor for umol C -> kg C
  Mc <- 12.017  #molar mass of C, g/mol
  umol2kg_C <- Mc * udunits2::ud.convert(1, "umol", "mol") * udunits2::ud.convert(1, "g", "kg")
  yr2s      <- udunits2::ud.convert(1, "s", "yr")
  pl2m2 <- out$Cohort_Density
  
  # TODO - remove this function and replace with ifelse statements inline below (SPS)
  conversion <- function(var.name, mult) {
    ## make sure only to convert those values that are not -999
    out[[var.name]][out[[var.name]] != -999] <- out[[var.name]][out[[var.name]] != -999] * mult
    return(out)
  }
  
  checkTemp <- function(var.name) {
    out[[var.name]][out[[var.name]] == 0] <- -999
    return(out)
  }
  
  
  # ----- define ncdf dimensions
  
  t <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", yr, "-01-01 00:00:00"), 
                        vals = seq(begins, ends, length.out = length(out[["GPP"]])), 
                        calendar = "standard", unlim = TRUE)
  pch <- ncdf4::ncdim_def(name = "npatch", units = "ID", 
                          vals = seq(1:nrow(out[["Patch_Area"]])), 
                          unlim = TRUE)
  cht <- ncdf4::ncdim_def(name = "ncohort", units = "ID", 
                          vals = seq(1:nrow(out[["Cohort_PFT"]])), 
                          unlim = TRUE)
  pft <- ncdf4::ncdim_def(name="pft", "", 1:length(out[["PFT_Name"]]),
                   longname = "Plant Functional Type", create_dimvar=FALSE)                 
  disturb <- ncdf4::ncdim_def(name = "disturbance", units = "", 
                              vals = seq(1:length(out[["Disturbance_Type"]])), 
                              create_dimvar=FALSE)
  dim.string <- ncdf4::ncdim_def("names", "", 1:48, create_dimvar=FALSE)
  
  
  slzdata <- out$SLZ
  dz <- diff(slzdata)
  dz <- dz[dz != 0]
  
  zg <- ncdf4::ncdim_def("SoilLayerMidpoint", "meters", c(slzdata[1:length(dz)] + dz/2, 0))
  
  dims  <- list(lon = lon, lat = lat, time = t)
  dimsz <- list(lon = lon, lat = lat, time = t, nsoil = zg)
  dimsp <- list(lon = lon, lat = lat, time = t, npatch = pch)
  dimsc <- list(lon = lon, lat = lat, time = t, ncohort = cht)
  dimsd <- list(lon = lon, lat = lat, time = t, ndisturb = disturb)
  
  # ----- fill list
  
  nc_var[["PFT_Name"]] <- ncdf4::ncvar_def("PFT_name", units = "string", dim = list(dim.string, pft), prec="char",
                                         longname = "Plant Functional Type name list")
  
  nc_var[["Disturbance_Name"]] <- ncdf4::ncvar_def("Disturbance_Type", units = "string", dim = list(dim.string, disturb), 
                                                   prec="char", longname = "Disturbance Type name list")
  
  # -------
  # Drivers
  # -------
  nc_var[["CO2air"]] <- ncdf4::ncvar_def("CO2air", units = "umol mol-1", dim = list(lon, lat, t), missval = -999, 
                                         longname = "Near surface CO2 concentration")
  nc_var[["LWdown"]] <- ncdf4::ncvar_def("LWdown", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                         longname = "Surface incident longwave radiation")
  nc_var[["Psurf"]] <- ncdf4::ncvar_def("Psurf", units = "Pa", dim = list(lon, lat, t), missval = -999, 
                                        longname = "Surface pressure")
  nc_var[["Qair"]] <- ncdf4::ncvar_def("Qair", units = "kg kg-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Near surface specific humidity")
  nc_var[["Rainf"]] <- ncdf4::ncvar_def("Rainf", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                        longname = "Rainfall rate")
  nc_var[["SWdown"]] <- ncdf4::ncvar_def("SWdown", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                         longname = "Surface incident shortwave radiation")
  out <- checkTemp("Tair")
  nc_var[["Tair"]] <- ncdf4::ncvar_def("Tair", units = "K", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Near surface air temperature")
  nc_var[["Wind"]] <- ncdf4::ncvar_def("Wind", units = "m s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Near surface module of the wind")
  nc_var[["LWnet"]] <- ncdf4::ncvar_def("LWnet", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                        longname = "Net Longwave Radiation")
  # -------
  
  # -------
  # Site-Level Disturbance information
  # -------
  out <- conversion("Fire_flux", yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[["Fire_flux"]] <- ncdf4::ncvar_def("Fire_flux", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                        longname = "Fire emissions")
  # -------
  
  # -------
  # Site-Level Carbon & Water Fluxes
  # -------
  # Standard Carbon Fluxes
  out <- conversion("AutoResp", umol2kg_C)  ## umol/m2 s-1 -> kg/m2 s-1
  nc_var[["AutoResp"]] <- ncdf4::ncvar_def("AutoResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                           longname = "Autotrophic Respiration")
  nc_var[["CO2CAS"]] <- ncdf4::ncvar_def("CO2CAS", units = "ppmv", dim = list(lon, lat, t), missval = -999, 
                                         longname = "CO2CAS")
  nc_var[["GPP"]] <- ncdf4::ncvar_def("GPP", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Gross Primary Productivity")
  out <- conversion("HeteroResp", yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[["HeteroResp"]] <- ncdf4::ncvar_def("HeteroResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                             longname = "Heterotrophic Respiration")
  out <- conversion("NEE", yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[["NEE"]] <-  ncdf4::ncvar_def("NEE", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Net Ecosystem Exchange")
  out <- conversion("NPP", yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[["NPP"]] <- ncdf4::ncvar_def("NPP", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Net Primary Productivity")
  out <- conversion("TotalResp", yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[["TotalResp"]] <- ncdf4::ncvar_def("TotalResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                            longname = "Total Respiration")
  
  # Water Fluxes
  nc_var[["Tveg"]] <- ncdf4::ncvar_def("TVeg", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Transpiration")
  nc_var[["Evap"]] <- ncdf4::ncvar_def("Evap", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Total Evaporation")
  
  # Non-Standard Carbon Fluxes & Stress Measures
  nc_var[["WaterAvail"]] <- ncdf4::ncvar_def("WaterAvail", units = "kg m-2", dim = list(lon, lat, t), missval = -999, 
                                             longname = "Available Water")
  
  out <- conversion("A_Closed", umol2kg_C)
  nc_var[["A_Closed"]] <- ncdf4::ncvar_def("A_Closed", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                           longname = "Minimum assimilation rate")
  out <- conversion("A_CO2", umol2kg_C)
  nc_var[["A_CO2"]] <- ncdf4::ncvar_def("A_CO2", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                        longname = "CO2-limited assimilation rate")
  out <- conversion("A_Light", umol2kg_C)
  nc_var[["A_Light"]] <- ncdf4::ncvar_def("A_Light", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                          longname = "Light-limited assimilation rate")
  out <- conversion("A_Net", umol2kg_C)
  nc_var[["A_Net"]] <- ncdf4::ncvar_def("A_Net", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                        longname = "Acutal net assimilation rate")
  out <- conversion("A_Open", umol2kg_C)
  nc_var[["A_Open"]] <- ncdf4::ncvar_def("A_Open", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                         longname = "Non-moisture limited assimilation rate")
  nc_var[["Tveg"]] <- ncdf4::ncvar_def("TVeg", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Transpiration")
  nc_var[["MoistStress"]] <- ncdf4::ncvar_def("MoistStress", units = "fraction; 1 = no stress", dim = list(lon, lat, t), 
                                              missval = -999, 
                                              longname = "Moisture Stress Factor; 1 = no stress")
  nc_var[["NetStress"]] <- ncdf4::ncvar_def("NetStress", units = "fraction; 1 = no stress", dim = list(lon, lat, t), 
                                            missval = -999, 
                                            longname = "Net Stress factor; 1 = no stress")
  nc_var[["Transp_StressMin"]] <- ncdf4::ncvar_def("Transp_StressMin", units = "kg m-2 s-1", dim = list(lon, lat, t), 
                                                   missval = -999, 
                                                   longname = "Transpiration with no stress")
  nc_var[["Transp_StressMax"]] <- ncdf4::ncvar_def("Transp_StressMax", units = "kg m-2 s-1", dim = list(lon, lat, t), 
                                                   missval = -999, 
                                                   longname = "Transpiration at maximum stress")
  # -------
  
  # -------
  # Site-Level Soil Characteristics by depth
  # -------
  nc_var[["SoilMoist"]] <- ncdf4::ncvar_def("SoilMoist", units = "kg m-2", dim = list(lon, lat, zg, t), missval = -999, 
                                            longname = "Average Layer Soil Moisture")
  out <- checkTemp("SoilTemp")
  nc_var[["SoilTemp"]] <- ncdf4::ncvar_def("SoilTemp", units = "K", dim = list(lon, lat, zg, t), missval = -999, 
                                           longname = "Average Layer Soil Temperature")
  nc_var[["SoilMstPot"]] <- ncdf4::ncvar_def("SoilMstPot", units = "m", dim = list(lon, lat, zg, t), missval = -999, 
                                           longname = "Average Layer Soil Matric Potential")
  # -------

  # -------
  # Patch-scale output
  # -------
  nc_var[["NPatch"]] <- ncdf4::ncvar_def("NPatch", units = "count", dim = list(lon, lat, t), missval = -999, 
                                             longname = "Total Number of Patches at each time")
  nc_var[["Patch_N_Cohort"]] <- ncdf4::ncvar_def("Patch_N_Cohort", units = "count", dim = list(lon, lat, pch, t), missval = -999, 
                                         longname = "Number of cohorts in each patch")
  nc_var[["Patch_Area"]] <- ncdf4::ncvar_def("Patch_Area", units = "fraction", dim = list(lon, lat, pch, t), missval = -999, 
                                                 longname = "fraction of total area in patch")
  # nc_var[["Disturb_Rate"]] <- ncdf4::ncvar_def("Disturb_Rate", units = "fraction", dim = list(lon, lat, disturb, t), missval = -999, 
                                             # longname = "Disturbance Transiton Matrix")
  nc_var[["Disturb_Type"]] <- ncdf4::ncvar_def("Disturb_Type", units = "ID", dim = list(lon, lat, pch, t), missval = -999, 
                                             longname = "Disturbance Identifier")
  nc_var[["WaterDef"]] <- ncdf4::ncvar_def("WaterDef", units = "kg/m2", dim = list(lon, lat, pch, t), missval = -999, 
                                             longname = "Average Water Deficit")
  # -------

  # -------
  # Cohort-scale output
  # -------
  nc_var[["Cohort_PatchID"]] <- ncdf4::ncvar_def("Cohort_PatchID", units = "ID", dim = list(lon, lat, cht, t), 
                                                 missval = -999, longname = "Cohort patch identifier")
  nc_var[["Cohort_PFT"]] <- ncdf4::ncvar_def("Cohort_PFT", units = "ID", dim = list(lon, lat, cht, t), 
                                                 missval = -999, longname = "Cohort plant functional type number")
  nc_var[["Cohort_Density"]] <- ncdf4::ncvar_def("Cohort_Density", units = "plants m-2", dim = list(lon, lat, cht, t),
                                                 missval = -999, longname = "Cohort plant desnity")
  nc_var[["Cohort_DBH"]] <- ncdf4::ncvar_def("Cohort_DBH", units = "cm", dim = list(lon, lat, cht, t), 
                                                    missval = -999, longname = "Cohort diameter at breast height")
  out <- conversion("Cohort_BasalArea", pl2m2)  ## cm2/plant -> cm2/m2
  nc_var[["Cohort_BasalArea"]] <- ncdf4::ncvar_def("TotLivBiom", units = "kg C m-2", dim = list(lon, lat, cht, t), 
                                                   missval = -999, longname = "Cohort basal area")
  
  out <- conversion("Cohort_AbvGrndBiom", pl2m2)  ## kgC/plant -> cm2/m2
  nc_var[["Cohort_AbvGrndBiom"]] <- ncdf4::ncvar_def("Cohort_AbvGrndBiom", units = "kg C m-2", dim = list(lon, lat, cht, t), 
                                                     missval = -999, longname = "Cohort above ground biomass")
  out <- conversion("Cohort_TotLivBiom", pl2m2)  ## kgC/plant -> cm2/m2
  nc_var[["Cohort_TotLivBiom"]] <- ncdf4::ncvar_def("Cohort_TotLivBiom", units = "kg C m-2", dim = list(lon, lat, cht, t), 
                                                    missval = -999, longname = "Cohort living biomass")
  out <- conversion("Cohort_TotDeadBiom", pl2m2)  ## kgC/plant -> cm2/m2
  nc_var[["Cohort_TotDeadBiom"]] <- ncdf4::ncvar_def("Cohort_TotDeadBiom", units = "kg C m-2", dim = list(lon, lat, cht, t), 
                                                     missval = -999, longname = "Total non-living biomass")
  nc_var[["Cohort_LAI"]] <- ncdf4::ncvar_def("Cohort_LAI", units = "m2 m-2", dim = list(lon, lat, cht, t), 
                                             missval = -999, longname = "Cohort Leaf Area Index")
  nc_var[["Cohort_Height"]] <- ncdf4::ncvar_def("Cohort_Height", units = "m", dim = list(lon, lat, cht, t), 
                                                missval = -999, longname = "Cohort Height")
  # out <- conversion("Cohort_CB", pl2m2)  ## kgC/plant -> cm2/m2
  # nc_var[["Cohort_CB"]] <- ncdf4::ncvar_def("Cohort_CB", units = "kg C m-2", dim = list(lon, lat, cht, t), 
  #                                           missval = -999, longname = "Monthly Carbon Balance")
  # out <- conversion("Cohort_CB_MoistMax", pl2m2)  ## kgC/plant -> cm2/m2
  # nc_var[["Cohort_CB_MoistMax"]] <- ncdf4::ncvar_def("Cohort_CB_MoistMax", units = "kg C m-2", dim = list(lon, lat, cht, t), 
  #                                                    missval = -999, 
  #                                                    longname = "Monthly Carbon Balance if no moisture limitation")
  # out <- conversion("Cohort_CB_LightMax", pl2m2)  ## kgC/plant -> cm2/m2
  # nc_var[["Cohort_CB_LightMax"]] <- ncdf4::ncvar_def("Cohort_CB_LightMax", units = "kg C m-2", dim = list(lon, lat, cht, t), 
  #                                                    missval = -999, 
  #                                                    longname = "Monthly Carbon Balance if no light limitation")
  # out <- conversion("Cohort_CB_MLMax", pl2m2)  ## kgC/plant -> cm2/m2
  # nc_var[["Cohort_CB_MLMax"]] <- ncdf4::ncvar_def("Cohort_CB_MLMax", units = "kg C m-2", dim = list(lon, lat, cht, t), 
  #                                                 missval = -999, 
  #                                                 longname = "Monthly Carbon Balance if no light or moisture limitation")
  nc_var[["Cohort_CB_MeanRel"]] <- ncdf4::ncvar_def("Cohort_CB_MeanRel", units = "", dim = list(lon, lat, cht, t), 
                                                    missval = -999, longname = "Relative carbon balance")
  
  # -------
  
  return(list(nc_var = nc_var, out = out))
  
} # put_E_values


##-------------------------------------------------------------------------------------------------#
