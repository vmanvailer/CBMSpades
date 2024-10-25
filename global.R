######## Trials at setting up the Python environment in reticulate without
######## confusing where it looks for libcbm package

# Setup ========================================================================
## Setup folder to host SPADES project files -----------------------------------
projectPath <- "C:/Users/vmanvail/OneDrive - NRCan RNCan/Documents/^GitHub Projects/CBMSpades"

# Check if directory already exists, otherwise create new directory.
if(!dir.exists(projectPath)){
dir.create(projectPath, recursive = TRUE, showWarnings = FALSE)
}

setwd(projectPath)

## (Optional) Setup python version and virtual environment location ------------

### Check for compatible python version and install if needed ------------------
# Currently we are trying with 3.12
pyVerSimp <- "312"
pyVerDot <- "3.12"
pyPath <- grep(pyVerSimp, py_discover_config()$python_versions, value = TRUE)

if(length(pyPath) != 1){ # If no paths are returned install compatible version.

  message("No compatible version of python found.\nInstalling latest version of Python ", pyVerDot)
  pyVerIns <- paste0(pyVerDot, ":latest")
  install_python(version = pyVerIns,
                 force = FALSE)
}

### Check for existing virtual environment and create if needed ----------------
PyEnvName <- paste0("/PyEnv", pyVerSimp, "_CBMSPADES")
EnvDirs <- list.dirs(path.expand("~/.virtualenvs"), recursive = FALSE)

pyEnvPath <- grep(PyEnvName, EnvDirs, value = TRUE)

if (!reticulate::virtualenv_exists(pyEnvPath)){

  message(paste0("Creating new python environment ", PyEnvName, " on ", path.expand(virtualenv_root())))

  pyEnvPath <- paste0(path.expand("~/.virtualenvs"), PyEnvName)
  virtualenv_create(envname = pyEnv,
                    python = paste0("<=", pyVerDot))

} else {message("Python virtual environment ", pyEnvPath, " already exists.")}

py_versions_windows()
py_discover_config(use_environment = "C:/Users/vmanvail/OneDrive - NRCan RNCan/Documents/.virtualenvs/PyEnv312_CBMSPADES")
py_version()
py_config()

use_python()
use_python_version()
use_virtualenv()

reticulate::py_versions_windows()[["install_path"]][2]

venvPath <- "C:/Users/vmanvail/.virtualenvs/PyEnv312_CBMSPADES" # substitute on ret argument of setupProject()

## Install SPADES from Github repo ---------------------------------------------
repos <- unique(c("predictiveecology.r-universe.dev", getOption("repos")))
if (!require("SpaDES.project")) install.packages("SpaDES.project", repos = repos)

## Setup SPADES module ---------------------------------------------------------
# start in 1998 because there are known disturbances in the study area
times <- list(start = 1998, end = 2000)

out <- SpaDES.project::setupProject(
  Restart    = TRUE,
  useGit     = TRUE,    # a developer sets and keeps this = TRUE
  overwrite  = TRUE,    # a user who wants to get latest modules sets this to TRUE
  inputScott = "modules/spadesCBM/inputsForScott",
  paths      = list(projectPath = projectPath,
                    modulePath = "modules",
                    inputPath = "inputs"), # this will be replaced with updates CBM_dataPrep_SK and CBM_defaults
  options    = options(
    repos = c(repos = repos),
    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
    # PE = "https://predictiveecology.r-universe.dev/",    ## latest PredictievEcology packages
    #         SF = "https://r-spatial.r-universe.dev/",    ## latest sf and other spatial packages
    #         CRAN = "https://cloud.r-project.org"),
    reproducible.destinationPath = "inputs", ## TODO: SpaDES.project#24
    ## These are for speed
    reproducible.useMemoise      = TRUE,
    # Require.offlineMode        = TRUE,
    spades.moduleCodeChecks      = FALSE
  ),
  modules =  c("cboisvenue/spadesCBM@libCBMtransition",
               "PredictiveEcology/CBM_defaults@python",
               "PredictiveEcology/CBM_dataPrep_SK@python",
               "PredictiveEcology/CBM_vol2biomass@libcbm",
               "PredictiveEcology/CBM_core@python"),##TODO not linked yet!
  times = times,
  require = c("SpaDES.core", "reticulate",
              "PredictiveEcology/libcbmr", "data.table"),
  # params = list(
  #   CBM_core = list(
  #     .useCache = c(".inputObjects", "init")
  #     )),
  ret = {
    reticulate::use_virtualenv(virtualenv = venvPath)
    reticulate::py_install("libcbm", envname = basename(venvPath))
  },

  #### begin manually passed inputs ##########################################

  spatialDT = {
    dt <- readRDS(file.path(inputScott, "spatialDT.rds"))
    ##Transition: getting rid of the double gcids columns and naming one column
    ##gcids
    data.table::setnames(dt,"growth_curve_component_id", "gcids")
    dt[, growth_curve_id := NULL]
    dt
  },


  delays = rep(0, length(unique(spatialDT$pixelGroup))),

  userDist = data.table(distName = c("wildfire", "clearcut", "deforestation", "20% mortality", "20% mortality"),
                        rasterID = c(1L, 2L, 4L, 3L, 5L),
                        wholeStand = c(1L, 1L, 1L, 0L, 0L)),

  ##Need to keep this master raster here. It defines the smaller study area. We
  ##will not need it when we run all of the managed forests of SK as the study
  ##area will be defined by a masterRaster that we get via a URL.
  masterRaster = {
    extent = reproducible::.unwrap(structure(list(xmin = -687696, xmax = -681036,
                                                  ymin = 711955, ymax = 716183), class = "PackedSpatExtent"))
    masterRaster <- terra::rast(extent, res = 30)
    terra::crs(masterRaster) <- "PROJCRS[\"Lambert_Conformal_Conic_2SP\",
        BASEGEOGCRS[\"GCS_GRS_1980_IUGG_1980\",
            DATUM[\"D_unknown\",
                ELLIPSOID[\"GRS80\",6378137,298.257222101,
                    LENGTHUNIT[\"metre\",1,
                        ID[\"EPSG\",9001]]]],
            PRIMEM[\"Greenwich\",0,
                ANGLEUNIT[\"degree\",0.0174532925199433,
                    ID[\"EPSG\",9122]]]],
        CONVERSION[\"Lambert Conic Conformal (2SP)\",
            METHOD[\"Lambert Conic Conformal (2SP)\",
                ID[\"EPSG\",9802]],
            PARAMETER[\"Latitude of false origin\",49,
                ANGLEUNIT[\"degree\",0.0174532925199433],
                ID[\"EPSG\",8821]],
            PARAMETER[\"Longitude of false origin\",-95,
                ANGLEUNIT[\"degree\",0.0174532925199433],
                ID[\"EPSG\",8822]],
            PARAMETER[\"Latitude of 1st standard parallel\",49,
                ANGLEUNIT[\"degree\",0.0174532925199433],
                ID[\"EPSG\",8823]],
            PARAMETER[\"Latitude of 2nd standard parallel\",77,
                ANGLEUNIT[\"degree\",0.0174532925199433],
                ID[\"EPSG\",8824]],
            PARAMETER[\"Easting at false origin\",0,
                LENGTHUNIT[\"metre\",1],
                ID[\"EPSG\",8826]],
            PARAMETER[\"Northing at false origin\",0,
                LENGTHUNIT[\"metre\",1],
                ID[\"EPSG\",8827]]],
        CS[Cartesian,2],
            AXIS[\"easting\",east,
                ORDER[1],
                LENGTHUNIT[\"metre\",1,
                    ID[\"EPSG\",9001]]],
            AXIS[\"northing\",north,
                ORDER[2],
                LENGTHUNIT[\"metre\",1,
                    ID[\"EPSG\",9001]]]]"
    masterRaster[] <- rep(1, terra::ncell(masterRaster))
    mr <- reproducible::prepInputs(targetFile = file.path(inputScott, "ldSp_TestArea.tif"),
                                   destinationPath = ".",
                                   to = masterRaster,
                                   method = "near")
    mr[mr[] == 0] <- NA
    mr
  },
  ##Need to keep the disturbance rasters here. We will not need it when we run all
  ##of the managed forests of SK as the disturbance rasters will be defined by
  ##rasters that we get via a URL.
  disturbanceRasters = {
    rasts <- terra::rast(file.path(inputScott, paste0("SaskDist_", times$start:times$end, ".grd")))
    names(rasts) <- times$start:times$end
    rasts <- reproducible::postProcessTo(rasts, cropTo = masterRaster, projectTo = masterRaster,
                                         maskTo = masterRaster, method = "near")
  },

  # Restart = getOption("SpaDES.project.Restart", FALSE),

  outputs = as.data.frame(expand.grid(objectName = c("cbmPools", "NPP"),
                                      saveTime = sort(c(times$start,
                                                        times$start +
                                                          c(1:(times$end - times$start))
                                      )))),
  updateRprofile = TRUE# ,

)
out$modules <- out$modules[grep("spadesCBM", invert = TRUE, out$modules)] # remove spadesCBM as it is not a true module
out$loadOrder <- unlist(out$modules)

# Simulate =====================================================================
simPython <- do.call(SpaDES.core::simInitAndSpades, out)
