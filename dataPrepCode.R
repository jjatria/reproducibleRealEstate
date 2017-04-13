################################################################################
#                                                                              #
#   This code creates the initial raw sales file to use in analyzing 2014      #
#   King County Residential Sales data                                         #
#                                                                              #
################################################################################

### Preliminary Commands -------------------------------------------------------

  ## Load Libraries

  library(RODBC)
  library(RSQLite)
  library(DBI)
  library(stringr)
  library(maptools)
  library(plyr)

  ## Set global values

  CODE_PATH    <- file.path('.')
  DATA_PATH    <- file.path('.', 'data')
  STUDY_YEAR   <- 2014
  VERBOSITY    <- 1                       # Increase value for more output
  CONVERT_DATA <- FALSE

  ## Read in Functions and helper files

  source(file.path(CODE_PATH, 'basicConversionTools.R'))
  source(file.path(CODE_PATH, 'kingDataDevelop.R'))
  source(file.path(CODE_PATH, 'kingBuildSales.R'))

  ## Set Parameters

### Converting data from .csv to .db -------------------------------------------

  if (CONVERT_DATA) {

    if (VERBOSITY) message("Convert Assessor's Characteristic Data")

    convertCSVtoSQLite(
      dataPathCurrent = DATA_PATH,
      dataPathNew     = DATA_PATH,
      newFileName     = paste0('KingData', STUDY_YEAR, '.db'),
      fileNames       = c('EXTR_Parcel.csv', 'EXTR_ResBldg.csv'),
      overWrite       = TRUE,
      verbose         = VERBOSITY - 1,
      tableNames = c(
        paste0('Parcel', STUDY_YEAR),
        paste0('ResBldg', STUDY_YEAR)
      )
    );

    if (VERBOSITY > 1) message('Convert Assessed Value History')

    convertCSVtoSQLite(
      dataPathCurrent = DATA_PATH,
      dataPathNew     = DATA_PATH,
      newFileName     = 'KingValueHistory.db',
      fileNames       = c('EXTR_ValueHistory_V.csv'),
      tableNames      = c('ValueHistory'),
      overWrite       = TRUE,
      verbose         = VERBOSITY - 1
    );


    if (VERBOSITY > 1) message('Isolate study year in Assessed Value File')

    kngBuildAssdVal(
      avYears   = STUDY_YEAR + 1,
      assdValDB = file.path(DATA_PATH, 'KingValueHistory.db'),
      overWrite = TRUE,
      verbose   = VERBOSITY - 1
    );

    if (VERBOSITY > 1) message('Convert sales file');

    convertCSVtoSQLite(
      dataPathCurrent = DATA_PATH,
      dataPathNew     = DATA_PATH,
      newFileName     = 'KingSales.db',
      fileNames       = c('EXTR_RPSale.csv'),
      tableNames      = c('AllSales'),
      overWrite       = TRUE,
      verbose         = VERBOSITY - 1
    );

    if (VERBOSITY) message('Finished data conversion')

  }

### Initial cleaning and combining ---------------------------------------------

  if (VERBOSITY)
    message('Remove possible non-arms length transactions (ie. with labels)')

  kngSCleanSales(
    saleYears = STUDY_YEAR,
    transLimit = 10,
    salesDB = file.path(DATA_PATH, 'KingSales.db'),
    trimList=list(
      SaleReason     = 2:19,
      SaleInstrument = c(0, 1, 4:28),
      SaleWarning    = paste0(" ", c(
        1:2, 5:9, 11:14, 18:23, 25, 27, 31:33, 37, 39,
        43, 46, 48, 49, 50:53, 59, 61, 63, 64, 66
      ), " ")
    ),
    overWrite = TRUE,
    verbose   = VERBOSITY - 1
  );

  if (VERBOSITY)
    message('Add Use category and limit to residential only sales')

  kngSLabelSales(
    saleYears = STUDY_YEAR,
    salesDB   = file.path(DATA_PATH, 'KingSales.db'),
    overWrite = TRUE,
    verbose   = VERBOSITY - 1
  )

  if (VERBOSITY) message('Remove multiple parcel sales')

  kngSConfirmLabels(
    salesDB    = file.path(DATA_PATH, 'KingSales.db'),
    latestYear = STUDY_YEAR,
    verbose    = VERBOSITY,
    overWrite  = TRUE
  )

  if (VERBOSITY)
    message('Add Parcel and Residential Building Information to the sales')

  kngSSplitAttachSales(
    salesDB   = file.path(DATA_PATH, 'KingSales.db'),
    verbose   = VERBOSITY,
    overWrite = TRUE
  )

  if (VERBOSITY) message('Add Assessed Values')

  xSales <-  kngSAttachAssdValues(
    salesDB   = file.path(DATA_PATH, 'KingSales.db'),
    dataDir   = DATA_PATH,
    dataYear  = STUDY_YEAR + 1,
    verbose   = VERBOSITY,
    overWrite = TRUE
  )

  if (VERBOSITY) message('Add Lat/Long to data')

  xSales <- kngSAttachXYs(
    xSales,
    latlongFile =
      file.path(DATA_PATH, paste0('parcelPoints', STUDY_YEAR, '.shp')),
    verbose = VERBOSITY - 1
  )

### ----------------------------------------------------------------------------

  if (VERBOSITY) message('Write out clean data')

  write.csv(
    xSales,
    file = file.path(DATA_PATH, 'cleansales.csv'),
    row.names = FALSE
  )

  if (VERBOSITY) message('Finished data preparation')
