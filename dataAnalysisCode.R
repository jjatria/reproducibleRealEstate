################################################################################
#                                                                              #
#       Analyzing King County Sales Data                                       #
#                                                                              #
#       by Anonymous Real Estate Author                                        #
#                                                                              #
#       Code for Data Analysis                                                 #
#                                                                              #
################################################################################

### Load Libraries, Helper Files and Set Global Parameters ---------------------

  ## Load Libraries

  library(maptools)
  library(spdep)
  library(sp)
  library(rgeos)
  library(spam)

  ## Set location parameter (You must update these)

  CODE_PATH <- file.path('.')
  DATA_PATH <- file.path('.', 'data')
  VERBOSITY <- 1                       # Increase value for more output

  ## Source Files

  source(file.path(CODE_PATH, 'spatEconTools.R'))

  ### --------------------------------------------------------------------------
  if (VERBOSITY) message('Load in Prepared sales data')

  kcSales <- read.csv(file.path(DATA_PATH, 'cleansales.csv'), header = TRUE)

  ### --------------------------------------------------------------------------
  if (VERBOSITY)
    message('Remove remaining likely non Single Family Residential')

  # Remove multiple building sales
  kcSales <- subset(kcSales, nbrBldgs == 1)

  # Remove those with a present use code not of 2 or 29
  kcSales <- subset(kcSales, PresentUse == 2 | PresentUse == 29)

  # Remove those with more than one living unit
  kcSales <- subset(kcSales, NbrLivingUnits == 1)

  # Remove those with an improved assessed value of 0
  kcSales <- subset(kcSales, ImpsVal != 0)

  ### --------------------------------------------------------------------------
  if (VERBOSITY) message('Setting control variables')

  # Create a sales month variable
  kcSales$Month <- as.numeric(substr(kcSales$DocumentDate, 1, 2))

  # Create a waterfront Binary variable
  kcSales$WFNT <- ifelse(kcSales$WfntLocation > 0, 1, 0)

  # Sum up all bathrooms
  kcSales$Baths <- (
    kcSales$BathFullCount +
    kcSales$Bath3qtrCount * 0.75 +
    kcSales$BathHalfCount * 0.5
  )

  # Sum up all fireplaces
  kcSales$Fireplaces <- (
    kcSales$FpSingleStory  + kcSales$FpMultiStory +
    kcSales$FpFreestanding + kcSales$FpAdditional
  )

  # Create a binary variable for townhomes
  kcSales$Townhome <- ifelse(kcSales$PresentUse == 29, 1, 0)

  # Scale variables
  kcSales$lotAcres <- kcSales$SqFtLot / 43560
  kcSales$homeSize <- kcSales$SqFtTotLiving / 1000
  kcSales$Age <- 2014 - kcSales$YrBuilt

  if (VERBOSITY) message('Variables of interest (Views)')

  # Has Mountain View
  kcSales$viewMount <- ifelse(
    rowSums(
      cbind(kcSales$MtRainier, kcSales$Olympics, kcSales$Cascades)
    ) > 0, 1, 0
  )

  # Best Mountain View Rating
  kcSales$viewMountScore <- apply(
    cbind(kcSales$MtRainier, kcSales$Olympics, kcSales$Cascades), 1, max
  )

  # Has Water View
  kcSales$viewWater <- ifelse(
    rowSums(
      cbind(kcSales$PugetSound, kcSales$LakeWashington, kcSales$LakeSammamish)
    ) > 0, 1, 0
  )

  # Best Water View Rating
  kcSales$viewWaterScore <- apply(
    cbind(
      kcSales$PugetSound, kcSales$LakeWashington, kcSales$LakeSammamish
    ), 1, max
  )

  # Has Other View
  kcSales$viewOther <- ifelse(
    rowSums(
      cbind(
        kcSales$Territorial, kcSales$SeattleSkyline,
        kcSales$SmallLakeRiverCreek, kcSales$OtherView
      )
    ) > 0, 1, 0
  )

  # Best Water View Rating
  kcSales$viewOtherScore <- apply(
    cbind(
      kcSales$Territorial, kcSales$SeattleSkyline,
      kcSales$SmallLakeRiverCreek, kcSales$OtherView
    ), 1, max
  )

  # Multiple Views?
  kcSales$viewMult <- ifelse(
    rowSums(
      cbind(kcSales$viewMount, kcSales$viewWater, kcSales$viewOther)
    ) > 0, 1, 0
  )

  # Total view score across all categories
  kcSales$viewTotalScore <- rowSums(
    kcSales[,
      which(names(kcSales) == 'MtRainier'):which(names(kcSales) == 'OtherView')
    ]
  )

  ### --------------------------------------------------------------------------
  if (VERBOSITY) message('Creating trimmed set without outliers')

  # Remove sales with more than 2 acres of land
  trimSales <- subset(kcSales, SqFtLot < (43560 * 2))

  # Remove sales with more than 8 bedrooms
  trimSales <- subset(trimSales, Bedrooms < 8)

  # Remove sales with more than 8 bedrooms
  trimSales <- subset(trimSales, SqFtTotLiving >= 500 & SqFtTotLiving <= 8000)

  ### --------------------------------------------------------------------------
  if (VERBOSITY) message('Creating based model')

  modBase <- lm(
    log(SalePrice) ~ as.factor(Month) +
      lotAcres  + WFNT      + BldgGrade  + homeSize +
      Baths     + Age       + Fireplaces + Townhome +
      viewMount + viewWater + viewOther,
    data = trimSales
  )

  if (VERBOSITY) message('Test for spatial autocorrelation')

  salesSP <- SpatialPointsDataFrame(
    coords = cbind(trimSales$X, trimSales$Y),
    data   = trimSales
  )

  # Create the Spatial Weights Matrix

  swmAll10 <- createSWM(
    salesSP,
    10,
    nugget = 25
  )

  # Test for spatial autocorrelation in the error terms

  miAll <- moran.test(
    modBase$resid,
    swmAll10,
    zero.policy = TRUE
  )

  # Test for the type of spatial dependence

  lmAll <- lm.LMtests(
    modBase,
    swmAll10,
    test = c("LMerr", "LMlag", "RLMerr", "RLMlag")
  )

  ## Specify a Spatial Error Model

  # Estimate Model

  modSEM <- errorsarlm(
    as.formula(modBase),
    data = salesSP@data,
    swmAll10,
    method = "spam",
    zero.policy = TRUE
  )

  calcPseudoR2(modSEM)

  ### --------------------------------------------------------------------------
  if (VERBOSITY) message('Add differentiation based on view score')

  modBaseSc <- lm(
    log(SalePrice) ~ as.factor(Month) +
      lotAcres + WFNT + BldgGrade  + homeSize +
      Baths    + Age  + Fireplaces + Townhome +
      as.factor(viewMountScore) +
      as.factor(viewWaterScore) +
      as.factor(viewOtherScore),
    data = trimSales
  )

  ## Specify a Spatial Error Model

  # Estimate Model

  modSEMSc <- errorsarlm(
    as.formula(modBaseSc),
    data = salesSP@data,
    swmAll10,
    method = "spam",
    zero.policy = TRUE
  )

  calcPseudoR2(modSEMSc)
