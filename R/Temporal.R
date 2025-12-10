### DATE REFORMATTING ==========================================================
#' Resolve time zones as requested by user and UTC format with which to query from CDS
#'
#' Create UTC counterparts of user-input dates for CDS queries
#'
#' @param DatesVec A vector of POSIXct objects
#'
#' @return A data frame on input dates respective to user-queried timezone and their UTC counterparts.
#'
#' @examples
#' IN_DateStart <- as.POSIXct("1995-01-01 00:00", tz = "CET")
#' IN_DateStop <- as.POSIXct("2005-01-01 23:00", tz = "CET")
#' Dates_df <- Make.UTC(DatesVec = c(IN_DateStart, IN_DateStop))
#' Dates_df
#'
#' @export
Make.UTC <- function(DatesVec = NULL) {
  data.frame(IN = DatesVec,
             UTC = do.call(c, lapply(DatesVec, FUN = function(x) {
               as.POSIXct(x, tz = "UTC")
             }))
  )
}

### Helper ====
complete_months_split <- function(DateStart, DateStop, TChunkSize, BaseTStep = 24){

  month_chunksize <- max(c(round(TChunkSize / 730),1)) # convert from number of hours per month to number of months

  months_vec <- seq(
    from = as.Date(format(DateStart, "%Y-%m-01")),
    to   = as.Date(format(DateStop, "%Y-%m-01")),
    by   = "month"
  )

  # Split into yearly buckets
  years <- unique(format(months_vec, "%Y"))
  year_list <- lapply(years, function(y) {
    months_vec[format(months_vec, "%Y") == y]
  })

  # --- Apply same logic for months as was done for years ----
  chunks_list <- lapply(year_list, function(months_in_year) {
    # identify each month's index in that year (1..12)
    month_index <- as.integer(format(months_in_year, "%m"))

    # assign each month to a chunk of TChunkSize months
    chunk_id <- ceiling(month_index / month_chunksize)

    # create month chunks
    split(months_in_year, chunk_id)
  })

  # flatten out
  QueryTimeWindows <- unlist(chunks_list, recursive = FALSE)

  # --- Expand each month to 24 * days(month) entries of first day-of-month ---
  QueryTimeWindows <- lapply(QueryTimeWindows, function(month_group) {

    x <- unlist(lapply(month_group, function(m) {
      # number of days in month m
      dim <- as.integer(format((m + months(1)) - 1, "%d"))

      # produce 24 * dim copies of first day of month, as Date
      rep(m, BaseTStep * dim)
    }))
   return(as.Date(x))
  })
  return(QueryTimeWindows)}

### QUERY SEPARATING INTO TIME WINDOWS =========================================
#' Creating time windows for CDS queries
#'
#' Make a list holding date ranges for which to make individual CDS queries
#'
#' @param Dates_df A two-column data frame (column names: "IN" and "UTC") holding POSIXct elements. Created with \code{\link{Make.UTC}}.
#' @param BaseTResolution Character. Base temporal resolution of queried data on CDS
#' @param BaseTStep Numeric. Base time steps of queried data on CDS
#' @param BaseTStart POSIXct. Base starting date and time of queried data on CDS
#' @param TChunkSize Numeric. Maximum amount of layers to include in each query
#'
#' @importFrom stringr str_pad
#' @importFrom stringr str_c
#'
#' @return List. Contains:
#' \itemize{
#' \item{QueryTimeWindows}{List of dates for individual CDS queries as used by \code{\link{Make.Request}}.}.
#' \item{QueryTimes}{Character. Layers of data in the raw data set}.
#' }
#'
#' @examples
#' IN_DateStart <- as.POSIXct("1995-01-01 00:00", tz = "CET")
#' IN_DateStop <- as.POSIXct("2005-01-01 23:00", tz = "CET")
#' Dates_df <- Make.UTC(DatesVec = c(IN_DateStart, IN_DateStop))
#' Make.RequestWindows(Dates_df = Dates_df,
#'                     BaseTResolution = "hour",
#'                     BaseTStep = 24
#'                     BaseTStart = as.POSIXct("1950-01-01 00:01", tz = "UTC")
#'                     TChunkSize = 12000)
#'
Make.RequestWindows <- function(Dates_df, BaseTResolution, BaseTStep, BaseTStart, TChunkSize) {
  # Normalize start / stop as POSIXct UTC (Dates_df$UTC expected)
  DateStart <- as.POSIXct(Dates_df$UTC[1], tz = "UTC")
  DateStop  <- as.POSIXct(Dates_df$UTC[2], tz = "UTC")

  # Basic validation of supported resolutions
  if (!(BaseTResolution %in% c("hour", "month"))) {
    stop("Make.RequestWindows: BaseTResolution must be 'hour' or 'month'.")
  }

  # TChunkSize must be a multiple of BaseTStep (so chunking produces whole number of layers)
  if ((TChunkSize / BaseTStep) %% 1 != 0) {
    stop(
      "Please specify a TChunkSize (currently = ", TChunkSize,
      ") that is a multiple of the base temporal subdivision (BaseTStep = ", BaseTStep, ")."
    )
  }

  # --- MONTH CASE -----------------------------------------------------------
  if (BaseTResolution == "month") {
    # Align DateStart to first day of its month and DateStop to first day of last month
    DateStart_mon <- as.POSIXct(format(DateStart, "%Y-%m-01 00:00:00"), tz = "UTC")
    # set stop to first of month of DateStop
    DateStop_mon  <- as.POSIXct(format(DateStop, "%Y-%m-01 00:00:00"), tz = "UTC")

    # sequence of months (one entry per month start)
    months_seq <- seq(from = DateStart_mon, to = DateStop_mon, by = "month")

    # Two sub-cases:
    # - BaseTStep == 1  -> single monthly layer per month (CDS "monthly_averaged_reanalysis" / moda)
    # - BaseTStep  > 1  -> multiple sub-month layers per month (e.g. 24 hour-of-day monthly means)
    if (BaseTStep == 1) {
      # One layer per month
      # T_RequestDates: one date per month (we use the first day as placeholder)
      T_RequestDates <- as.Date(format(months_seq, "%Y-%m-01"))
      # QueryTimes: single time (CDS expects "00:00")
      QueryTimes <- "00:00"
    } else {
      # Multiple sub-month layers per month (e.g. one per hour-of-day)
      # Represent each month by BaseTStep repeated entries (placeholder dates)
      # Each element in T_RequestDates will be the same month repeated BaseTStep times
      T_RequestDates <- as.Date(rep(format(months_seq, "%Y-%m-01"), each = BaseTStep))

      # Build QueryTimes as hour-of-day strings. If BaseTStep divides 24 evenly, produce symmetric hours.
      if (24 %% BaseTStep == 0) {
        # generate times equally spaced over 24h, most common case: BaseTStep == 24 -> 00..23
        step_hours <- seq(0, 24 - 24 / BaseTStep, by = 24 / BaseTStep)
        QueryTimes <- sprintf("%02d:00", as.integer(step_hours))
      } else {
        # fallback: produce BaseTStep hourly labels 00:00.. (0..BaseTStep-1)
        QueryTimes <- sprintf("%02d:00", 0:(BaseTStep - 1))
      }
    }

    # Now split into windows of length TChunkSize (TChunkSize counts layers, not months)
    QueryTimeWindows <- split(T_RequestDates, ceiling(seq_along(T_RequestDates) / TChunkSize))

    return(list(QueryTimeWindows = QueryTimeWindows, QueryTimes = QueryTimes))
  } # end month case

  # --- HOUR CASE (original logic) ------------------------------------------
  # For hourly base resolution, BaseTStep is expected to be e.g. 24/BaseStep (as your caller sets)
  # Build QueryTimes ("HH:MM") for the sub-daily product
  if (BaseTResolution == "hour") {
    if (BaseTStep == 24) {
      QueryTimes <- sprintf("%02d:00", 0:23)
    } else {
      # sequence of hours stepping by 24/BaseTStep (e.g. for 3-hourly: by = 3)
      hours_seq <- seq(from = as.numeric(format(BaseTStart, "%H")), to = 23, by = 24 / BaseTStep)
      QueryTimes <- sprintf("%02d:00", as.integer(hours_seq))
    }
  }

  QueryTimeWindows <- complete_months_split(DateStart, DateStop, TChunkSize, length(QueryTimes))

  # Build request date sequence for hourly case
  # T_RequestRange <- seq(from = DateStart, to = DateStop, by = BaseTResolution)
  # T_RequestDates <- as.Date(rep(unique(format(T_RequestRange, "%Y-%m-%d")), each = BaseTStep))
  #
  # QueryTimeWindows <- split(T_RequestDates, ceiling(seq_along(T_RequestDates) / TChunkSize))

  return(list(QueryTimeWindows = QueryTimeWindows, QueryTimes = QueryTimes))
}


### BACK-CALCULATION OF CUMULATIVE VARIABLES ===================================
#' Make cumulatively stored records into sequential ones
#'
#' Takes a SpatRaster of cumulatively stored records and returns a SpatRaster of sequential counterparts
#'
#' @param CDS_rast SpatRaster
#' @param CumulVar Logical. Whether to apply cumulative back-calculation
#' @param BaseResolution Character. Base temporal resolution of data set
#' @param BaseStep Numeric. Base time step of data set
#' @param Type CDS Dataset type 
#' @param TZone Character. Time zone for queried data.
#' @param verbose Logical. Whether to print/message function progress in console or not.
#'
#' @importFrom terra rast
#' @importFrom terra nlyr
#' @importFrom terra subset
#' @importFrom terra time
#' @importFrom lubridate days_in_month
#' @importFrom pbapply pblapply
#'
#' @return A SpatRaster
#'
Temporal.Cumul <- function(CDS_rast, CumulVar, BaseResolution, BaseStep, Type, TZone, verbose = TRUE) { # nolint: cyclocomp_linter.
  Era5_ras <- CDS_rast
  if (verbose && CumulVar) {
    print("Disaggregation of cumulative records")
  }
  if (CumulVar && BaseResolution == "hour") {
    if (BaseStep != 1) {
      stop("Back-calculation of hourly cumulative variables only supported for 1-hour interval data. The data you have specified reports hourly data in intervals of ", BaseStep, ".")
    }
    ## removing non-needed layers
    RemovalLyr <- c(1, (nlyr(Era5_ras) - 22):nlyr(Era5_ras)) # need to remove first layer and last 23 for backcalculation
    Era5_ras <- subset(Era5_ras, RemovalLyr, negate = TRUE)
    ## back-calculation
    #' break apart sequence by UTC days and apply back-calculation per day in pblapply loop, for loop for each hour in each day
    DataDays <- ceiling(1:nlyr(Era5_ras) / 24)
    DissagDays <- unique(DataDays)
    Era5_ls <- pblapply(DissagDays, FUN = function(DissagDay_Iter) {
      counter <- 1
      Interior_ras <- Era5_ras[[which(DataDays == DissagDay_Iter)]]
      Interior_ls <- as.list(rep(NA, nlyr(Interior_ras)))
      names(Interior_ls) <- terra::time(Interior_ras)
      for (i in 1:nlyr(Interior_ras)) {
        if (counter == 1) {
          Interior_ls[[i]] <- Interior_ras[[i]]
        }
        if (counter == 24) {
          Interior_ls[[i]] <- Interior_ras[[i]] - sum(rast(Interior_ls[1:(1 + counter - 2)]))
        }
        if (counter != 24 & counter != 1) {
          Interior_ls[[i]] <- Interior_ras[[i + 1]] - Interior_ras[[i]]
        }
        counter <- counter + 1
      }
      rast(Interior_ls)
    })
    ## finishing off object
    Ret_ras <- rast(Era5_ls)
    Era5_ras <- Ret_ras
    warning("You toggled on the CumulVar option in the function call. Hourly records have been converted from cumulative aggregates to individual hourly records.")
  }
  ## multiply by number of days per month
  if (CumulVar && BaseResolution == "month") {
    Days_in_Month_vec <- days_in_month(terra::time(CDS_rast))
    if (grepl("ensemble_members", Type)) {
      Days_in_Month_vec <- rep(Days_in_Month_vec, each = 10)
    }
    Era5_ras <- Era5_ras * Days_in_Month_vec
    warning("You toggled on the CumulVar option in the function call. Monthly records have been multiplied by the amount of days per respective month.")
  }
  return(Era5_ras)
}

### TEMPORAL AGGREGATION =======================================================
#' Carry out temporal aggregation
#'
#' Takes a SpatRaster and user-specifications of temporal aggregation and carries it out
#'
#' @param CDS_rast SpatRaster
#' @param BaseResolution Character. Base temporal resolution of data set
#' @param BaseStep Numeric. Base time step of data set
#' @param TResolution Character. User-specified temporal resolution
#' @param TStep Numeric. User-specified time step
#' @param FUN User-defined aggregation function
#' @param Cores Numeric. Number of cores for parallel processing
#' @param QueryTargetSteps Character. Target resolution steps
#' @param TZone Character. Time zone for queried data.
#' @param verbose Logical. Whether to print/message function progress in console or not.
#'
#' @importFrom terra time
#' @importFrom terra tapp
#' @importFrom terra app
#'
#' @return A SpatRaster
#'
Temporal.Aggr <- function(CDS_rast, BaseResolution, BaseStep,
                          TResolution, TStep, FUN, Cores, QueryTargetSteps, TZone, verbose = TRUE, aggregation_needed = FALSE) {
  if (verbose) message("Temporal Aggregation started")

  if (aggregation_needed) {
    if (verbose) message("Applying hourly → monthly aggregation")
    times <- terra::time(CDS_rast)
    month_index <- format(times, "%Y%m")
    AggrIndex <- match(month_index, unique(month_index))
  } else if (BaseResolution == TResolution && BaseStep == TStep) {
    if (verbose) message("No temporal aggregation required")
    return(CDS_rast) # no temporal aggregation needed
  } else {
    TimeDiff <- sapply(terra::time(CDS_rast), FUN = function(xDate) {
      length(seq(
        from = terra::time(CDS_rast)[1],
        to = xDate,
        by = TResolution
      )) - 1
    })
    AggrIndex <- floor(TimeDiff / TStep) + 1
  }

  Form <- substr(TResolution, 1, 1)
  Form <- ifelse(Form %in% c("h", "y"), toupper(Form), Form)
  LayerFormat <- format(terra::time(CDS_rast), paste0("%", Form))

  if (length(unique(AggrIndex)) == 1) { ## this is to avoid a warning message thrown by terra
    Final_rast <- app(
      x = CDS_rast,
      cores = Cores,
      fun = FUN
    )
  } else {
  # --- Assign timestamps to aggregated layers ---
  if (verbose) message("Assigning time dimension to aggregated raster")

  if (TResolution == "year") {
    terra::time(Final_rast) <- as.POSIXct(
      paste0(LayerFormat[!duplicated(AggrIndex)], "-01-01"),
      tz = TZone
    )
  }
  if (TResolution == "month") {
    terra::time(Final_rast) <- as.POSIXct(
      paste0(format(terra::time(CDS_rast)[!duplicated(AggrIndex)], "%Y-%m"), "-01"),
      tz = TZone
    )
  }
  if (TResolution == "day") {
    terra::time(Final_rast) <- as.POSIXct(
      format(terra::time(CDS_rast)[!duplicated(AggrIndex)], "%Y-%m-%d"),
      tz = TZone
    )
  }
  if (TResolution == "hour") {
    terra::time(Final_rast) <- as.POSIXct(
      terra::time(CDS_rast)[!duplicated(AggrIndex)],
      tz = TZone
    )
  }
  if (verbose) message("Temporal aggregation done")
  return(Final_rast)
}

### TEMPORAL AGGREGATION CHECK =================================================
#' Checking temporal aggregation can use all queried data
#'
#' Error message if specified aggregation and time window clash.
#'
#' @param QuerySeries Character. Vector of dates/times queried for download. Created by \code{\link{Make.RequestWindows}}.
#' @param DateStart UTC start date.
#' @param DateStop UTC stop date.
#' @param TResolution User-specified temporal resolution for aggregation.
#' @param BaseTResolution Dataset-specific native temporal resolution.
#' @param TStep User-specified time step for aggregation.
#' @param BaseTStep Dataset-specific native time step.
#' @param tz Character. Timezone of data

#' @return Character - target resolution formatted steps in data.
#'
TemporalAggregation.Check <- function(
    QuerySeries,
    DateStart,
    DateStop,
    TResolution,
    BaseTResolution,
    TStep,
    BaseTStep,
    tz
) {
  ## check clean division
  if (BaseTResolution == TResolution) { ## this comes into play for hourly aggregates of ensemble data
    if ((TStep / BaseTStep) %% 1 != 0) {
      stop("Your specified time range does not allow for a clean integration of your selected time steps. You specified a time series of raw data with a length of ", length(QueryTargetFormat), " (", BaseTResolution, " intervals of length ", BaseTStep, "). Applying your desired temporal aggregation of ", TResolution, " intervals of length ", TStep, " works out to ", round(TStep / BaseTStep, 3), " intervals. Please fix this so the specified time range can be cleanly divided into aggregation intervals.")
    }
    QueryTargetSteps <- paste("Ensembling at base resolution, Factor =", TStep / BaseTStep)
    return(QueryTargetSteps)
  }
  
  # limit query series to what will be retained
  QuerySeries <- QuerySeries[as.POSIXct(QuerySeries, tz = tz) >= DateStart & as.POSIXct(QuerySeries, tz = tz) <= DateStop]
  ## extract format of interest
  Form <- substr(TResolution, 1, 1)
  Form <- ifelse(Form %in% c("h", "y"), toupper(Form), Form)
  
  ## extract desired format
  QueryTargetFormat <- format(as.POSIXct(QuerySeries, tz = tz), paste0("%", Form))
  QueryTargetSteps <- unique(QueryTargetFormat)
  
  if ((length(QueryTargetSteps) / TStep) %% 1 != 0) {
    stop("Your specified time range does not allow for a clean integration of your selected time steps. You specified a time series of raw data with a length of ", length(QueryTargetFormat), " (", BaseTResolution, " intervals of length ", BaseTStep, "). Applying your desired temporal aggregation of ", TResolution, " intervals of length ", TStep, " works out to ", round(length(QueryTargetSteps) / TStep, 3), " intervals. Please fix this so the specified time range can be cleanly divided into aggregation intervals.")
  }
  return(QueryTargetSteps)
}