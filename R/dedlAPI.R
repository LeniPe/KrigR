DEDL.token <- function(DEDL_User, DEDL_Pwd){
  reticulate::py_require('destinelab')
  destinelab <- reticulate::import('destinelab')

  auth = destinelab$AuthHandler(DEDL_User, DEDL_Pwd)
  access_token = auth$get_token()
  auth_headers <- httr::add_headers(Authorization = paste("Bearer", access_token))
  auth_headers
}

DEDL.httr_status_check <- function(API_request){
  if (httr::status_code(API_request) != 200) {
    stop(paste(
      "Request failed with status", httr::status_code(API_request),
      "\nMessage:", httr::content(API_request, as = "text", encoding = "UTF-8")
    ))
  }
}

DEDL.safe_get <- function(url, ..., max_retries = 5, retry_delay = 5) {
  attempt <- 1
  repeat {
    tryCatch({
      resp <- httr::GET(url, ...)
      DEDL.httr_status_check(resp)
      return(resp)
    }, error = function(e) {
      if (attempt >= max_retries) stop(e)
      message(sprintf("GET failed (attempt %d/%d): %s", attempt, max_retries, e$message))
      Sys.sleep(retry_delay * attempt)  # exponential backoff
      attempt <<- attempt + 1
    })
  }
}

DEDL.dataset_map <- function(cds_dataset){
  if(cds_dataset=='reanalysis-era5-land-monthly-means'){
    return("EO.ECMWF.DAT.ERA5_LAND_MONTHLY")
  }
  if(cds_dataset=='reanalysis-era5-land'){
    return('EO.ECMWF.DAT.ERA5_LAND_HOURLY')
  }
  stop(sprintf("Unknown DEDL mapping for CDS dataset '%s'", cds_dataset))
}

DEDL.order<- function(Requests_ls, API_Key, API_User, verbose = TRUE,
                      stac_url = 'https://hda.data.destination-earth.eu/stac/v2/') {

  auth_headers = DEDL.token(DEDL_User, DEDL_Pwd)

  for (requestID in 1:length(Requests_ls)) { ## looping over requests
    if (verbose) {
      print(names(Requests_ls)[requestID])
    }
    if (class(Requests_ls[[requestID]]) == "logical") {
      next()
    }
    request = Requests_ls[[requestID]]
    cds_dataset = request$dataset_short_name
    dedl_dataset = DEDL.dataset_map(cds_dataset)
    stac_order_url = paste0(stac_url, "collections/", dedl_dataset, "/order")

    body <- list(
      `ecmwf:variable` = request$variable,
      `ecmwf:month` = request$month,
      `ecmwf:year` = request$year,
      `ecmwf:download_format` = request$format,
      `ecmwf:product_type` = request$product_type,
      `ecmwf:area` = request$area,
      `ecmwf:time` = request$time
    )

    API_request <- httr::POST(
      url = stac_order_url,
      body = body,
      encode = "json",
      auth_headers
    )

    DEDL.httr_status_check(API_request)
    cat("Request URL: ", httr::url(API_request), "\n")
    cat("Request Body:\n")
    cat(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE), "\n")

    Requests_ls[[requestID]]$API_request <- API_request
  }
  Requests_ls
}

DEDL.download <- function(API_request, FNAME, DEDL_User, DEDL_Pwd){
  auth_headers = DEDL.token(DEDL_User, DEDL_Pwd)

  ordered_item <- httr::content(API_request, as = "parsed", simplifyVector = TRUE)
  self_url = ordered_item$links$href[ordered_item$links$rel == "self"]

  repeat {
    item_response <- DEDL.safe_get(self_url, auth_headers, max_retries = TryDown)
    DEDL.httr_status_check(item_response)
    item_data <- httr::content(item_response, as = "parsed", type = "application/json")

    # 2. Extract order status
    order_status <- item_data$properties[["order:status"]]
    cat("Order status:", order_status, "\n")

    # 3. Check conditions
    if (order_status == "succeeded") {
      cat("Order completed! Asset is now available for download.\n")
      break
    } else if (order_status == "failed") {
      stop("Order processing failed")
    }

    # 4. Wait 30 seconds before next check
    Sys.sleep(10)
  }

  asset_url <- item_data$assets$downloadLink$href

  file_response <- DEDL.safe_get(asset_url, auth_headers, httr::write_disk(FNAME, overwrite = TRUE), max_retries = TryDown)
  DEDL.httr_status_check(file_response)

  LoadTry <- tryCatch(rast(FNAME),
                      error = function(e) {
                        e
                      }
  )
  if (class(LoadTry)[1] == "simpleError") {
    file.rename(FNAME, paste0(FNAME, ".zip")) # make into zip
    extrazip <- unzip(paste0(FNAME, ".zip"), list = TRUE)$Name # find name of file in zip
    unzip(paste0(FNAME, ".zip"), exdir = dirname(FNAME))
    file.rename(
      file.path(dirname(FNAME), extrazip),
      file.path(dirname(FNAME), basename(FNAME))
    ) # make into zip
    unlink(paste0(FNAME, ".zip"))
    warning("CDS download seems to have produced a .zip file. KrigR has automatically extracted data from this file. This is currently an experimental fix.")
  }
}
