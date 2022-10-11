# Set up ------------------------------------------------------------------

library(httr)
library(jsonlite)
library(magrittr)
library(readxl)

## key funcrtions

#' Review an API response
#'
#' Check that API returned a `JSON` element without any errors.
#'
#' @param res A response object.
#' @importFrom httr http_type http_error content status_code
#' @importFrom jsonlite fromJSON
check_for_response <- function(res = NULL) {
  if (!is.null(res)) {
    if (http_type(res) != "application/json") {
      stop("kobotoolbox did not return json", call. = F)
    }
    parsed <- fromJSON(content(res, "text"), simplifyVector = F)
    if (http_error(res)) {
      stop(
        sprintf(
          "kobotoolbox request failed [%s] \n%s\n<%s>",
          status_code(res),
          parsed$message,
          parsed$url
        ),
        call. = F
      )
    }
  }
}


# load the configuration options
config <- config::get()

# Get AC Kobo Data by Facilities ------------------------------------------

#' read all the access collaborative facilities 
ac_facilities <- readxl::read_xlsx(paste0("./data/",config$mapping_file),
                                   sheet = "273 facilities")

# break the data.frame into a list of 10 facilities (max)
ac_facilities <- split(ac_facilities, rep(1:ceiling(nrow(ac_facilities)/10), each = 10)[1:nrow(ac_facilities)])

## construct the api queries
ac_facilities_api_query <- purrr::map_chr(ac_facilities, function(x){
  query <- paste0('?query={"site_selection/facility":{"$in":', toJSON(x$Facility), '}}')
  query
})

## Retrieve Access Collaborative data
ac_data <- purrr::map(ac_facilities_api_query, function(x){
  path <- paste(config$source_url,"api", config$api_version, "assets", config$assets, "data.json", x, sep = "/")
  
  r <- httr::GET(URLencode(path), authenticate(config$source_user, config$source_pass),
                 timeout(config$timeout))
  
  check_for_response(r)

  d <- httr::content(r, "text") %>%
    jsonlite::fromJSON(.) %>%
    .$results
  
})

## marge the list back to one df
ac_facilities <- dplyr::bind_rows(ac_facilities)

# Transform AC DATA -------------------------------------------------------

## remove the nulls
ac_data <- ac_data[!vapply(ac_data, is.vector, logical(1))]

## load dhis2 config file
dhis_des <- readxl::read_xlsx(paste0("./data/", config$des_mapping_file),
                              sheet = "data elements") %>% 
  dplyr::filter(dhis2_labelss != "0")

## rename thee columns
# split the /
ac_data_transformed <- purrr::map(ac_data, function(x){
  col_names <- strsplit(names(x), "/") %>% 
    rapply(., function(x) tail(x, 1)) %>% trimws(.)
  
  names(x) <- col_names
  
  # select cols to import
  x <- dplyr::select(x, data_date, facility, dplyr::any_of(dhis_des$kobo_labels))
  
  ## convert the -99 to zero
  x[x == "-99"] <- "0"
  
  ## make the data long
  x %>%
    tidyr::pivot_longer(., 3:30,
                        names_to = "dataElement",
                        values_to = "value") %>% # remove NAs
    dplyr::filter(., !is.na(value)) -> x
  
  ## rename data elements
  x$dataElement <- plyr::mapvalues(x$dataElement,
                                   from = dhis_des$kobo_labels,
                                   to = dhis_des$id,
                                   warn_missing = F
  )
  
  ### Rename facilities
  x$facility <- plyr::mapvalues(x$facility,
                                from = ac_facilities$Facility,
                                to = ac_facilities$`DHIS2 UID`,
                                warn_missing = F)
  
  ## rename thee columns
  names(x) <- c("period", "orgUnit", "dataElement", "value")
  
  ## convert the date to months
  x$period <- format(as.Date(x$period), "%Y%m")
  
  ## filter the dates
  if (config$data_pull == "last_month"){
    ## Get the date for last month
    ## considering the data pull happens every 16 day of the month (28 + 16)
    last_month <- paste0(lubridate::year(as.POSIXlt(Sys.Date() - 44, format = "%Y-%m-%d")),
                         paste0(0,lubridate::month(as.POSIXlt(Sys.Date() - 44, format = "%Y-%m-%d"))))
    
    x <- dplyr::filter(x, period == last_month)
  }
  
  x
  
})

# Update the AC KOBO TOOL (UG HMIS 105 Tool) in DHIS2  --------------------

ug_hmis_105_tool <- httr::GET(paste0(config$dest_url, "api/dataSets/", config$dhis2_data_set),
                              authenticate(config$dest_user, config$dest_pass)) %>%
  httr::content(., "text") %>%
  jsonlite::fromJSON(.)

ug_hmis_105_tool$organisationUnits <- data.frame(
  id = c(ug_hmis_105_tool$organisationUnits$id,ac_facilities$`DHIS2 UID`) %>% unique(.)
)

## update the UG HMIS tool
d <- httr::POST(paste0(config$dest_url, "api/metadata?importStrategy=UPDATE"),
                authenticate(config$dest_user, config$dest_pass),
                timeout(config$timeout),
                body = toJSON(list(dataSets = list(ug_hmis_105_tool)), auto_unbox = T),
                content_type_json())


# Import the data to DHIS2 ------------------------------------------------

ac_data_transformed_d <- purrr::map(ac_data_transformed, function(x){
  d <- httr::POST(paste0(config$dest_url, "api/dataValueSets"),
                  authenticate(config$dest_user, config$dest_pass),
                  timeout(config$timeout),
                  body = toJSON(list(dataValues = x), auto_unbox = T),
                  content_type_json())
  d
})


