# load httr for API requests
library(httr)

# Function to get data from OCLC ID.
# API only allows 50 records at once.
get_libs <- function (oclc, max_lib = 50,
                      # documentation says that value is 0-based, but using 0
                      # returns "Invalid value" error. 1 is OK.
                      start_lib = 1,
                      base = "https://americas.discovery.api.oclc.org/worldcat/search/v2/bibs-holdings",
                      verbose = FALSE) {
  # 1. authenticate
  # get API key and secret from .Renviron file
  RE_key <- Sys.getenv("RE_key")
  RE_secret <- Sys.getenv("RE_secret")
  # ask for token
  oauth_resp <- POST(url = "https://oauth.oclc.org/token",
                   query = list(scope = "wcapi",
                                grant_type = "client_credentials"),
                   # use the API key for "user" and the secret for "password"
                   authenticate(RE_key, RE_secret))
  # extract token from response
  acc_tok <- content(oauth_resp)$access_token
  # 3. send request
  f <- function () {
    GET(url = base,
        query = list(
          oclcNumber = oclc,
          # UQ's IP address to first list local libraries
          # ip = "130.102.13.26",
          limit = max_lib,
          # offset is 0-based
          offset = start_lib,
          holdingsAllEditions = "true"
        ),
        add_headers(Authorization = paste("Bearer", acc_tok))
    )
  }
  # give more information if requested
  if (verbose) {
    resp <- with_verbose(f())
  } else {
    resp <- f()
  }
  # unsuccessful if status code is not 200
  if (resp$status_code != 200L) {
    # check if started again on empty
    if (resp$status_code == 400L) {
      if (any(grepl("less than or equal to the number of holdings", content(resp)))) return(NULL)
      }
    # all other cases: report error
    stop("API reports an error.\nError code: ",
         resp$status_code, "\n",
         "Error type: ", content(resp)$type, "\n",
         content(resp)$title, "\n",
         content(resp)$detail, "\n")
    }
  # check if record does not exist
  if (start_lib == 1) {
    if (content(resp)$numberOfRecords == 0) stop("No record found. Is this the correct OCLC number?")
  }
  return(resp)
}

### OLD ###
# get_libs_old <- function (oclc, max_lib = 100, start_lib = 1,
#                       base = "https://www.worldcat.org/webservices/catalog/content/libraries/",
#                       out_format = "json",
#                       verbose = FALSE) {
#   # append code to path
#   base <- paste0(base, oclc)
#   # send request
#   f <- function () {
#     # get the API key from the .Renviron file
#     RE_key <- Sys.getenv("RE_key")
#     GET(url = base,
#       query = list(
#         # UQ's IP address to first list local libraries
#         ip = "130.102.13.26",
#         maximumLibraries = max_lib,
#         startLibrary = start_lib,
#         # JSON data might be easier to handle than XML
#         format = out_format,
#         # also get the holdings that are not publicly visible
#         servicelevel = "full",
#         wskey = RE_key
#       )
#     )
#   }
#   if (verbose) {
#     resp <- with_verbose(f())
#   } else {
#     resp <- f()
#   }
#   # check if status code means the authentication failed
#   if (resp$status_code == 407L) stop("Authentication error. Has the API key expired?")
#   # check if record does not exist (status code is 200, like for a valid OCLC code)
#   if (grepl("Record does not exist", content(resp))) stop("This OCLC code did not match a record. Try again with a valid OCLC code.")
#   return(resp)
# }

# define function to extract and convert to dataframe
library(dplyr) # to tranform and pipe
library(tibble) # to enframe
library(tidyr) # to unnest
flatten_libs <- function (response) {
  # if NULL, return empty dataframe
  if (is.null(response)) return(data.frame())
  # extract data
  contents <- content(response)
  # focus on listing briefs
  briefs <- contents[["briefRecords"]][[1]][["institutionHolding"]][["briefHoldings"]]
  # enframe and unnest list-cols
  flattened <- briefs %>%
    tibble::enframe() %>%
    unnest_wider(col = 2) %>%
    select(-state, -country) %>%
    unnest_wider(col = address)
  return(flattened)
}

#### OLD ####
# # import package for reading JSON
# library(jsonlite)
# # dplyr to flatten the interesting part of the JSON
# library(dplyr)
# # define function to extract and convert to dataframe
# flatten_libs_old <- function (results) {
#   contents <- httr::content(results)
#   cont_ls <- jsonlite::parse_json(contents)$library
#   dplyr::bind_rows(cont_ls)
# }

# function to loop through everything until all records are retrieved
get_all <- function (oclc, limit = 2000, verbose = FALSE) {
  # checks
  if (!is.numeric(limit) | limit < 50 | limit %% 50 != 0) stop("limit needs to be a multiple of 50")
  message("Limiting results to ", limit, " holdings. Change the value with limit = xxx")
  # get how many fities
  fifties <- limit %/% 50
  # loop through them until nothing left and add remainder
  # get first 50 as a base dataframe
  result_df <- get_libs(oclc, verbose = verbose) %>% flatten_libs()
  # only continue if limit is greater than 50 and there might be more results
  if (fifties > 1 & nrow(result_df) == 50) {
    # setup counter
    counter <- 1
    for (i in 1:(fifties-1)) {
      # time out to be respectful
      Sys.sleep(1)
      # give user some feedback
      message(i * 50, " holdings found. Getting more...")
      # grab next batch
      extra_df <- get_libs(oclc, start_lib = i * 50 + 1, verbose = verbose) %>% flatten_libs()
      # merge in main dataframe
      result_df <- dplyr::bind_rows(result_df, extra_df)
      # check if got everything: less than 50, or started on non-existent
      if (nrow(extra_df) != 50 | ncol(extra_df) != ncol(result_df)) break
    }
  }
  message("Done. Retrieved ", nrow(result_df), " holdings.")
  return(result_df)
}

#### OLD ####
# get_all_old <- function (oclc, limit = 2000, verbose = FALSE) {
#   # checks
#   if (!is.numeric(limit) | limit < 100 | limit %% 100 != 0) stop("limit needs to be a multiple of 100")
#   message("Limiting results to ", limit, " holdings. Change the value with limit = xxx")
#   # get fifties
#   fifties <- limit %/% 100
#   # loop through them until nothing left and add remainder
#   # get first 100 as a base dataframe
#   result_df <- get_libs(oclc, verbose = verbose) %>% flatten_libs()
#   # only continue if limit is greater than 100 and there might be more results
#   if (fifties > 1 & nrow(result_df) == 100) {
#     # setup counter
#     counter <- 1
#     for (i in 1:(fifties-1)) {
#       # time out to be respectful
#       Sys.sleep(1)
#       # give user some feedback
#       message(i * 100, " holdings found. Getting more...")
#       # grab next batch
#       extra_df <- get_libs(oclc, start_lib = i * 100 + 1, verbose = verbose) %>% flatten_libs()
#       # merge in main dataframe
#       result_df <- dplyr::bind_rows(result_df, extra_df)
#       # check if got everything: less than 100, or started on non-existent
#       if (nrow(extra_df) != 100 | ncol(extra_df) != ncol(result_df)) break
#       }
#   }
#   message("Done. Retrieved ", nrow(result_df), " holdings.")
#   return(result_df)
#   }

# TODO: several OCLC numbers at once, get book details (title, author) and compile full CSV
# TODO: comparison of research groups, or different authors, with different colours
