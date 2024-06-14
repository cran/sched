#' Class Request.
#'
#' This class represents a Request object that can be used with the Request
#' Scheduler.
#'
#' @examples
#' # Create a GET request for the getCompleteEntity webservice of ChEBI database
#' request <- sched::Request$new(
#'   sched::URL$new(
#'     'https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity',
#'     params=c(chebiId=15440)))
#'
#' # Get an MD5 key, unique to this request
#' key <- request$getUniqueKey()
#'
#' # Print the request
#' print(request)
#'
#' @import R6
#' @import openssl
#' @import chk
#' @import RCurl
#' @export
Request <- R6::R6Class("Request", # nolint: object_name_linter

  public = list(

    #' @description
    #' Initializer.
    #'
    #' @param url A \code{sched::URL} object.
    #' @param method HTTP method. Either "get" or "post".
    #' @param header The header of the POST method as a named character vector.
    #' The names are the fields of the header.
    #' @param body The body as a character single value.
    #' @param encoding The encoding to use. A valid integer or string as
    #' required by RCurl.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a GET request for the getCompleteEntity webservice of ChEBI
    #' # database
    #' request <- sched::Request$new(
    #'   sched::URL$new(
    #'     'https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity',
    #'     params=c(chebiId=15440)))
    #'
    #' # Create a POST Request object for the records-batch-post webservice of
    #' # ChemSpider database
    #' request <- sched::Request$new(
    #'   url=sched::URL$new(c('https://api.rsc.org/compounds/v1/', 'records',
    #'                        'batch')),
    #'   method='post', header=c('Content-Type'="", apikey='my-token'),
    #'   body='{"recordIds": [2], "fields": ["SMILES","Formula","InChI"]}')
    #'
    initialize = function(url, method = c("get", "post"), header = NULL,
                          body = NULL, encoding = NULL) {

      chk::chk_is(url, "URL")
      chk::chk_character(method)
      chk::chk_null_or(header, vld = chk::vld_character)
      chk::chk_null_or(body, vld = chk::vld_string)
      chk::chk_true(chk::vld_null(encoding) || chk::vld_whole_number(encoding)
                    || chk::vld_string(encoding))

      private$url <- url
      private$method <- match.arg(method)
      private$header <- header
      private$body <- body
      private$encoding <- encoding

      return(invisible(NULL))
    },

    #' @description
    #' Gets the URL.
    #'
    #' @return The URL of this Request object as a sched::URL object.
    #'
    #' @examples
    #' # Create a GET request
    #' request <- sched::Request$new(sched::URL$new('https://peakforest.org/'))
    #'
    #' # Get the stored URL object
    #' print(request$getUrl())
    getUrl = function() {
      return(private$url)
    },

    #' @description
    #' Gets the method.
    #'
    #' @return The method as a character value.
    #'
    #' @examples
    #' # Create a GET request
    #' request <- sched::Request$new(sched::URL$new('https://peakforest.org/'))
    #'
    #' # Get the stored method
    #' print(request$getMethod())
    getMethod = function() {
      return(private$method)
    },

    #' @description
    #' Gets the encoding.
    #'
    #' @return The encoding.
    #'
    #' @examples
    #' # Create a GET request
    #' request <- sched::Request$new(sched::URL$new('https://my.site.fr/'),
    #'                               encoding='UTF-8')
    #'
    #' # Get the stored encoding
    #' print(request$getEncoding())
    getEncoding = function() {
      return(private$encoding)
    },

    #' @description
    #' Gets the options object to pass to cURL library.
    #'
    #' Make a RCurl::CURLOptions object by calling RCurl::curlOptions()
    #' function. Useragent, header and body are passed as options if not NULL.
    #'
    #' @param useragent The user agent as a character value, or NULL.
    #' @return An RCurl::CURLOptions object.
    #'
    #' @examples
    #' # Create a POST Request object for the records-batch-post webservice of
    #' # ChemSpider database
    #' request <- sched::Request$new(
    #'   url=sched::URL$new(c('https://api.rsc.org/compounds/v1/', 'records',
    #'                        'batch')),
    #'   method='post', header=c('Content-Type'="", apikey='my-token'),
    #'   body='{"recordIds": [2], "fields": ["SMILES","Formula","InChI"]}')
    #'
    #' # Get the associated RCurl options object
    #' rcurl_opts <- request$getCurlOptions('myapp ; me@my.address')
    #'
    getCurlOptions = function(useragent = NULL) {

      chk::chk_null_or(useragent, vld = chk::vld_string)

      # Set option field with header and body if any
      opts <- list()
      if (! is.null(private$header) && length(private$header) > 0)
        opts$httpheader <- private$header
      if (! is.null(private$body) && length(private$body) > 0)
        opts$postfields <- private$body

      # Build RCurl options object
      opts <- RCurl::curlOptions(useragent = useragent, timeout.ms = 60000,
                                 verbose = FALSE, .opts = opts)

      return(opts)
    },

    #' @description
    #' Gets a unique key to identify this request.
    #'
    #' The key is an MD5 sum computed from the string representation of this
    #' request.
    #'
    #' @return A unique key as an MD5 sum.
    #'
    #' @examples
    #' # Create a GET request
    #' request <- sched::Request$new(sched::URL$new('https://peakforest.org/'))
    #'
    #' # Get the MD5 sum of this request
    #' print(request$getUniqueKey())
    #'
    getUniqueKey = function() {

      key <- openssl::md5(self$toString())

      return(key)
    },

    #' @description
    #' Gets the HTTP header as a string, concatenating all its information
    #' into a single string.
    #'
    #' @return The header as a single character value.
    #'
    #' @examples
    #' # Create a POST Request object for the records-batch-post webservice of
    #' # ChemSpider database
    #' request <- sched::Request$new(
    #'   url=sched::URL$new(c('https://api.rsc.org/compounds/v1/', 'records',
    #'                        'batch')),
    #'   method='post', header=c('Content-Type'="", apikey='my-token'),
    #'   body='{"recordIds": [2], "fields": ["SMILES","Formula","InChI"]}')
    #'
    #' # Get back the POST header as a single string
    #' print(request$getHeaderAsSingleString())
    #'
    getHeaderAsSingleString = function() {

      s <- ""

      if (length(private$header) > 0) {
        fct <- function(k) paste(k, private$header[[k]], sep = "=")
        kv <- vapply(names(private$header), fct, FUN.VALUE = "")
        s <- paste(kv, collapse = ", ")
      }

      return(s)
    },

    #' @description
    #' Gets the body.
    #'
    #' @return The body as a single character value.
    #'
    #' @examples
    #' # Create a POST Request object for the records-batch-post webservice of
    #' # ChemSpider database
    #' request <- sched::Request$new(
    #'   url=sched::URL$new(c('https://api.rsc.org/compounds/v1/', 'records',
    #'                        'batch')),
    #'   method='post', header=c('Content-Type'="", apikey='my-token'),
    #'   body='{"recordIds": [2], "fields": ["SMILES","Formula","InChI"]}')
    #'
    #' # Get back the POST body
    #' print(request$getBody())
    #'
    getBody = function() {
      return(private$body)
    },

    #' @description
    #' Displays information about this instance.
    #'
    #' @return self as invisible.
    #'
    #' @examples
    #' # Create a GET request
    #' request <- sched::Request$new(sched::URL$new('https://peakforest.org/'))
    #'
    #' # Print the Request object
    #' print(request)
    #'
    print = function() {
      cat(toupper(private$method), " request at URL ", private$url$toString(),
          "\n", sep = "")
      return(invisible(self))
    },

    #' @description
    #' Gets a string representation of this instance.
    #'
    #' @return A single string giving a representation of this instance.
    #'
    #' @examples
    #' # Create a GET request
    #' request <- sched::Request$new(sched::URL$new('https://peakforest.org/'))
    #'
    #' # Get the string representation of this request
    #' print(request$toString())
    #'
    toString = function() {

      request <- list(url = private$url$toString(), header = private$header,
                      body = private$body)
      request_json <- jsonlite::serializeJSON(request)
      request_json_str <- as.character(request_json)

      return(request_json_str)
    }
  ),

  private = list(
    url = NULL,
    method = NULL,
    header = NULL,
    body = NULL,
    encoding = NULL,
    conn = NULL
  )
)
