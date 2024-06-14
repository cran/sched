#' Make a POST request.
#'
#' Construct a sched::Request object with a valid header for a POST request.
#'
#' @param url A \code{sched::URL} object.
#' @param body The body of the POST request.
#' @param mime The MIME type of the body. Example: "text/xml",
#'             "application/json".
#' @param soap_action In case of a SOAP request, the SOAP action to contact, as
#'                    a character string.
#' @param encoding The encoding to use. A valid integer or string as required by
#'                 RCurl.
#' @return A sched::Request object.
#'
#' @examples
#' # Prepare the URL and the request body
#' the_url <- sched::URL$new('https://httpbin.org/anything')
#' the_body <- '{"some_key": "my_value"}'
#'
#' # Make the request object
#' my_request <- sched::make_post_request(the_url, body = the_body,
#'                                        mime = "application/json")
#'
#' @import chk
#' @import methods
#' @include URL.R
#' @include Request.R
#' @export
make_post_request <- function(url, body, mime, soap_action = NULL,
                              encoding = NULL) {

  chk::chk_is(url, "URL")
  chk::chk_string(body)
  chk::chk_string(mime)
  chk::chk_null_or(soap_action, vld = chk::vld_string)
  chk::chk_true(chk::vld_null(encoding) || chk::vld_whole_number(encoding)
                || chk::vld_string(encoding))

  # Prepare
  header <- c(Accept = mime, Accept = "multipart/*",
              "Content-Type" = paste(mime, "charset=utf-8", sep = ";"))
  if (! is.null(soap_action))
    header <- c(header, SOAPAction = soap_action)

  # Build request
  request <- sched::Request$new(url, method = "post",
                                header = header, body = body,
                                encoding = encoding)

  return(request)
}

#' Send a request and get results.
#'
#' Send the request described by a Request instance, using the provided user
#' agent, and return the results.
#'
#' @param request A \code{sched:Request} object.
#' @param useragent The user agent, as a character value. Example: "myapp ;
#' my.name@my.addr"
#' @param ssl_verifypeer Set to \code{FALSE} if you want to disable SSL
#' verification for https sites. \code{TRUE} by default.
#' @param binary Set to TRUE if the content to be retrieved is binary.
#' @return The request result, as a character value.
#'
#' @examples
#' # Retrieve the content of a web page
#' u <- sched::URL$new('https://httpbin.org/get')
#' content <- sched::get_url_request_result(sched::Request$new(u))
#'
#' @import chk
#' @import RCurl
#' @include Request.R
#' @export
get_url_request_result <- function(request, useragent = NULL,
                                   ssl_verifypeer = TRUE, binary = FALSE) {

  chk::chk_is(request, "Request")
  chk::chk_null_or(useragent, vld = chk::vld_string)
  chk::chk_flag(ssl_verifypeer)
  # Tests first if URL exists, since it may occur that RCurl does not
  # see a valid URL as in the case of UniProt server on Windows.
  # We want to catch the following error:
  # <simpleWarning in max(i): no non-missing arguments to max;
  #     returning -Inf>
  #
  # This error happens on Windows when downloading from UniProt using
  # RCurl:
  # https://www.uniprot.org/uniprot/?query=&columns=id&format=tab&limit=2
  #
  # More precisely the original error is:
  # Error in function (type, msg, asError = TRUE)  :
  #error:14077102:SSL routines:SSL23_GET_SERVER_HELLO:unsupported protocol
  # which leads to the "simpleWarning" error.
  #
  # The error does not appear if we use base::url() instead of
  # RCurl::getUrl().
  if (does_rcurl_request_url_exist(request, useragent = useragent)) {
    res <- get_rcurl_request_result(request, useragent = useragent,
                                    ssl_verifypeer = ssl_verifypeer,
                                    binary = binary)
  } else {
    lgr::get_logger("sched")$trace("Using base::url() for sending request.")
    s_url <- request$getUrl()$toString()
    lgr::get_logger("sched")$debug(sprintf(paste(
      "URL \"%s\" does not exist according to RCurl.",
      "That may happen with some protocol misunderstanding.",
      "Trying with base::url()."
    ), s_url))
    res <- get_base_url_request_result(request, binary = binary)
  }

  return(res)
}

# Test if a URL is valid according to RCurl
does_rcurl_request_url_exist <- function(request, useragent = NULL) {

  opts <- request$getCurlOptions(useragent = useragent)
  s_url <- request$getUrl()$toString()
  exists <- RCurl::url.exists(s_url, .opts = opts)
  if (! exists)
    lgr::get_logger("sched")$trace(sprintf(
      "According to RCurl, URL %s does not exist.",
      s_url
    ))

  return(exists)
}

# Get URL content using RCurl::getURL().
get_rcurl_content <- function(u, opts = NULL, enc = integer(),
                              header_fct = NULL, ssl_verifypeer = TRUE,
                              method = c("get", "post"), binary = FALSE) {

  method <- match.arg(method)
  if (is.null(opts))
    opts <- list()

  if (binary)
    content <- RCurl::getBinaryURL(u, .opts = opts, .encoding = enc,
                                   ssl.verifypeer = ssl_verifypeer,
                                   headerfunction = header_fct)
  else
    content <- RCurl::getURL(u, .opts = opts, .encoding = enc,
                             ssl.verifypeer = ssl_verifypeer,
                             headerfunction = header_fct)

  return(content)
}

# Get URL request result using RCurcl::getURL().
#' @include RequestResult.R
get_rcurl_request_result <- function(request, useragent = NULL,
                                     ssl_verifypeer = TRUE, binary = FALSE) {

  # Try to run request
  res <- try_get_rcurl_content(request, useragent = useragent, binary = binary,
                               ssl_verifypeer = ssl_verifypeer)

  # Process errors
  status <- chk_status_in_curl_result(res)

  return(RequestResult$new(content = res$content, # nolint: object_usage_linter
                           retry = res$retry, err_msg = status$err_msg,
                           status = status$status,
                           status_msg = status$status_msg,
                           retry_after = status$retry_after,
                           location = status$location))
}

# Get URL content using base::url().
get_base_url_content <- function(u, binary = FALSE) {

  # Open URL and get URL descriptor
  ud <- base::url(u)

  # Get content
  content <- tryCatch(
    expr = paste(readLines(ud, warn = FALSE), collapse = "\n"),
    warning = function(w) NULL,
    error = function(e) NULL
  )

  # Close URL descriptor
  close(ud)

  # Convert to raw
  if (binary)
    content <- charToRaw(content)

  return(content)
}

# Get URL request result using base::url().
get_base_url_request_result <- function(request, binary = FALSE) {

  s_url <- request$getUrl()$toString()
  lgr::get_logger("sched")$trace(sprintf(
    "Using base::url() for sending request (%s).",
    s_url
  ))

  if (request$getMethod() != "get") {
    msg <- sprintf("Request method \"%s\" is not hanlded by base::url().",
                   request$getMethod())
    lgr::get_logger("sched")$fatal(msg)
    stop(msg)
  }

  content <- get_base_url_content(s_url, binary = binary)

  # NOTE For which case?
  #  if (! is.null(content) && (! is.character(content) || content == ""))
  #    content <- NULL # nolint: commented_code_linter

  err <- if (is.null(content)) "Error when retrieving URL content" else
    NULL

  status <- if (is.null(content))
    http_status$not_found # nolint: object_usage_linter
  else
    http_status$ok # nolint: object_usage_linter

  res <- RequestResult$new(content = content, # nolint: object_usage_linter
                           retry = FALSE, err_msg = err, status = status)

  return(res)
}

try_get_rcurl_content <- function(request, useragent, ssl_verifypeer, binary) {

  # Log msg
  s_url <- request$getUrl()$toString()
  lgr::get_logger("sched")$trace(sprintf(
    "Using RCurl package for sending request (%s).",
    s_url
  ))

  # Set options
  opts <- request$getCurlOptions(useragent = useragent)
  enc <- request$getEncoding()
  if (is.null(enc))
    enc <- integer()

  # Run request and catch errors
  res <- tryCatch(
    expr = {

      # Create an HTTP header object in order to receive HTTP information from
      # server
      header <- RCurl::basicHeaderGatherer()
      header$reset()

      # Get content
      x <- get_rcurl_content(s_url, opts = opts, enc = enc,
                             header_fct = header$update,
                             ssl_verifypeer = ssl_verifypeer,
                             method = request$getMethod(), binary = binary)
      list(content = x, header = header, curl_error = NULL, retry = FALSE)
    },
    # nolint start: object_usage_linter
    # NOTE Included in GenericCurlError, right?
#    PEER_FAILED_VERIFICATION = function(err) {
#      list(content = NA_character_, header = NULL, curl_error = err,
#           retry = TRUE)
#    },
    GenericCurlError = function(err) {
      list(content = NA_character_, header = NULL, curl_error = err,
           retry = TRUE)
    }
    # NOTE When such an error should occur?
#    error = function(err) {
#      list(content = NA_character_, header = NULL, curl_error = err,
#           retry = FALSE)
#    }
    # nolint end
  )

  return(res)
}

chk_status_in_curl_result <- function(res) { # nolint: cyclocomp_linter

  x <- list(err_msg = NULL, hdr = NULL)

  if (! is.null(res$curl_error)) { # Store RCurl error
    x$err_msg <- paste0("RCurl error: ", res$curl_error)

  } else { # Get & analyze header information sent by server
    x <- tryCatch(
      expr = list(hdr = as.list(res$header$value()), err_msg = NULL),
      warning = function(msg) {
        list(hdr = NULL,
             err_msg = paste0("Warning while retrieving HTTP header:",
                              msg, "."))
      },
      error = function(msg) {
        list(hdr = NULL,
             err_msg = paste0("Error while retrieving HTTP header:",
                              msg, "."))
      }
    )

    if (! is.null(x$hdr)) {
      x$hdr$status <- as.integer(x$hdr$status)
      if (is.na(x$hdr$status) || x$hdr$status == 0) {
        x$hdr <- NULL
        x$err_msg <- "Cannot find status info in HTTP header."
      }
    }
  }

  # Process header status
  x$status <- if (is.null(x$hdr) || ! "status" %in% names(x$hdr)) 0 else
    x$hdr$status
  x$status_msg <- if (is.null(x$hdr) || ! "status_msg" %in% names(x$hdr))
    "" else x$hdr$status_msg
  x$retry_after <- if (is.null(x$hdr) || ! "Retry-After" %in% names(x$hdr))
    NULL else x$hdr[["Retry-After"]]
  x$location <- if (is.null(x$hdr) || ! "location" %in% names(x$hdr)) NULL else
    x$hdr[["location"]]
  x$hdr <- NULL

  return(x)
}
