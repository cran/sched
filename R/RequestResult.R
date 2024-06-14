http_status <- list(
  ok = 200,
  found = 302,
  not_found = 404,
  request_timeout = 408,
  internal_server_error = 500,
  service_unavailable = 503
)

#' Class RequestResult.
#'
#' Represents the result of a request.
#'
#' @import R6
#' @import chk
#' @import lgr
RequestResult <- R6::R6Class("RequestResult", # nolint: object_name_linter

  public = list(

    #' @description
    #' New instance initializer.
    #'
    #' @param content The result content.
    #' @param retry If request should be resent.
    #' @param err_msg Error message.
    #' @param status HTTP status.
    #' @param status_msg Status message.
    #' @param retry_after Time after which to retry.
    #' @param location New location.
    #' @return Nothing.
    initialize = function(content = NULL, retry = FALSE, err_msg = NULL,
                          status = 0, status_msg = "", retry_after = NULL,
                          location = NULL) {

      chk::chk_null_or(content, vld = chk::vld_character) # May be NA
      chk::chk_null_or(content, vld = chk::vld_length, length = 1L)
      chk::chk_flag(retry)
      chk::chk_null_or(err_msg, vld = chk::vld_string)
      chk::chk_whole_number(status)
      chk::chk_string(status_msg)
      chk::chk_null_or(retry_after, vld = chk::vld_string)
      chk::chk_null_or(location, vld = chk::vld_string)

      private$content <- content
      private$retry <- retry
      private$err_msg <- err_msg
      private$status <- status
      private$status_msg <- status_msg
      private$retry_after <- retry_after
      private$location <- location

      return(invisible(NULL))
    },

    #' @description
    #' Get content.
    #' @return The content as a character value or NULL.
    getContent = function() {
      return(private$content)
    },

    #' @description
    #' Get the retry flag.
    #' @return TRUE if the URL request should be sent again, FALSE otherwise.
    getRetry = function() {
      return(private$retry)
    },

    #' @description
    #' Get the error message.
    #' @return The error message as a character value or NULL.
    getErrMsg = function() {
      return(private$err_msg)
    },

    #' @description
    #' Get the HTTP status of the response.
    #' @return The status as an integer.
    getStatus = function() {
      return(private$status)
    },

    #' @description
    #' Get the time to wait before retrying.
    #' @return The time.
    getRetryAfter = function() {
      return(private$retry_after)
    },

    #' @description
    #' Get the redirect location.
    #' @return The redirect location as a character value or NULL.
    getLocation = function() {
      return(private$location)
    },

    #' @description
    #' Process possible HTTP error.
    #' @return Nothing.
    processRequestErrors = function() {

      # Recoverable HTTP errors
      rec_err <- unlist(http_status[c("not_found", "request_timeout",
                                      "internal_server_error", "found",
                                      "service_unavailable")])
      if (private$status %in% rec_err) {
        private$addErrMsg(paste0("HTTP error ", private$status, " (\"",
                                 private$status_msg, "\")."))
        if (! is.null(private$retry_after))
          private$addErrMsg(paste("Retry after", private$retry_after,
                                  "."))
        if (! is.null(private$location))
          private$addErrMsg(paste("Redirect location to", private$location))
        private$retry <- TRUE
      }

      # Other HTTP errors
      if (is.null(private$err_msg) && private$status != 0
          && private$status != http_status$ok) {
        private$addErrMsg(paste0("Unrecoverable HTTP error ", private$status,
                                 " (\"", private$status_msg, "\")."))
        if (! is.null(private$retry_after))
          private$addErrMsg(paste0("Retry after ", private$retry_after,
                                   "."))
        private$content <- NA_character_
        private$retry <- FALSE
      }

      # Proxy server error
      # This happens sometime with NCBI CCDS server.
      if (! is.null(private$content) && ! is.na(private$content)
          && length(grep("The proxy server could not handle the request",
                         unname(private$content))) > 0) {
        lgr::get_logger("sched")$debug("Found proxy error message in content.")
        private$addErrMsg("Error between the proxy and the main server.")
        private$content <- NA_character_
        private$retry <- FALSE
      }
    }

  ),

  private = list(
    content = NULL,
    retry = NULL,
    err_msg = NULL,
    status = NULL,
    status_msg = NULL,
    retry_after = NULL,
    location = NULL,

    addErrMsg = function(msg) {

      if (is.null(private$err_msg))
        private$err_msg <- msg
      else
        private$err_msg <- paste(private$err_msg, msg)
    }
  )
)
