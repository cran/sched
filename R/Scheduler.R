#' Class for scheduling web requests.
#'
#' The Scheduler class controls the frequency of access to web sites, through
#' the definiton of access rules (`Rule` class).
#' It handles GET and POST requests, as well as file downloading.
#' It can use a cache system to store request results and avoid resending
#' identical requests.
#'
#' @examples
#' # Create a scheduler instance without cache
#' scheduler <- sched::Scheduler$new(cache_dir = NULL)
#'
#' # Define a rule with default values
#' scheduler$setRule('www.ebi.ac.uk')
#'
#' # Create a request object
#' u <- 'https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity'
#' url <- sched::URL$new(url=u, params=c(chebiId=15440))
#' request <- sched::Request$new(url)
#'
#' # Send the request and get the content result
#' content <- scheduler$sendRequest(request)
#'
#' @import R6
#' @import fscache
#' @import chk
#' @import lgr
#' @import tools
#' @import openssl
#' @include Request.R
#' @include RequestResult.R
#' @include Rule.R
#' @include request_fcts.R
#' @export
Scheduler <- R6::R6Class( # nolint: object_name_linter
  "Scheduler",

  public = list(

    #' @description
    #' New instance initializer.
    #'
    #' There should be only one Scheduler instance in an application. There is
    #' no sense in having two or more instances, since they will ignore each
    #' other and break the access frequency rules when they contact the same
    #' sites.
    #'
    #' @param default_rule The default_rule to use when none has been defined
    #' for a site.
    #' @param ssl_verifypeer If set to TRUE (default), SSL certificate will be
    #' checked, otherwise certificates will be ignored.
    #' @param nb_max_tries Maximum number of tries when running a request.
    #' @param cache_dir Set the path to the file system cache. Set to NULL to
    #' disable the cache system. The cache system will save downloaded content
    #' and reuse it later for identical requests.
    #' @param user_agent The application name and contact address to send to the
    #' contacted web server.
    #' @param dwnld_timeout The timeout used by \code{downloadFile()} method, in
    #' seconds.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a scheduler instance with a custom default_rule
    #' scheduler <- sched::Scheduler$new(default_rule=sched::Rule$new(10, 1),
    #'                                   cache_dir = NULL)
    #'
    initialize = function(default_rule = Rule$new(), ssl_verifypeer = TRUE,
                          nb_max_tries = 10L,
                          cache_dir = tools::R_user_dir("sched",
                                                        which = "cache"),
                          user_agent = NULL, dwnld_timeout = 3600) {

      chk::chk_is(default_rule, "Rule")
      chk::chk_null_or(cache_dir, vld = chk::vld_string)
      chk::chk_flag(ssl_verifypeer)
      chk::chk_whole_number(nb_max_tries)
      chk::chk_true(nb_max_tries > 0)
      chk::chk_null_or(user_agent, vld = chk::vld_string)
      chk::chk_whole_number(dwnld_timeout)
      chk::chk_true(dwnld_timeout > 0)

      private$host2rule <- list()
      private$default_rule <- default_rule
      private$nb_max_tries <- nb_max_tries
      private$ssl_verifypeer <- ssl_verifypeer
      if (! is.null(cache_dir))
        private$cache <- fscache::Cache$new(folder = cache_dir)
      private$user_agent <- user_agent
      private$dwnld_timeout <- dwnld_timeout

      return(invisible(NULL))
    },

    #' @description
    #' Defines a rule for a site.
    #'
    #' Defines a rule for a site. The site is identified by its hostname. Each
    #' time a request will be made to this host (i.e.: the URL contains the
    #' defined hostname), the scheduling rule will be applied in order to wait
    #' (sleep) if nedeed before sending the request.
    #'
    #' If a rule already exists for this hostname, it will be replaced.
    #'
    #' @param host The hostname of the site.
    #' @param n    Number of events during a time lap.
    #' @param lap  Duration of a time lap, in seconds.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a scheduler instance
    #' scheduler <- sched::Scheduler$new(cache_dir = NULL)
    #'
    #' # Define a rule with default values
    #' scheduler$setRule('www.ebi.ac.uk')
    #'
    #' # Define a rule with custome values
    #' scheduler$setRule('my.other.site', n=10, lap=3)
    #'
    setRule = function(host, n = 3L, lap = 1) {

      chk::chk_string(host)

      private$host2rule[[host]] <- sched::Rule$new(n = n, lap = lap)

      return(invisible(NULL))
    },

    #' @description
    #' Sends a request, and retrieves content result.
    #'
    #' @param request A \code{sched::Request} instance.
    #' @param cache_read If set to TRUE and the cache system is enabled, the
    #' cache system will be searched for the request and the cached result
    #' returned. In any case, if the the cache system is enabled, and the
    #' request sent, the retrieved content will be stored into the cache.
    #' @return The results returned by the contacted server, as a single string
    #' value.
    #'
    #' @examples
    #' # Create a scheduler instance
    #' scheduler <- sched::Scheduler$new(cache_dir = NULL)
    #'
    #' # Define a scheduling rule of 7 requests every 2 seconds
    #' scheduler$setRule('www.ebi.ac.uk', n=7, lap=2)
    #'
    #' # Create a request object
    #' u <- 'https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity'
    #' url <- sched::URL$new(url=u, params=c(chebiId=15440))
    #' request <- sched::Request$new(url)
    #'
    #' # Send the request and get the content result
    #' content <- scheduler$sendRequest(request)
    #'
    sendRequest = function(request, cache_read = TRUE) {

      chk::chk_is(request, "Request")
      chk::chk_flag(cache_read)

      content <- NA_character_

      # Get rule
      rule <- private$findRule(request$getUrl())

      # Log URL
      lgr::get_logger("sched")$debug(sprintf(
        "Getting content of %s URL request \"%s\".",
        request$getMethod(), request$getUrl()$toString(encode = FALSE)
      ))

      # Try to get query result from cache
      request_key <- request$getUniqueKey()
      if (cache_read && ! is.null(private$cache)) {
        content <- private$cache$loadContents(paths = request_key,
                                              suffix = ".content")
        if (! is.na(content))
          lgr::get_logger("sched")$debug("Content loaded from cache.")
      }

      if (is.na(content)) {

        # Check if in offline mode
        private$checkOfflineMode()

        # Send request
        content <- private$doSendRequestLoop(request = request, rule = rule)

        # Save content to cache
        if (! is.na(content) && ! is.null(private$cache)) {
          private$cache$saveContents(content, dst = request_key,
                                     suffix = ".content")
          private$cache$saveContents(request$toString(), dst = request_key,
                                     suffix = ".request")
          lgr::get_logger("sched")$debug("Retrieved content saved to cache.")
        }
      }

      return(content)
    },

    #' @description
    #' Downloads the content of a URL and save it into the specified
    #' destination file.
    #'
    #' This method works for any URL, even if it has been written with heavy
    #' files in mind.
    #' Since it uses \code{utils::download.file()} which saves the content
    #' directly on disk, the cache system is not used.
    #'
    #' @param url The URL to access, as a sched::URL object.
    #' @param dest_file A path to a destination file.
    #' @param quiet The quiet parameter for \code{utils::download.file()}.
    #' @param timeout The timeout in seconds. Defaults to value provided in
    #' initializer.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a scheduler instance
    #' scheduler <- sched::Scheduler$new(cache_dir = NULL)
    #'
    #' # Create a temporary directory
    #' tmp_dir <- tempdir()
    #'
    #' # Download a file
    #' u <- sched::URL$new(
    #'     'https://gitlab.com/cnrgh/databases/r-sched/-/raw/main/README.md',
    #'     c(ref_type='heads'))
    #' scheduler$downloadFile(u, file.path(tmp_dir, 'README.md'))
    #'
    #' # Remove the temporary directory
    #' unlink(tmp_dir, recursive = TRUE)
    #'
    downloadFile = function(url, dest_file, quiet = FALSE, timeout = NULL) {

      chk::chk_is(url, "URL")
      chk::chk_string(dest_file)
      chk::chk_flag(quiet)
      if (is.null(timeout)) {
        timeout <- private$dwnld_timeout
      } else {
        chk::chk_whole_number(timeout)
        chk::chk_true(timeout > 0)
      }
      # Get rule
      rule <- private$findRule(url)

      # Wait required time between two requests
      rule$wait()

      # Convert URL to string
      url <- url$toString()

      # Make sure path exists
      path <- dirname(dest_file)
      if (! dir.exists(path))
        dir.create(path, recursive = TRUE)

      # Download
      lgr::get_logger("sched")$debug(sprintf("Downloading file \"%s\".", url))
      options(HTTPUserAgent = private$user_agent,
              timeout = private$dwnld_timeout)
      utils::download.file(url = url, destfile = dest_file, mode = "wb",
                           method = "auto", cacheOK = FALSE, quiet = quiet)

      return(invisible(NULL))
    },

    #' @description
    #' Builds a URL string, using a base URL and parameters to be passed.
    #'
    #' The provided base URL and parameters are combined into a full URL string.
    #'
    #' DEPRECATED. Use the \code{sched::URL} class and its method
    #' \code{toString()} instead.
    #'
    #' @param url A URL string.
    #' @param params A list of URL parameters.
    #' @return The full URL string as a single character value.
    #'
    #' @examples
    #' # Create a scheduler instance
    #' scheduler <- sched::Scheduler$new(cache_dir = NULL)
    #'
    #' # Create a URL string
    #' url.str <- scheduler$getUrlString(
    #'   'https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity',
    #'   params=c(chebiId=15440))
    #'
    getUrlString = function(url, params = list()) {

      lifecycle::deprecate_warn("1.0.0", "getUrlString()",
                                "sched::URL$toString()")

      u <- sched::URL$new(url = url, params = params)$toString(encode = FALSE)

      return(u)
    },

    #' @description
    #' Sends a request and get the result.
    #'
    #' DEPRECATED. Use method \code{sendRequest()} instead.
    #'
    #' @param url A URL string.
    #' @param params A list of URL parameters.
    #' @param method The method to use. Either 'get' or 'post'.
    #' @param header The header to send.
    #' @param body The body to send.
    #' @param encoding The encoding to use.
    #' @return The results of the request.
    #'
    #' @examples
    #' # Create a scheduler instance
    #' scheduler <- sched::Scheduler$new(cache_dir = NULL)
    #'
    #' # Send request
    #' content <- scheduler$getUrl(
    #'   'https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity',
    #'   params=c(chebiId=15440))
    #'
    getUrl = function(url, params = list(), method = c("get", "post"),
                      header = NULL, body = NULL, encoding = NULL) {

      lifecycle::deprecate_warn("1.0.0", "getUrl()",
                                "sched::Scheduler$sendRequest()")

      method <- match.arg(method)

      request <- sched::Request$new(url = sched::URL$new(url = url,
                                                         params = params),
                                    method = method, header = header,
                                    body = body, encoding = encoding)

      return(self$sendRequest(request))
    },

    #' @description
    #' Removes all defined rules, including the ones automatically defined using
    #' default_rule.
    #'
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a scheduler instance
    #' scheduler <- sched::Scheduler$new(cache_dir = NULL)
    #'
    #' # Define a rule with custome values
    #' scheduler$setRule('my.other.site', n=10, lap=3)
    #'
    #' # Delete all defined rules
    #' scheduler$deleteRules()
    #'
    deleteRules = function() {
      private$host2rule <- list()
      return(invisible(NULL))
    },

    #' @description
    #' Gets the number of defined rules, including the ones automatically
    #' defined using default_rule.
    #'
    #' @return The number of rules defined.
    #'
    #' @examples
    #' # Create a scheduler instance
    #' scheduler <- sched::Scheduler$new(cache_dir = NULL)
    #'
    #' # Get the number of defined rules
    #' print(scheduler$getNbRules())
    #'
    getNbRules = function() {
      return(length(private$host2rule))
    },

    #' @description
    #' Enables or disables offline mode.
    #'
    #' If the offline mode is enabled, an error will be raised when the class
    #' attemps to send a request.
    #' This mode is mainly useful when debugging the usage of the cache system.
    #'
    #' @param offline Set to TRUE to enable offline mode, and FALSE otherwise.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a scheduler instance
    #' scheduler <- sched::Scheduler$new(cache_dir = NULL)
    #'
    #' # Enable offline mode
    #' scheduler$setOffline(TRUE)
    #'
    setOffline = function(offline) {
      chk::chk_flag(offline)
      private$offline <- offline
      return(invisible(NULL))
    }
  ),

  private = list(
    host2rule = NULL,
    default_rule = NULL,
    nb_max_tries = NULL,
    ssl_verifypeer = NULL,
    cache = NULL, # Cache system instance
    user_agent = NULL,
    dwnld_timeout = NULL,
    offline = FALSE,

    findRule = function(url) {

      domain <- url$getDomain()

      # Rule already exists
      if (domain %in% names(private$host2rule))
        rule <- private$host2rule[[domain]]
      else
        private$host2rule[[domain]] <- private$default_rule$clone()

      return(private$host2rule[[domain]])
    },

    checkOfflineMode = function() {

      if (private$offline) {
        msg <- "Attempting a connection while offline mode is enabled."
        lgr::get_logger("sched")$fatal(msg)
        stop(msg)
      }

      return(invisible(NULL))
    },

    doSendRequestOnce = function(request) {

      # Get URL as a string
      s_url <- request$getUrl()$toString()
      lgr::get_logger("sched")$trace(sprintf("Sent URL is \"%s\".", s_url))

      # Send request and get result
      res <- get_url_request_result(request, useragent = private$user_agent,
                                    ssl_verifypeer = private$ssl_verifypeer)

      # Process errors
      res$processRequestErrors()

      # Return result
      return(list(content = res$getContent(), err_msg = res$getErrMsg(),
                  retry = res$getRetry()))
    },

    doSendRequestLoop = function(request, rule) {

      content <- NA_character_

      # Enter query loop
      i <- 0
      retry <- TRUE
      while (retry && i < private$nb_max_tries) {

        # Increment try number
        i <- i + 1

        # Print debug information about header and body
        lgr::get_logger("sched")$debug(sprintf(
          "Request header is: \"%s\".",
          request$getHeaderAsSingleString()
        ))
        lgr::get_logger("sched")$debug(sprintf(
          "Request body is \"%s\".",
          paste(request$getBody(), collapse = ", ")
        ))

        # Wait required time between two requests
        rule$wait()

        # Send request
        res <- private$doSendRequestOnce(request = request)
        retry <- res$retry

        # Print connection error message
        if (! is.null(res$err_msg)) {
          if (retry) {
            m <- paste0(" When contacting URL \"",
                        request$getUrl()$toString(),
                        "\". Retrying connection to server...")
            res$err_msg <- paste0(res$err_msg, m)
          }
          lgr::get_logger("sched")$info(res$err_msg)

        } else {
          content <- res$content
        }
      }

      return(content)
    }
  )
)
