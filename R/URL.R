#' URL class.
#'
#' This class represents a URL object that can be used in requests.
#' It handles parameters as a list, making it easy to build URLs for contacting
#' web services.
#'
#' @examples
#' # Create a URL object from a base URL string and a list of parameters
#' base.url <- c("https://www.uniprot.org", "uniprot")
#' params <- c(query="reviewed:yes+AND+organism:9606",
#'        columns='id,entry name,protein names',
#'        format="tab")
#' url <- sched::URL$new(url=base.url, params=params)
#'
#' # Print the URL converted to a string
#' print(url$toString())
#'
#' @import R6
#' @import chk
#' @export
URL <- R6::R6Class( # nolint: object_name_linter
  "URL",

  public = list(

    #' @description
    #' Initializer.
    #'
    #' @param url The URL to access, as a character vector.
    #' @param params The list of parameters to append to this URL. If it is an
    #' unnamed list or vector, the values will be converted to strings and
    #' concatenated with the `&` separator. If it is a named list or vector, the
    #' names will be used as keys as in "name1=value1&name2=value2&...".
    #' @param chomp_extra_slashes If set to TRUE, then slashes at the end and
    #' the beginning of each element of the url vector parameter will be removed
    #' before proper concatenation.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a URL object
    #' url <- sched::URL$new("https://www.my.server/", c(param1=12,
    #'                       param2='abc'))
    #'
    initialize = function(url = character(), params = character(),
                          chomp_extra_slashes = TRUE) {

      chk::chk_character(url)
      chk::chk_not_any_na(url)
      chk::chk_flag(chomp_extra_slashes)
      # params is not necessarily named, and may contain strings as well as
      # numbers. TODO How to check its content?

      private$url <- url
      private$chomp_extra_slashes <- chomp_extra_slashes

      # Set parameters
      if (is.list(params))
        params <- unlist(params)
      if (! is.character(params)) {
        names <- names(params)
        params <- as.character(params)
        names(params) <- names
      }
      private$params <- params

      return(invisible(NULL))
    },

    #' @description
    #' Etracts the domain name from the URL.
    #'
    #' @return The domain.
    #'
    #' @examples
    #' # Create a URL object
    #' url <- sched::URL$new("https://www.my.server/",
    #'                       c(param1=12, param2='abc'))
    #'
    #' # Extract the domain name
    #' print(url$getDomain())
    getDomain = function() {

      domain <- sub("^.+://([^/]+)(/.*)?$", "\\1", private$url[[1]],
                    perl = TRUE)

      return(domain)
    },

    #' @description
    #' Sets the base URL string.
    #'
    #' @param url The base URL string.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create an empty URL object
    #' url <- sched::URL$new()
    #'
    #' # Set the URL
    #' url$setUrl('https://www.my.server/')
    #'
    #' # Convert the URL to a string
    #' print(url$toString())
    setUrl = function(url) {

      private$url <- url

      return(invisible(NULL))
    },

    #' @description
    #' Sets a parameter.
    #'
    #' @param key The parameter name.
    #' @param value  The value of the parameter.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create an URL object
    #' url <- sched::URL$new('https://www.my.server/')
    #'
    #' # Set a parameter
    #' url$setParam('a', 12)
    #'
    #' # Convert the URL to a string
    #' print(url$toString())
    setParam = function(key, value) {

      private$params[[key]] <- value

      return(invisible(NULL))
    },

    #' @description
    #' Displays information about this instance.
    #'
    #' @return self as invisible.
    #'
    #' @examples
    #' # Create an URL object
    #' url <- sched::URL$new('https://www.my.server/')
    #'
    #' # Print the URL object
    #' print(url)
    print = function() {
      cat(self$toString(), "\n", sep = "")
      return(invisible(self))
    },

    #' @description
    #' Gets the URL as a string representation.
    #'
    #' @param encode If set to TRUE, then encodes the URL.
    #' @return The URL as a string, with all parameters and values set.
    #'
    #' @examples
    #' # Create an URL object
    #' url <- sched::URL$new('https://www.my.server/', c(a=12))
    #'
    #' # Convert the URL to a string
    #' print(url$toString())
    toString = function(encode = TRUE) {

      u <- private$url

      # Remove '/' at start and end of each element of the URL
      if (private$chomp_extra_slashes)
        u <- gsub("^/*([^/].*[^/])/*$", "\\1", u)

      # Concatenate URL elements together
      u <- paste(u, collapse = "/")

      # Encode URL part
      if (encode)
        u <- utils::URLencode(u)

      # Add parameters to URL
      if (length(private$params) > 0) {

        pn <- names(private$params)
        pv <- unname(private$params)

        # Build parameters string
        fct <- function(i) {
          if (is.null(pn) || nchar(pn[[i]]) == 0) {
            if (encode) utils::URLencode(pv[[i]], reserved = TRUE) else pv[[i]]
          } else {
            if (encode) paste(utils::URLencode(pn[[i]], reserved = TRUE),
                              utils::URLencode(pv[[i]], reserved = TRUE),
                              sep = "=") else paste(pn[[i]], pv[[i]], sep = "=")
          }
        }
        kv_list <- vapply(seq_along(pv), fct, FUN.VALUE = "")
        params_str <- paste(kv_list, collapse = "&")

        # Concatenate URL with parameters
        u <- paste(u, params_str, sep = "?")
      }

      return(u)
    }
  ),

  private = list(
    url = NULL,
    params = NULL,
    chomp_extra_slashes = NULL
  )
)
