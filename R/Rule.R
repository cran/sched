#' Scheduling rule class.
#'
#' This class represents a scheduling rule, used to limit the number of events
#' during a certain lap of time.
#'
#' @examples
#' # Create a new Rule object:
#' rule <- sched::Rule$new(n=1,lap=0.2) # 1 event allowed each 2 seconds
#'
#' # Wait to be allowed to process with first event:
#' rule$wait() # The first event will be allowed directly, no waiting time.
#' # Process your first event here
#' rule$wait() # The second event will be delayed 0.2 seconds. This time
#'             # includes the time passed between the first call to wait() and
#'             # this one.
#' # Process your second event here
#'
#' @import R6
#' @import chk
#' @import lgr
#' @export
Rule <- R6::R6Class("Rule", # nolint: object_name_linter

  public = list(

    #' @description
    #' Initializer.
    #'
    #' @param n    Number of events during a time lap.
    #' @param lap  Duration of a time lap, in seconds.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a rule object with default parameters
    #' r <- Rule$new()
    #'
    #' # Create a rule object with 5 events allowed each second (default time)
    #' r2 <- Rule$new(5L)
    #'
    #' # Create a rule object with 5 events allowed each 3 seconds
    #' r3 <- Rule$new(5L, 3)
    initialize = function(n = 3L, lap = 1) {

      chk::chk_whole_number(n)
      chk::chk_number(lap)
      chk::chk_gt(n, 0)
      chk::chk_gt(lap, 0)
      private$n <- n
      private$lap <- lap

      return(invisible(NULL))
    },

    #' @description
    #' Gets the number of events allowed during a lap time.
    #'
    #' @return Returns the number of events as an integer.
    #'
    #' @examples
    #' r <- Rule$new()
    #'
    #' #' Get the allowed number of events for a rule
    #' print(r$getN())
    getN = function() {
      return(private$n)
    },

    #' @description
    #' Gets the lap time.
    #'
    #' The number of seconds during which N events are allowed.
    #'
    #' @return Returns Lap time as a numeric.
    #'
    #' @examples
    #' # Create a rule object with default parameters
    #' r <- Rule$new()
    #'
    #' #' Get the configured lap time for a rule
    #' print(r$getLapTime())
    getLapTime = function() {
      return(private$lap)
    },


    #' @description
    #' Displays information about this instance.
    #'
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a rule object with default parameters
    #' r <- Rule$new()
    #'
    #' # Print information about a rule object
    #' print(r)
    print = function() {

      cat("Scheduling rule instance.\n")
      cat("  Parameters are Lap time=", private$lap, " and N=",
          private$n, ".\n", sep = "")

      return(invisible(NULL))
    },

    #' @description
    #' Wait (sleep) until a new event is allowed.
    #'
    #' @param do_sleep Debug parameter that turns off the call to Sys.sleep().
    #' Use only for testing.
    #' @return The time passed to wait, in seconds.
    #'
    #' @examples
    #' # Create a rule object that allows 3 events each 0.02 seconds
    #' r <- Rule$new(3, 0.02)
    #'
    #' #' Loop for generating 20 events
    #' i <- 0 # event index
    #' while (i < 20) {
    #'   # Wait until next event is allowed
    #'   wait_time <- r$wait()
    #'   print(paste("We have waited", wait_time,
    #'     "second(s) and are now allowed to process event number", i))
    #'   i <- i + 1
    #' }
    wait = function(do_sleep = TRUE) {

      chk::chk_flag(do_sleep)

      # Store current time
      op <- options(digits.secs = 6)
      on.exit(options(op))
      cur_time <- Sys.time()
      options(op)

      # Compute sleep time
      sleep_time <- compute_sleep_time(private$n, private$lap, cur_time,
                                       private$times)

      # Sleep if needed
      if (sleep_time > 0.0) {
        lgr::get_logger("sched")$debug(sprintf("Wait %g second(s).",
                                               sleep_time))
        if (do_sleep)
          Sys.sleep(sleep_time)
      }

      # Store current time
      private$times <- if (is.null(private$times)) cur_time else
        c(private$times, cur_time)

      return(sleep_time)
    }
  ),

  private = list(
    n = NULL,
    lap = NULL,
    times = NULL
  )
)
