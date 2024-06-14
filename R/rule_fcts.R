compute_sleep_time <- function(n, lap, cur_time, times) {

  chk::chk_whole_number(n)
  chk::chk_true(n > 0)
  chk::chk_number(lap)
  chk::chk_true(lap > 0.0)

  sleep_time <- 0.0

  if (! is.null(times)) {

    # Remove all times older than lap
    olds <- (cur_time - times) >= lap
    times <- times[! olds]

    # Do we need to wait?
    if (length(times) >= n) {

      # Get the time of the nth element starting from last
      x <- times[length(times) - n + 1]

      # Compute needed waiting time
      sleep_time <- lap - (cur_time - x)
    }
  }

  return(sleep_time)
}
