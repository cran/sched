testthat::context("Rule class")

test_new <- function() {

  rule <- sched::Rule$new()
  testthat::expect_equal(rule$getN(), 3)
  testthat::expect_equal(rule$getLapTime(), 1)

  rule <- sched::Rule$new(10)
  testthat::expect_equal(rule$getN(), 10)
  testthat::expect_equal(rule$getLapTime(), 1)

  rule <- sched::Rule$new(lap = 5)
  testthat::expect_equal(rule$getN(), 3)
  testthat::expect_equal(rule$getLapTime(), 5)
}

test_print <- function() {
  testthat::expect_output(print(sched::Rule$new()),
                          "^Scheduling rule instance.*$")
}

testthat::test_that("We can create an object", test_new())
testthat::test_that("print() works fine", test_print())

testthat::test_that("compute_sleep_time() works fine", {
  testthat::expect_error(compute_sleep_time(0, 0.0, 0.0, NULL))
  testthat::expect_error(compute_sleep_time(0, 0.0, 0.0, c()))
  testthat::expect_error(compute_sleep_time(0, 0.0, 0.0, c(0.0)))
  testthat::expect_error(compute_sleep_time(1, 0.0, 0.0, c(0.0)))
  testthat::expect_equal(compute_sleep_time(1, 0.1, 0.0, NULL), 0.0)
  testthat::expect_equal(compute_sleep_time(1, 0.1, 0.0, c()), 0.0)
  testthat::expect_equal(compute_sleep_time(1, 0.1, 0.0, c(0.0)), 0.1)
  testthat::expect_equal(compute_sleep_time(1, 0.1, 0.3, c(0.0)), 0.0)
  testthat::expect_equal(compute_sleep_time(2, 0.5, 0.1, c(0.0)), 0.0)
  testthat::expect_equal(compute_sleep_time(2, 0.5, 0.2, c(0.0, 0.1)), 0.3)

  for (n in c(1L, 3L, 5L)) {
    for (lap in c(0.01, 0.1, 1.0)) {
      testthat::expect_equal(compute_sleep_time(n, lap, 0.0, rep(0.0, n)),
                             lap)
      testthat::expect_equal(compute_sleep_time(n, lap, lap, rep(0.0, n)),
                             0.0)
      testthat::expect_equal(compute_sleep_time(n, lap, 2 * lap,
                                                rep(0.0, n)),
                             0.0)
      testthat::expect_equal(compute_sleep_time(n, lap, lap / 2,
                                                rep(0.0, n)),
                             lap / 2)
    }
  }
})

testthat::test_that("wait() works fine", {

  # Loop on various number of events
  for (n in c(1L, 3L, 5L)) {

    # Loop on various lap times
    for (lap in c(0.01, 0.1, 1.0)) {

      # Create a new rule object
      rule <- Rule$new(n = n, lap = lap)

      # Wait n times
      x <- Sys.time()
      for (i in seq(n)) {
        testthat::expect_equal(rule$wait(), 0.0)
      }

      # On the n+1 time, wait() should return a non-zero value, unless the
      # value of lap is too small
      sleep_time <- rule$wait(do_sleep = FALSE)
      y <- Sys.time()

      # On the n+1 time, the computed sleep time must be non-zero
      diff_time <- y - x
      if (diff_time < lap)
        testthat::expect_gt(sleep_time, 0.0)

      # The amount of sleep time must be lower than a lap time
      testthat::expect_true(!!sleep_time < !!lap)
    }
  }
})

testthat::test_that("Sys.sleep() inside wait() works fine", {
  rule <- Rule$new(n = 1, lap = 0.01)
  testthat::expect_equal(rule$wait(), 0.0)
  testthat::expect_true(rule$wait() > 0.0)
})
