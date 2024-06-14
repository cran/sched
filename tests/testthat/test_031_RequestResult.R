testthat::context("RequestResult class")

testthat::test_that("We can define a default RequestResult object", {
  content <- ""
  res <- sched:::RequestResult$new(content = content)
  testthat::expect_is(res, "RequestResult")
  testthat::expect_equal(res$getContent(), content)
})

testthat::test_that("We can set all parameters of RequestResult.", {
  content <- "Blablabla"
  retry <- TRUE
  err_msg <- ""
  status <- 200
  status_msg <- "OK"
  retry_after <- "5s"
  location <- ""
  res <- sched:::RequestResult$new(content = content, retry = retry,
                                   err_msg = err_msg, status = status,
                                   status_msg = status_msg,
                                   retry_after = retry_after,
                                   location = location)
  testthat::expect_is(res, "RequestResult")
  testthat::expect_equal(res$getContent(), content)
  testthat::expect_equal(res$getRetry(), retry)
  testthat::expect_equal(res$getErrMsg(), err_msg)
  testthat::expect_equal(res$getStatus(), status)
  testthat::expect_equal(res$getRetryAfter(), retry_after)
  testthat::expect_equal(res$getLocation(), location)
})

testthat::test_that("Recoverable errors are recognized.", {
  content <- "Blablabla"
  recoverable <- c(302, 404, 408, 500, 503)
  for (status in c(600, unlist(sched:::http_status))) {
    res <- sched:::RequestResult$new(content = content, status = status,
                                     retry_after = "3s",
                                     location = "https://some.site/")
    res$processRequestErrors()
    if (status == 200) { # OK
      testthat::expect_false(res$getRetry())
      testthat::expect_equal(res$getContent(), content)
    } else if (status %in% recoverable) { # Recoverable
      testthat::expect_true(res$getRetry())
      testthat::expect_equal(res$getContent(), content)
    } else { # Unrecoverable
      testthat::expect_false(res$getRetry())
      testthat::expect_true(is.na(res$getContent()))
    }
  }
})

testthat::test_that("Proxy server error is handled.", {
  content <- "The proxy server could not handle the request"
  res <- sched:::RequestResult$new(content = content, status = 200)
  res$processRequestErrors()
  testthat::expect_true(is.na(res$getContent()))
  testthat::expect_false(res$getRetry())
})
