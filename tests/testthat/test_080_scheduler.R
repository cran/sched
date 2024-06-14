testthat::context("Scheduler class")

wrk_dir <- tempdir()
cache_dir <- file.path(wrk_dir, "cache")
unlink(cache_dir, recursive = TRUE)

testthat::test_that("We can add a rule", {

  # Get the scheduler
  scheduler <- sched::Scheduler$new(cache_dir = cache_dir)
  testthat::expect_is(scheduler, "Scheduler")

  # Check number of rules
  testthat::expect_equal(scheduler$getNbRules(), 0)

  # Create a rule
  host <- "www.ebi.ac.uk"
  scheduler$setRule(host)
  testthat::expect_equal(scheduler$getNbRules(), 1)

  # Reset the rule
  scheduler$setRule(host, n = 5)
  testthat::expect_equal(scheduler$getNbRules(), 1)

  # Delete rules
  scheduler$deleteRules()
  testthat::expect_equal(scheduler$getNbRules(), 0)
})

testthat::test_that("We can add two rules", {

  # Get the scheduler
  scheduler <- sched::Scheduler$new(cache_dir = cache_dir)
  testthat::expect_is(scheduler, "Scheduler")

  # Create first rule
  host1 <- "www.ebi.ac.uk"
  scheduler$setRule(host1)
  testthat::expect_equal(scheduler$getNbRules(), 1)

  # Create second rule
  host2 <- "www2.ebi.ac.uk"
  scheduler$setRule(host2)
  testthat::expect_equal(scheduler$getNbRules(), 2)

  # Delete rules
  scheduler$deleteRules()
  testthat::expect_equal(scheduler$getNbRules(), 0)
})

testthat::test_that("We can send a direct request to ChEBI.", {

  # Get the scheduler
  scheduler <- sched::Scheduler$new(cache_dir = cache_dir)
  testthat::expect_is(scheduler, "Scheduler")

  # Create URL object
  u <- "https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity"
  url <- sched::URL$new(url = u, params = list(chebiId = 15440))

  # Create a request object
  request <- sched::Request$new(method = "get", url = url)

  # Send request
  result <- scheduler$sendRequest(request)
  testthat::expect_is(result, "character")
  testthat::expect_length(result, 1)
  testthat::expect_false(is.na(result))
})

testthat::test_that("We can send a direct request to UniProt.", {

  # Get the scheduler
  scheduler <- sched::Scheduler$new(cache_dir = cache_dir)
  testthat::expect_is(scheduler, "Scheduler")

  # Create URL object
  u <- "https://rest.uniprot.org/uniprotkb/search"
  p <- list(query = "e", fields = "id", format = "tsv", size = 2)
  url <- sched::URL$new(url = u, params = p, chomp_extra_slashes = FALSE)

  # Create a request object
  request <- sched::Request$new(method = "get", url = url)

  # Send request
  result <- scheduler$sendRequest(request)
  testthat::expect_is(result, "character")
  testthat::expect_length(result, 1)
  testthat::expect_false(is.na(result))
})

testthat::test_that("We can handle correctly a wrong URL.", {

  # Get the scheduler
  sched <- sched::Scheduler$new(cache_dir = cache_dir)
  testthat::expect_is(sched, "Scheduler")

  # Create URL object with a WRONG URL
  u <- "https://gitlab.com/cnrgh/databases/r-sched/-/raw/main/README.txt"
  u <- sched::URL$new(url = u)

  # Create a request object
  request <- sched::Request$new(method = "get", url = u)

  # Send request
  result <- sched$sendRequest(request)
  testthat::expect_is(result, "character")
  testthat::expect_length(result, 1)
  testthat::expect_true(is.na(result))
})

testthat::test_that("We can download a file.", {
  scheduler <- sched::Scheduler$new(cache_dir = cache_dir)
  u <- sched::URL$new(
    "https://gitlab.com/cnrgh/databases/r-sched/-/raw/main/README.md",
    c(ref_type = "heads")
  )
  out_file <- file.path(wrk_dir, "README.md")
  scheduler$downloadFile(u, out_file)
  testthat::expect_true(file.exists(out_file))

  # Use a different time-out
  scheduler$downloadFile(u, out_file, timeout = 2)

  # Use a deep path (folders should be created)
  out_file <- file.path(wrk_dir, "abc", "def", "README.md")
  scheduler$downloadFile(u, out_file)
  testthat::expect_true(file.exists(out_file))
})

testthat::test_that("Offline mode forbids connection.", {
  scheduler <- sched::Scheduler$new(cache_dir = cache_dir)
  scheduler$setOffline(TRUE)
  u <- "https://gitlab.com/cnrgh/databases/r-sched/-/raw/main/README.md"
  u <- sched::URL$new(url = u)
  request <- sched::Request$new(method = "get", url = u)
  testthat::expect_error(scheduler$sendRequest(request),
                         "^Attempting a connection while offline mode.*$")
})

testthat::test_that("Use twice same domain.", {
  scheduler <- sched::Scheduler$new(cache_dir = cache_dir)
  u <- "https://gitlab.com/cnrgh/databases/r-sched/-/raw/main/README.md"
  u <- sched::URL$new(url = u)
  request <- sched::Request$new(method = "get", url = u)
  result <- scheduler$sendRequest(request)
  testthat::expect_is(result, "character")
  testthat::expect_length(result, 1)
  testthat::expect_false(is.na(result))
  testthat::expect_equal(scheduler$getNbRules(), 1)
  u <- "https://gitlab.com/cnrgh/databases/r-sched/-/raw/main/DESCRIPTION"
  u <- sched::URL$new(url = u)
  request <- sched::Request$new(method = "get", url = u)
  result <- scheduler$sendRequest(request)
  testthat::expect_is(result, "character")
  testthat::expect_length(result, 1)
  testthat::expect_false(is.na(result))
  testthat::expect_equal(scheduler$getNbRules(), 1)
})

testthat::test_that("getUrl() works fine.", {
  scheduler <- sched::Scheduler$new(cache_dir = cache_dir)
  u <- "https://gitlab.com/cnrgh/databases/r-sched/-/raw/main/README.md"
  testthat::expect_warning(result <- scheduler$getUrl(u), "^.* deprecated .*$")
  testthat::expect_is(result, "character")
  testthat::expect_length(result, 1)
  testthat::expect_false(is.na(result))
})

testthat::test_that("getUrl() works fine.", {
  scheduler <- sched::Scheduler$new(cache_dir = cache_dir)
  u <- "https://gitlab.com/cnrgh/databases/r-sched/-/raw/main/README.md"
  testthat::expect_warning(url_obj <- scheduler$getUrlString(u),
                           "^.* deprecated .*$")
  testthat::expect_is(url_obj, "character")
  testthat::expect_length(url_obj, 1)
  testthat::expect_false(is.na(url_obj))
})
