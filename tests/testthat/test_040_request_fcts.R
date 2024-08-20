testthat::context("Request functions")

testthat::test_that("we can handle unresolved host.", {

  # Define unexistent URL
  u <- "https://my.host.that.does.not.exists/"

  # Error must be returned on sending request
  testthat::expect_error(sched:::get_rcurl_content(u))

  # In `fail = FALSE` mode, we must get NULL
  content <- sched:::get_rcurl_content(u, fail = FALSE)
  testthat::expect_equal(content, NULL)
})

testthat::test_that("get_rcurl_content() works fine.", {

  u <- paste0("https://www.ebi.ac.uk/webservices/chebi/2.0/test/",
              "getCompleteEntity?chebiId=17001")
  content <- sched:::get_rcurl_content(u, fail = FALSE)
  testthat::expect_true(is.character(content) || is.null(content))
  if (! is.null(content)) {
    testthat::expect_true(length(content) == 1)
    testthat::expect_true(nchar(content) > 0)
  }
})

testthat::test_that("Binary mode works in get_rcurl_content().", {
  u <- paste0("https://gitlab.com/api/v4/projects/",
              "cnrgh%2Fdatabases%2Fr-sched/repository/archive.tar.bz2")
  content <- sched:::get_rcurl_content(u, binary = TRUE, fail = FALSE)
  testthat::expect_true(is.raw(content) || is.null(content))
  if (! is.null(content))
    testthat::expect_true(length(content) > 0)
})

testthat::test_that("Post request works in get_rcurl_content().", {
  u <- "https://httpbin.org/post"
  content <- sched:::get_rcurl_content(u, method = "post", fail = FALSE)
  testthat::expect_true(is.character(content) || is.null(content))
  if (! is.null(content))
    testthat::expect_true(length(content) > 0)
})

testthat::test_that("get_url_request_result() works fine.", {
  u <- sched::URL$new("https://cran.r-project.org/")# nolint: object_name_linter
  result <- sched::get_url_request_result(sched::Request$new(u))
  testthat::expect_is(result, "RequestResult")
  content <- result$getContent()
  testthat::expect_true(is.character(content) || is.null(content))
  if (! is.null(content))
    testthat::expect_true(nchar(content) > 0)
})

testthat::test_that("Binary mode works in get_base_url_content()", {
  u <- "https://httpbin.org/image/jpeg"
  content <- sched:::get_base_url_content(u, binary = TRUE)
  testthat::expect_true(is.raw(content) || is.null(content))
  if (! is.null(content))
    testthat::expect_true(length(content) > 0)
})

testthat::test_that("make_post_request() works fine.", {
  entry_ids <- c("AU380004", "EA256108")
  request <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<SOAP-ENV:Envelope",
    "xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"",
    " xmlns:tns=\"http://api.massbank\"><SOAP-ENV:Body><tns:getRecordInfo>",
    paste(paste("<tns:entry.ids>", entry_ids, "</tns:entry.ids>", sep = ""),
          collapse = ""),
    "</tns:getRecordInfo></SOAP-ENV:Body></SOAP-ENV:Envelope>"
  )

  u <- sched::URL$new(c("https://massbank.eu/api/services",
                        "MassBankAPI.MassBankAPIHttpSoap11Endpoint/"))

  request <- sched::make_post_request(u, body = request, mime = "text/xml")
  testthat::expect_is(request, "Request")
})

testthat::test_that("make_post_request() accepts a SOAP action.", {
  u <- sched::URL$new("https://my.url/to/soap/service")
  request <- sched::make_post_request(u, body = "", mime = "application/json",
                                      soap_action = "my.action")
  testthat::expect_is(request, "Request")
})

testthat::test_that("get_base_url_request_result() accepts only GET method.", {

  # Build POST request
  u <- sched::URL$new("https://httpbin.org/post")
  request <- sched::make_post_request(u, body = "", mime = "application/json")
  res <- get_base_url_request_result(request)
  testthat::expect_is(res, "RequestResult")
  testthat::expect_equal(res$getStatus(), 404)
  testthat::expect_null(res$getContent())
})

testthat::test_that("try_get_rcurl_content() handles errors.", {

  # Wrong URL (==> GenericCurlError)
  ua <- paste0("r-sched ; pierrick", ".roger", "@", "cea.fr")
  u <- sched::URL$new("https://wrongsite.foo")
  res <- sched:::try_get_rcurl_content(sched::Request$new(u), useragent = ua,
                                       ssl_verifypeer = TRUE, binary = FALSE)
  testthat::expect_false(is.null(res$curl_error))
})

testthat::test_that("We can build and send a POST request.", {
  u <- "https://httpbin.org/anything"
  my_url <- sched::URL$new(u)
  b <- "{\"some_key\": \"my_value\"}"
  my_request <- sched::make_post_request(my_url, body = b,
                                         mime = "application/json")
  testthat::expect_is(my_request, "Request")
  res <- sched::get_url_request_result(my_request)
  testthat::expect_is(res, "RequestResult")
})

testthat::test_that("chk_status_in_curl_result() works fine.", {

  # Curl error
  res <- sched:::chk_status_in_curl_result(list(curl_error = "My error"))
  testthat::expect_equal(res$err_msg, "RCurl error: My error")

  # Simulate status 0
  hdr <- list(value = function() list(status = "0"))
  res <- sched:::chk_status_in_curl_result(list(header = hdr))
  testthat::expect_equal(res$err_msg, "Cannot find status info in HTTP header.")

  # Simulate error
  hdr <- list(value = function() stop("wrong"))
  res <- sched:::chk_status_in_curl_result(list(header = hdr))
  testthat::expect_match(res$err_msg,
                         "^Error while retrieving HTTP header:.*\\.$")

  # Simulate warning
  hdr <- list(value = function() warning("wrong"))
  res <- sched:::chk_status_in_curl_result(list(header = hdr))
  testthat::expect_match(res$err_msg,
                         "^Warning while retrieving HTTP header:.*\\.$")
})
