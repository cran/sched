testthat::context("Request class")

testthat::test_that("We can build a Request object.", {

  testthat::expect_error(sched::Request$new())
  testthat::expect_error(sched::Request$new("http://www.my.server/"))
  testthat::expect_is(
    sched::Request$new(sched::URL$new("http://www.my.server/")),
    "Request"
  )
  testthat::expect_error(
    sched::Request$new(sched::URL$new("http://www.my.server/"), method = "z")
  )
  testthat::expect_is(
    sched::Request$new(sched::URL$new("http://www.my.server/"),
                       method = "get"),
    "Request"
  )

  # Test POST method
  testthat::expect_is(
    sched::Request$new(sched::URL$new("http://www.my.server/"),
                       method = "post"),
    "Request"
  )
  testthat::expect_is(
    sched::Request$new(
      url = sched::URL$new(c("https://api.rsc.org/compounds/v1/", "records",
                             "batch")),
      method = "post",
      header = c("Content-Type" = "", apikey = "my-token"),
      body = paste("{\"recordIds\": [2],",
                   "\"fields\": [\"SMILES\",\"Formula\",\"InChI\"]}")
    ),
    "Request"
  )

  # Test encoding
  testthat::expect_is(
    sched::Request$new(sched::URL$new("http://www.my.server/"),
                       encoding = "UTF-8"),
    "Request"
  )
})

testthat::test_that("getUrl() works fine.", {
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url)
  testthat::expect_equal(request$getUrl(), url)
})

testthat::test_that("getMethod() works fine.", {

  # Test GET
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url)
  testthat::expect_equal(request$getMethod(), "get")

  # Test POST
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url, method = "post")
  testthat::expect_equal(request$getMethod(), "post")
})

testthat::test_that("getEncoding() works fine.", {

  # Default encoding
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url)
  testthat::expect_null(request$getEncoding())

  # UTF-8 encoding
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url, encoding = "UTF-8")
  testthat::expect_equal(request$getEncoding(), "UTF-8")
})

testthat::test_that("getCurlOptions() works fine.", {

  # GET
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url)
  testthat::expect_type(request$getCurlOptions(), "list")
  testthat::expect_equal(class(request$getCurlOptions()), "CURLOptions")

  # POST
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url, method = "post",
                                header = c(a = "a", b = "b"),
                                body = "abcdef")
  testthat::expect_type(request$getCurlOptions(), "list")
  testthat::expect_equal(class(request$getCurlOptions()), "CURLOptions")
})

testthat::test_that("getUniqueKey() works fine.", {
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url)
  md5 <- request$getUniqueKey()
  testthat::expect_type(md5, "character")
  testthat::expect_equal(class(md5), c("hash", "md5"))
  testthat::expect_equal(as.character(request$getUniqueKey()),
                         "3547e3daadd4bc704999443925ff5f4a")
})

testthat::test_that("getHeaderAsSingleString() works fine.", {
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url, method = "post",
                                header = c(a = "a", b = "b"), body = "abcdef")
  testthat::expect_equal(request$getHeaderAsSingleString(), "a=a, b=b")
})

testthat::test_that("getBody() works fine.", {
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url, method = "post",
                                header = c(a = "a", b = "b"), body = "abcdef")
  testthat::expect_equal(request$getBody(), "abcdef")
})

testthat::test_that("getBody() works fine.", {
  url <- sched::URL$new("http://www.my.server/")
  request <- sched::Request$new(url)
  testthat::expect_output(print(request),
                          "^GET .* http://www\\.my\\.server$")
})
