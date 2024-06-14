testthat::context("URL class")

testthat::test_that("URL class works fine.", {

  # Simple URL
  url <- sched::URL$new(url = "https://www.somesite.fr")
  testthat::expect_equal(url$toString(), "https://www.somesite.fr")
  url <- sched::URL$new(url = "https://www.somesite.fr/")
  testthat::expect_equal(url$toString(), "https://www.somesite.fr")
  url <- sched::URL$new(url = c("https://www.somesite.fr", ""))
  testthat::expect_equal(url$toString(), "https://www.somesite.fr/")

  # URL in multiple parts
  url <- sched::URL$new(url = c("https://www.somesite.fr/", "some", "page"))
  testthat::expect_equal(url$toString(), "https://www.somesite.fr/some/page")
  url <- sched::URL$new(url = c("https://www.somesite.fr//", "some", "/page/"))
  testthat::expect_equal(url$toString(), "https://www.somesite.fr/some/page")

  # With an unnamed parameter in a character vector
  url <- sched::URL$new(url = "https://www.somesite.fr/somepage",
                        params = c("rerun"))
  testthat::expect_equal(url$toString(),
                         "https://www.somesite.fr/somepage?rerun")

  # With a parameter in a character vector
  url <- sched::URL$new(url = "https://www.somesite.fr/somepage",
                        params = c(format = "txt"))
  testthat::expect_equal(url$toString(),
                         "https://www.somesite.fr/somepage?format=txt")

  # With a parameter in a numeric vector
  url <- sched::URL$new(url = "https://www.somesite.fr/somepage",
                        params = c(limit = 2))
  testthat::expect_equal(url$toString(),
                         "https://www.somesite.fr/somepage?limit=2")

  # With two parameters in a character vector
  url <- sched::URL$new(url = "https://www.somesite.fr/somepage",
                        params = c(format = "txt", limit = "2"))
  testthat::expect_equal(url$toString(),
                         "https://www.somesite.fr/somepage?format=txt&limit=2")

  # With a parameter in a list
  url <- sched::URL$new(url = "https://www.somesite.fr/somepage",
                        params = list(format = "txt"))
  testthat::expect_equal(url$toString(),
                         "https://www.somesite.fr/somepage?format=txt")

  # With two parameters in a list
  url <- sched::URL$new(url = "https://www.somesite.fr/somepage",
                        params = list(format = "txt", limit = 2))
  ref_url <- "https://www.somesite.fr/somepage?format=txt&limit=2"
  testthat::expect_equal(url$toString(), ref_url)
})

testthat::test_that("URL encoding works correctly.", {

  testthat::expect_equal(sched::URL$new("my.site.com")$toString(),
                         "my.site.com")
  testthat::expect_equal(sched::URL$new("my.site.com", c("a"))$toString(),
                         "my.site.com?a")
  testthat::expect_equal(sched::URL$new("my.site.com", c("a" = 2))$toString(),
                         "my.site.com?a=2")
  testthat::expect_equal(
    sched::URL$new("my.site.com", c("a" = 2, "n"))$toString(),
    "my.site.com?a=2&n"
  )
  url_w_space <- sched::URL$new("my.site.com", c("a b" = 2, "n"))
  testthat::expect_equal(url_w_space$toString(), "my.site.com?a%20b=2&n")
  testthat::expect_equal(url_w_space$toString(FALSE), "my.site.com?a b=2&n")
  url_w_space <- sched::URL$new("my.site.com", c("a b" = "my value", "n"))
  testthat::expect_equal(url_w_space$toString(),
                         "my.site.com?a%20b=my%20value&n")
  testthat::expect_equal(url_w_space$toString(FALSE),
                         "my.site.com?a b=my value&n")
  testthat::expect_equal(sched::URL$new("my site")$toString(), "my%20site")
  testthat::expect_equal(sched::URL$new("my site")$toString(FALSE), "my site")

  param_w_bracket <- sched::URL$new("my.site.com", c("b" = "[0 TO 2]"))
  testthat::expect_equal(param_w_bracket$toString(),
                         "my.site.com?b=%5B0%20TO%202%5D")
})

testthat::test_that("getDomain() works fine", {
  domain <- "www.mon.domain.fr"
  url <- sched::URL$new(paste0("http://", domain, "/"))
  testthat::expect_equal(url$getDomain(), domain)
})

testthat::test_that("setUrl() works fine", {
  url <- sched::URL$new()
  s <- "www.my.address.com"
  url$setUrl(s)
  testthat::expect_equal(url$toString(), s)
})

testthat::test_that("setParam() works fine", {
  s <- "www.my.address.com"
  url <- sched::URL$new(s)
  url$setParam("a", 12)
  testthat::expect_equal(url$toString(), paste(s, "a=12", sep = "?"))
})

testthat::test_that("print() works fine", {
  testthat::expect_output(print(sched::URL$new("www.my.address.com",
                                               c(a = 12))),
                          "^www\\.my\\.address.com\\?a=12$")
})
