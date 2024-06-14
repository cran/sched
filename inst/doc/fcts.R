## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
my_url <- sched::URL$new("https://httpbin.org/anything")
my_request <- sched::make_post_request(my_url,
                                       body = "{\"some_key\": \"my_value\"}",
                                       mime = "application/json")

## -----------------------------------------------------------------------------
res <- sched::get_url_request_result(my_request)
res$getContent()

## -----------------------------------------------------------------------------
my_url <- sched::URL$new("https://httpbin.org/get")
my_request <- sched::Request$new(my_url)
sched::get_url_request_result(my_request)

