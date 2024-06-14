## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
scheduler <- sched::Scheduler$new(cache_dir = NULL,
                                  user_agent = "sched ; pierrick.roger@cea.fr")

## -----------------------------------------------------------------------------
my_url <- sched::URL$new(
  url = "https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity",
  params = c(chebiId = 15440)
)

my_request <- sched::Request$new(my_url)

## -----------------------------------------------------------------------------
content <- scheduler$sendRequest(my_request)

## -----------------------------------------------------------------------------
content

## -----------------------------------------------------------------------------
scheduler$setRule("www.ebi.ac.uk", n = 7, lap = 2)

## -----------------------------------------------------------------------------
scheduler$deleteRules()

## -----------------------------------------------------------------------------
my_temp_dir <- file.path(tempdir(), "my_temp_folder_for_sched_vignette")

## -----------------------------------------------------------------------------
my_url <- sched::URL$new(
  "https://gitlab.com/cnrgh/databases/r-sched/-/raw/main/README.md"
)
dst <- file.path(my_temp_dir, "readme.md")
scheduler$downloadFile(my_url, dest_file = dst)

## -----------------------------------------------------------------------------
unlink(my_temp_dir, recursive = TRUE)

