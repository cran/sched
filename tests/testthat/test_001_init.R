testthat::context("Logger setting")

# Turn off default appender
lgr::lgr$appenders$console$set_threshold("off")

# Define log file
log_file <- file.path(getwd(), "..", "..", "tests.log")
if (file.exists(log_file))
  unlink(log_file)

# Define layout
layout_format <- lgr::LayoutFormat$new("%g.%L[%t][%c] %m")

# Create new appender
app <- lgr::AppenderFile$new(log_file, layout = layout_format)
app$set_threshold(NA)

# Declare appender
lgr::lgr$add_appender(app, "tests")

# Log everything
lgr::lgr$set_threshold(NA)
