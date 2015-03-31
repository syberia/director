library(testthatsomemore)

touch_file <- function(file, time = "1 minute from now") {
  testthatsomemore::pretend_now_is(time, {
    Sys.setFileTime(file, Sys.time())
  })
}
