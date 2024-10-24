
.le <- new.env() # parent = emptyenv()

# #' @importFrom log4r warn error info debug logger console_appender
NULL

init_logger <- function() {
  LEVEL_NAMES <- c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  verbosity <- Sys.getenv("GHQC_VERBOSE", unset = "INFO")
  if (!(verbosity %in% LEVEL_NAMES)){
    cat("Invalid verbosity level. Available options are:", paste(LEVEL_NAMES, collapse = ", "), "\n")
  }

  appenders_in <- log4r::console_appender(my_layout)
  logger <- log4r::logger(verbosity, appenders = appenders_in)
  assign("logger", logger, envir = .le)

  # log for logger
  #other <- ifelse(verbosity == "INFO", "DEBUG", "INFO")
  #info(.le$logger, glue::glue("logger level set to {verbosity}. Use ghqc_toggle_logger() to change to {other}"))
  # if (verbosity == "INFO") {
  #   info(.le$logger, glue::glue("logger level set to INFO. Use Sys.setenv(\"GHQC_VERBOSE\" = \"DEBUG\") to change to DEBUG mode"))
  # }
  # if (verbosity == "DEBUG") {
  #   info(.le$logger, glue::glue("logger level set to DEBUG. Use Sys.setenv(\"GHQC_VERBOSE\" = \"INFO\") to change to INFO mode"))
  # }

}

my_layout <- function(level, ...) {
  paste0(format(Sys.time()), " [", level, "] ", ..., "\n", collapse = "")
}

# ghqc_toggle_logger <- function() {
#   verbosity <- Sys.getenv("GHQC_VERBOSE")
#
#   ifelse(verbosity != "DEBUG",
#          Sys.setenv("GHQC_VERBOSE" = "DEBUG"),
#          Sys.setenv("GHQC_VERBOSE" = "INFO"))
#
#   init_logger()
#
# }


