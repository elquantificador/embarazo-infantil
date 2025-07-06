library(readr)
library(haven)

read_env <- function(year) {
  path <- file.path("data", sprintf("ENV_%s", year))
  if (year >= 2018) {
    read_delim(paste0(path, ".csv"), delim = ";", locale = locale(encoding = "UTF-8"))
  } else {
    read_sav(paste0(path, ".sav"))
  }
}

years <- 2013:2022
cols_expected <- NULL
for (y in years) {
  df <- read_env(y)
  if (is.null(cols_expected)) {
    cols_expected <- names(df)
  } else if (!all(names(df) == cols_expected)) {
    stop(sprintf("Column mismatch on %s", y))
  }
}
cat("All files contain the same columns.\n")
