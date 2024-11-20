

download.file("http://fmwww.bc.edu/ec-p/data/wooldridge/barium.dta",
              destfile = "data-raw/barium.dta")

library(foreign)

barium <- read.dta("data-raw/barium.dta")

attributes(barium)$datalabel <- NULL
attributes(barium)$formats <- NULL
attributes(barium)$time.stamp <- NULL
attributes(barium)$types <- NULL
attributes(barium)$val.labels <- NULL
attributes(barium)$var.labels <- NULL
attributes(barium)$version <- NULL
row.names(barium) <- NULL

attributes(barium)

usethis::use_data(barium, overwrite = TRUE)
