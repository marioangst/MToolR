library(MToolR)

# this reads an example MToolR export provided by Karlijn for which IDs have been reshuffled to anonymize it
# and saves it as a readable MtoolR object to data/ and as a csv file to inst/extdata

example_models <-
  MToolR::mentalmodel_from_csv("data-raw/example_export.csv",
                               exclude_nonresponse = TRUE)
file.copy(from = "data-raw/example_export.csv",
          to = "inst/extdata/example_export.csv",
          overwrite = TRUE)
file.copy(from = "data-raw/example_export.csv",
          to = "tests/testthat/example_export.csv",
          overwrite = TRUE)
usethis::use_data(example_models, overwrite = TRUE)
