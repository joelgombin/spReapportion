library(spReapportion)
# skip_on_cran()

test_that("spReapportion reapportions data correctly", {
  data(ParisPollingStations2012)
  data(ParisIris)
  data(RP_2011_CS8_Paris)
  CS_ParisPollingStations <- spReapportion(ParisIris, ParisPollingStations2012, RP_2011_CS8_Paris, "DCOMIRIS", "ID", "IRIS")
  expect_equal_to_reference(CS_ParisPollingStations, "CS_ParisPollingStations.rds")
})


