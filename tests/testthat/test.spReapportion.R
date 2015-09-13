library(spReapportion)


test_that("spReapportion reapportions data correctly", {
  skip_on_cran()
  data(ParisPollingStations2012)
  data(ParisIris)
  data(RP_2011_CS8_Paris)
  CS_ParisPollingStations <- spReapportion(ParisIris, ParisPollingStations2012, RP_2011_CS8_Paris, "DCOMIRIS", "ID", "IRIS")
  expect_equal_to_reference(CS_ParisPollingStations, "CS_ParisPollingStations.rds")
})

test_that("spReapportion reapportions data correctly with proportions data", {
  skip_on_cran()
  data(ParisPollingStations2012)
  data(ParisIris)
  data(RP_2011_CS8_Paris)
  RP_2011_CS8_Paris[, paste0("CS", 1:8, "pc")] <- RP_2011_CS8_Paris[, paste0("C11_POP15P_CS", 1:8)] / RP_2011_CS8_Paris$C11_POP15P * 100
  CS_ParisPollingStationsProp <- spReapportion(ParisIris, ParisPollingStations2012, RP_2011_CS8_Paris, "DCOMIRIS", "ID", "IRIS", variables = paste0("CS", 1:8, "pc"), weights = "C11_POP15P")
  expect_equal_to_reference(CS_ParisPollingStationsProp, "CS_ParisPollingStationsprop.rds")
})


test_that("NA values are handled correctly", {
  data(ParisPollingStations2012)
  data(ParisIris)
  data(RP_2011_CS8_Paris)

  CS_ParisPollingStations <- spReapportion(ParisIris[ParisIris@data$DEPCOM %in% "75104",], ParisPollingStations2012[ParisPollingStations2012@data$arrondisse %in% "4",], RP_2011_CS8_Paris[substr(RP_2011_CS8_Paris$IRIS, 1, 5) %in% "75104",], "DCOMIRIS", "ID", "IRIS")
  expect_equal(sum(is.na(CS_ParisPollingStations)), 0)

  RP_2011_CS8_Paris[RP_2011_CS8_Paris$IRIS %in% "751041399", -1] <- NA

  CS_ParisPollingStations <- spReapportion(ParisIris[ParisIris@data$DEPCOM %in% "75104",], ParisPollingStations2012[ParisPollingStations2012@data$arrondisse %in% "4",], RP_2011_CS8_Paris[substr(RP_2011_CS8_Paris$IRIS, 1, 5) %in% "75104",], "DCOMIRIS", "ID", "IRIS")
  expect_equal(sum(is.na(CS_ParisPollingStations)), 0)
})
