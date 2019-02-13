#eq_read_data test
test_that("Chech the filename",{
  expect_that(eq_read_data("wrong_name"),throws_error())
})

#eq_clean_data test
context("eq_clean_data test")
test_that("eq_clean_data returns a Date column", {
  filename <- system.file("extdata","earthquakes_data.txt.zip",package="Capstone")

  A <- eq_clean_data(eq_read_data(filename))

  expect_equal(any(which(names(A)=="DATE")), TRUE)
})

#eq_location_clean test
context("eq_location_clean test")
test_that("eq_location_clean returns a LOCATION_NAME column", {
  filename <- system.file("extdata","earthquakes_data.txt.zip",package="Capstone")

  B <- eq_location_clean(eq_clean_data(eq_read_data(filename)))

  expect_equal(any(which(names(B)=="LOCATION_NAME")), TRUE)
})

#eq_map test
context("eq_map test")
test_that("eq_map return a leaflet map", {
  filename <- system.file("extdata","earthquakes_data.txt.zip",package="Capstone")

  expect_that(eq_location_clean(eq_clean_data(eq_read_data(filename))) %>%
                dplyr::filter(COUNTRY == "VENEZUELA" & lubridate::year(DATE) >= 2000) %>%
                eq_map(name_col = "LOCATION_NAME"), is_a("leaflet"))
})



