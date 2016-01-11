library(myhelpers)
context("Date conversion from NetCDF files")

nc1 <- nc2 <- nc3 <- nc4 <- nc5 <- nc6 <- list(dim=list(time=NULL))
nc1$dim$time <- list(units='seconds since 1980-01-31 23:59:59',
                     vals=c(0, 1, (28:30)*86400))
nc2$dim$time <- list(units='minutes since 1980-01-31 23:59:59',
                     vals=c(0, 1/60, (28:30)*24*60))
nc3$dim$time <- list(units='hours since 1980-01-31 23:59:59',
                     vals=c(0, 1/3600, (28:30)*24))
nc4$dim$time <- list(units='days since 1980-01-31 23:59:59',
                     vals=c(0, 1/86400, 28:30))
nc5$dim$time <- list(units='months since 1980-01-31',
                     vals=0:24)
nc6$dim$time <- list(units='years since 1980-02-29',
                     vals=0:5)

test_that("Equality of time specifications", {
  expect_equal(nc_time(nc1), nc_time(nc2))
  expect_equal(nc_time(nc1), nc_time(nc3))
  expect_equal(nc_time(nc1), nc_time(nc4))
})

test_that("leap years are dealt with correctly",{
  expect_true(all(day(nc_time(nc5)) %in% 28:31))
  expect_true(all(day(nc_time(nc6)) %in% 28:29))
})
