library(myhelpers)
context("Read NetCDF")
fcfiles <- get_fcfiles(source='demo')
tas <- read_ncdf(fcfiles)

test_that("Variants of time selection produce equivalent results", {
  expect_equal(read_ncdf(fcfiles, ti=1),
               read_ncdf(fcfiles, time=as.Date(paste0(2000:2002, '-06-30'))))
  expect_equal(read_ncdf(fcfiles[1], time=as.Date(c("2000-07-31"))),
               read_ncdf(fcfiles[1], ti=2))
  expect_equal(read_ncdf(fcfiles, time=as.Date(attr(tas, 'time')[3:11])),
               read_ncdf(fcfiles, tlim=as.Date(attr(tas, 'time')[c(3,11)])))
  expect_equal(read_ncdf(fcfiles, ti=1:4),
               read_ncdf(fcfiles))
  expect_equal(as.vector(read_ncdf(fcfiles, tlim=as.Date(c("2000-01-01", "2000-12-31")))),
               as.vector(read_ncdf(fcfiles[1])))

}
)

test_that("Time selection with empty set produces NULL", {
  expect_null(read_ncdf(fcfiles, ti=0))
  expect_null(read_ncdf(fcfiles[1], ti=0))
  expect_null(read_ncdf(fcfiles, tlim=as.Date('1999-01-01') + 0:1))
  expect_null(read_ncdf(fcfiles, time=as.Date(paste0(2000:2002, '-06-29'))))
})

test_that("Variants of spatial selection produce equivalent results", {
  expect_equal(read_ncdf(fcfiles[1], lon=attr(tas, 'lon')[2]),
               read_ncdf(fcfiles[1], loi=2))
  expect_equal(read_ncdf(fcfiles[1], lat=attr(tas, 'lat')[3]),
               read_ncdf(fcfiles[1], lai=3))
  expect_equal(read_ncdf(fcfiles[1], lon=attr(tas, 'lon')[2], lat=attr(tas, 'lat')[3]),
               read_ncdf(fcfiles[1], loi=2, lai=3))
  expect_equal(read_ncdf(fcfiles[1], lon=range(attr(tas, 'lon')[1:3])),
               read_ncdf(fcfiles[1], loi=1:3))
  expect_equal(read_ncdf(fcfiles[1], lat=range(attr(tas, 'lat')[2:3])),
               read_ncdf(fcfiles[1], lai=2:3))
  expect_equal(read_ncdf(fcfiles[1], lon=range(attr(tas, 'lon')[1:3]), lai=2:3),
               read_ncdf(fcfiles[1], loi=1:3, lat=range(attr(tas, 'lat')[2:3])))
})

test_that("Spatial selection with empty set produces NULL", {
  expect_null(read_ncdf(fcfiles[1], lon=c(30,90)))
  expect_null(read_ncdf(fcfiles[1], lat=c(-30,0)))
  expect_null(read_ncdf(fcfiles[1], lai=30))
  expect_null(read_ncdf(fcfiles[1], loi=0))
})

# test_that("Parallelization works", {
#   expect_equal(read_ncdf(fcfiles, n.cores = 1),
#                read_ncdf(fcfiles, n.cores = 3))
# })
