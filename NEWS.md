## myhelpers 0.0.0.9021

* fixed bug in `read_single` when reading with constraints on
only latitudes or longitudes

## myhelpers 0.0.0.9020

* Chanegd layout of main functions. `read_ncdf` is now used to
read either a collection or single file, `read_singl` is the 
underlying workhorse operating on netcdf

## myhelpers 0.0.0.9019

* Added function to read subsets of NetCDF files `read_ncdf`
* rewrote `read_forecasts` to harness the functionality of `read_ncdf`

## myhelpers 0.0.0.9018

* Added functionality to read in subsets of the forecast array
* Added functions to expand and shrink arrays based on `abind`

## myhelpers 0.0.0.9017

* Added support for operational forecasts to `read_forecast`

## myhelpers 0.0.0.9016

* Added flexibility to supply method string (partial or full) to `read_scores`

## myhelpers 0.0.0.9015

* added function to read forecasts

## myhelpers 0.0.0.9014

* restructure `read_scores` to minimize file open and close operations

## myhelpers 0.0.0.9012

* added cleaning up of rows with all values missing `read_scores`

## myhelpers 0.0.0.9001

* added function to create bubble plot and legends
