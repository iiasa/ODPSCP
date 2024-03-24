# Create new package environment
# .pkgenv <- new.env(parent = emptyenv())

# For unloading a package
.onUnload <- function(libpath) {}

# What to do on attaching the package
.onAttach <- function(libname, pkgname) {
  # packageStartupMessage("############################")
  # packageStartupMessage("Loading OPSCP package ...")
  # packageStartupMessage("############################")

  # Don't show constant rgdal warning
  options("rgdal_show_exportToProj4_warnings" = "none")

  # Increase maximum upload size
  options(shiny.maxRequestSize = 30*1024^2)
}

