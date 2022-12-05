
# Object verification -----------------------------------------------------

is.edd_df <- function(x) {
  if ("edd_df" %in% class(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is.edd_ndf <- function(x) {
  if ("edd_ndf" %in% class(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is.edd_obj <- function(x) {
  if ("edd_obj" %in% class(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Object coercion ---------------------------------------------------------

as.edd_df <- function(x) {
  # test for conversion ability, i.e. verify names, types, data.frame
  UseMethod("as.edd_df")
}

as.edd_ndf <- function(x) {
  UseMethod("as.edd_ndf")
}

as.edd_obj <- function(x) {
  UseMethod("as.edd_obj")
}

# Object translation functions --------------------------------------------

df_to_edd_df <- function(x) {

}

edd_df_to_edd_ndf <- function(x) {

}

edd_ndf_to_edd_obj <- function(x) {

}

edd_obj_to_edd_ndf <- function(x) {

}

edd_ndf_to_edd_df <- function(x) {

}


# dplyr::filter() methods for EDD objects ---------------------------------



# S4 OOP ------------------------------------------------------------------

# fedobj <- structure(data.frame(
#   dates =
# ))
#
#
# setClass("fedobj", slots = c(data = "data.frame"))
# getClass("fedobj")
#
#
# setClass("fedobj",
#          slots = c(data = "data.frame",
#                    dimensions = "data.frame",
#                    meta = "data.frame"),
#          contains = "data.frame"
# )
#
# fo <- new("fedobj")
#
#
#
# setClass("EddieData",
#          slots = c(date = "character",
#                    variable = "character",
#                    value = "numeric"),
#          contains = "data.frame")
#
# setClass("EddieDataGeog",
#          slots = c(geography.code = "character",
#                    geography.name = "character"),
#          contains = "EddieData")
