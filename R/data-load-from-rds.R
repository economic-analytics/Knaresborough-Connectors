dataFileLocation <- "data/datasets.knaresborough"

dataFiles <- list.files(dataFileLocation, full.names = TRUE)

edd_datasets <- lapply(dataFiles, \(x) readRDS(x)) |>
  setNames(tools::file_path_sans_ext(basename(dataFiles)))


################################################################

# # fedo_dict    <- readr::read_csv("data-raw/fedo_dict.csv")
#
# #ons_datasets <- readr::read_rds("data/ons_datasets.rds")
#
# #variables    <- readr::read_rds("data/variables.rds")
# #boundaries   <- readr::read_rds("data/boundaries_nuts1-2-3.rds")
# RGVA         <- readr::read_rds("data/datasets.knaresborough/RGVA.rds")
# #RGVAI        <- readr::read_rds("data/ons_rgvai.rds")
# BRES         <- readr::read_rds("data/datasets.knaresborough/BRES.rds")
# UKBC         <- readr::read_rds("data/datasets.knaresborough/UKBC.rds")
# #RGFCF        <- readr::read_rds("data/RGFCF.rds")
# CTSOP        <- readr::read_rds("data/datasets.knaresborough/CTSOP.rds")
# SAIE         <- readr::read_rds("data/datasets.knaresborough/SAIE.rds")
#
# sp_datasets  <- list(RGVA  = RGVA,
#                      #RGVAI = RGVAI,
#                      BRES  = BRES,
#                      UKBC  = UKBC,
#                      #RGFCF = RGFCF,
#                      CTSOP = CTSOP,
#                      SAIE = SAIE)
#
# datasets <- list(# ons_datasets = ons_datasets,
#   sp_datasets = sp_datasets)
#
# # this added to provide a second option - instead of two lists within a list
# # we provide a single all_datasets list which contains ons and spatial
# eddie_datasets <- c(# ons_datasets,
#   sp_datasets)
