ons_post_processing <- list()

# ons_post_processing$MGDP <- function(edd_obj){
#   variable_df <- edd_obj$dimensions$variable %>%
#     tidyr::separate(col = name,
#                     into = c("description", "other"),
#                     sep = "[(]") %>%
#     tidyr::separate(col = other,
#                     into = c("period", "value"),
#                     sep = "[)] :") %>%
#     dplyr::mutate(dplyr::across(.fns = ~ stringr::str_trim(.x))) %>%
#     dplyr::mutate(description = toupper(description)) %>%
#     dplyr::left_join(readRDS("data/sic.rds") %>%
#                        dplyr::select(`Section Description`, SECTION) %>%
#                        dplyr::filter(!is.na(SECTION)) %>%
#                        unique(),
#                      by = c("description" = "Section Description")
#     ) %>%
#     dplyr::mutate(SECTION = dplyr::case_when(
#       description == "GROSS VALUE ADDED - MONTHLY"                                      ~ "Total",
#       description == "SERVICE INDUSTRIES - TOTAL"                                       ~ "G-T",
#       description == "PRODUCTION INDUSTRIES - TOTAL"                                    ~ "B-E",
#       description == "ACTIVITIES OF HOUSHOLDS AS EMPLOYERS;UNDIFF GOODS & SERVICES"     ~ "T",
#       description == "PUBLIC ADMIN AND DEFENCE; COMPULSORY SOCIAL SECURITY"             ~ "O",
#       description == "WATER SUPPLY; SEWERAGE,WASTE MANAGEMENT & REMEDIATION ACTIVITIES" ~ "E",
#       description == "WHOLESALE & RETAIL TRADE; REPAIR OF MOTOR VEHICLES/CYCLES"        ~ "G",
#       TRUE                                                                              ~ as.character(SECTION)
#     )
#     )
#
#   edd_obj$data <- edd_obj$data %>%
#     dplyr::left_join(dplyr::select(variable_df, code, SECTION), by = c("variable" = "code")) %>%
#     dplyr::rename(industry = SECTION)
#
#   edd_obj$dimensions$industry <- tibble::tibble(code = variable_df$SECTION,
#                                                name = variable_df$description) %>%
#     dplyr::distinct()
#
#   edd_obj$dimensions$variable <- variable_df %>%
#     dplyr::mutate(description = paste("GVA", value, period)) %>%
#     dplyr::rename(name = description) %>%
#     dplyr::select(-SECTION)
#
#   return(edd_obj)
# }

ons_post_processing$EMP <- function(x){
  x$dimensions$variable <- x$dimensions$variable %>%
    tidyr::separate(col= name, into = c("survey", "name", "value"), sep = ":") %>%
    dplyr::mutate(preunit = dplyr::case_when(
      stringr::str_detect(name, "[(]£[)]") ~ "£",
      stringr::str_detect(name, "[(]%[)]") ~ "%",
      TRUE ~ as.character(preunit))) %>%
    dplyr::mutate(name = ifelse(stringr::str_detect(name, "[(]£[)]"), stringr::str_sub(name, start = 1, end = -4), name)) %>%
    dplyr::mutate(name = ifelse(stringr::str_detect(name, "[(]%[)]"), stringr::str_sub(name, start = 1, end = -4), name)) %>%
    dplyr::mutate(name = stringr::str_trim(name)) %>%
    dplyr::mutate(name = stringr::str_trim(value))
  return(x)
}
