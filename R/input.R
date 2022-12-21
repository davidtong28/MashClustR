#' Read in a mash pairwise distance file, parses it and save in variable "mash_matrix" in the global environment.
#'
#' @param filepath IN QUOTES: Relative path to the mash pairwise distance output file, should be a 5-column tsv without column names.
#'
#' @return parsed distance matrix saved in variable "mash_matrix" in the global environment. Do not have other variables with the same name in .GlobalEnv.
#'
#' @import dplyr
#'
#' @export

input <- function(filepath){
mash_dist <-read_tsv(filepath,col_names = c("Query","Reference","Distance","p_value","matched_hashes"))%>%
    select (c(1,2,3)) %>%
    mutate(Query=as.factor(Query),Reference=as.factor(Reference)) %>%
    as.data.frame()

mash_matrix <- xtabs(mash_dist[,3] ~ mash_dist[,1] + mash_dist[,2]) %>%
  as.dist()

assign("mash_matrix",mash_matrix,envir = .GlobalEnv)
}
