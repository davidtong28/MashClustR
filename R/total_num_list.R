#' Generates a chart of possible total number of cluster and the minimum cutoff required to reach that number. No input is needed.
#'
#'
#' @return Generates a chart of possible total number of cluster and the minimum cutoff used to reach that number.
#'
#' @import  dplyr
#'
#' @export

total_num_list <- function() as.data.frame(hclust(mash_matrix,method = "average")$height) %>%
  select(cutoff=1) %>%
  group_by(cutoff) %>%
  summarise(count=n()) %>%
  mutate(total_num=nrow(cluster_list_gen(1))-cumsum(count)) %>%
  select(1,3)
