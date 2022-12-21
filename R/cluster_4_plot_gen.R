#' Generates 4 consecutive histograms (in a row) of cluster count distribution from cutoff, given 4 cutoffs.
#'
#' @param cutoff1 a hierarchical clustering cutoff, from 0 to 1, same as cluster_list_gen
#' @param cutoff2
#' @param cutoff3
#' @param cutoff4
#'
#' @return 4 histograms of cluster count distribution, using cluster_plot_gen
#'
#' @import  dplyr
#'          ggplot2
#'          gridExtra
#'
#' @export
cluster_4_plot_gen <- function(cutoff1,cutoff2,cutoff3,cutoff4){
grid.arrange(cluster_plot_gen(cutoff1)+ scale_x_continuous(breaks = c(1:max(cluster_list_gen(min(c(cutoff1,cutoff2,cutoff3,cutoff4)))$cluster)))+ coord_cartesian(xlim = c(1,max(cluster_list_gen(min(c(cutoff1,cutoff2,cutoff3,cutoff4)))$cluster))),
             cluster_plot_gen(cutoff2)+ scale_x_continuous(breaks = c(1:max(cluster_list_gen(min(c(cutoff1,cutoff2,cutoff3,cutoff4)))$cluster)))+ coord_cartesian(xlim = c(1,max(cluster_list_gen(min(c(cutoff1,cutoff2,cutoff3,cutoff4)))$cluster))),
             cluster_plot_gen(cutoff3)+ scale_x_continuous(breaks = c(1:max(cluster_list_gen(min(c(cutoff1,cutoff2,cutoff3,cutoff4)))$cluster)))+ coord_cartesian(xlim = c(1,max(cluster_list_gen(min(c(cutoff1,cutoff2,cutoff3,cutoff4)))$cluster))),
             cluster_plot_gen(cutoff4)+ scale_x_continuous(breaks = c(1:max(cluster_list_gen(min(c(cutoff1,cutoff2,cutoff3,cutoff4)))$cluster)))+ coord_cartesian(xlim = c(1,max(cluster_list_gen(min(c(cutoff1,cutoff2,cutoff3,cutoff4)))$cluster))),
             ncol=1)
}
