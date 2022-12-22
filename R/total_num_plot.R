#' Generates a geom_step plot showing distribution of total cluster counts (y axis) by clustering cutoff (x axis).
#'
#' @param min The minimum cutoff shown in this plot. Defaults to 0.01.
#' @param max The maximum cutoff shown in this plot. Defaults to 1.
#' @param log A logical value (TRUE/FALSE) to indicate if the logarithm scale should be used on the cutoff (x) axis. Defaults to FALSE
#'
#' @return A step plot showing how cluster count decreases over raising the cutoff.
#'
#' @import dplyr
#'        ggplot2
#'
#' @export
#'
#'
#'

total_num_plot <- function(min=0.01,max=1,log=TRUE) {
  if(min<0 && min>=1 && is.numeric(min)==F){
    print("Invalid min value, defaults to min=0.01")
    min=0.01
  }
  if(max<0 && max>=1 && max<min && is.numeric(max)==F){
    print("Invalid max value, defaults to max=1")
    max=1
  }
  if(is.logical(log)==F){
    print("log is defaulted as TRUE")
    log=T
  }
    total_num_list() %>%
    ggplot()+
    geom_step(aes(x=cutoff,y=total_num))+
    coord_cartesian(xlim = c(min,max),ylim = c(max(cluster_list_gen(max)$cluster),max(cluster_list_gen(min)$cluster)) )+
    if(log==TRUE){
      scale_x_log10(n.breaks=10,minor_breaks=NULL)
    }else{
      scale_x_continuous(n.breaks=10,minor_breaks=NULL)
      }

}
