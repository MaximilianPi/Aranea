#' deg2rad help function
#' @param degree degree to rad
deg2rad <- function(deg) {(deg * pi) / (180)}



#' addA hex into rgb
#' @param col color as hex
#' @param alpha alpha grade
addA    <- function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))
