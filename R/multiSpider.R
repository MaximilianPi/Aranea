#' multispider plot
#' @param x1
#' @param x2
#' @param colRec
#' @param alphaRec
#' @param colRecBorder
#' @param titles
#' @param cexSteps
#' @param cexProcent
#' @param parValues
#' @param mar
#' @param cexPoints
#' @param sizeMiddle
#' @param twist
#'
#'
#' @export



multiSpider = function(x1 = NULL,x2 = NULL,
                       colRec = NULL, alphaRec = 0.5,colRecBorder = "#ff6666", titles = NULL,cexSteps = c(0.01, 1.4), cexProcent = c(0.2,0.8),
                       parValues = list(pty = "s"), mar = list(c(0,2,1,2)+0.7,c(0,0,1,0)+0.2),cexPoints = 1.0, sizeMiddle = 0.5, twist = 90, ...){
  ## help functions:
  deg2rad <- function(deg) {(deg * pi) / (180)}
  addA = function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))
  ##

  if(is.null(dim(x1))) stop("Only one row found, use spider()")
  if(length(colRecBorder) == 1) colRecBorder = rep(colRecBorder, nrow(x1))

  ## sizes:
  mid = rep(c(0.5 - 0.5*sizeMiddle, 0.5 + 0.5*sizeMiddle), 2)
  sideMid = 3*(1-sizeMiddle) / 4
  sideDis = (1-sizeMiddle)/4
  ##

  ## init:
  nrTest = nrow(x1)
  if(is.null(colRec)) colRec = colRecBorder
  if(is.null(titles)) titles = rep(NULL, nrow(x1))


  multiCoords = matrix(0, nrow(x1) + 1, 4)
  angles = seq(0+twist,360+twist,length.out = nrTest+1)[1:(nrTest)]
  multiCoords[1,] = mid
  for(p in 2:nrow(multiCoords)){
    pp = p -1
    multiCoords[p,1] = cos(deg2rad(angles[pp])) * sideMid + 0.5 - sideDis
    multiCoords[p,2] = multiCoords[p,1] + 2*sideDis
    if(multiCoords[p,1] > multiCoords[p,2]) multiCoords[p,] = multiCoords[p,c(2,1,3,4)]
    multiCoords[p,3] = sin(deg2rad(angles[pp])) * sideMid  + 0.5 -sideDis
    multiCoords[p,4] = multiCoords[p,3] + 2*sideDis
    if( multiCoords[p,4] > 1) multiCoords[p,4] = 1


  }
  for(plt in 1:(nrTest + 1)){
    if(plt == (nrTest + 1)){
      parValues2 = c(parValues, list(fig = multiCoords[1,], new = T, mar = mar[[1]]))
      spider(x1,  colRec = NULL, alphaRec = alphaRec, colRecBorder = colRecBorder,
              cexSteps = cexSteps[2], cexProcent = cexProcent[2], parValues = parValues2, cexPoints = cexPoints, singlePanel = T, titles = NULL, ...)
    } else {
      if(plt == 1) parValues2 = c(parValues, list(fig = multiCoords[plt+1,], mar = mar[[2]]))
      else parValues2 = c(parValues, list(fig = multiCoords[plt+1,], new = T))
      spider(x1[plt,,drop = F], x2[plt,,drop = F],colRec = colRec[plt], alphaRec = alphaRec, colRecBorder = colRecBorder[plt],
                 cexSteps = cexSteps[1], cexProcent = cexProcent[1], parValues = parValues2, cexPoints = cexPoints, singlePanel = F, titles = titles[plt],...)
    }
  }

}
