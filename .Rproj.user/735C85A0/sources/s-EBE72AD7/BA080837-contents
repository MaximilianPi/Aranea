multiSpider = function(test, colSpider = "#e6e6e6" ,alphaSpider = 0.5,
                       colBorder = c("#cccccc","#cccccc","#666798","#cccccc","#cccccc"),
                       colRec = "#ff9999", alphaRec = 0.5,colRecBorder = "#ff6666", rad = 5, cexM = c(0.01, 1.4), cexP = c(0.2,0.8),
                       parValues = list(pty = "s"), cexPoints = 1.0, sizeMiddle = 0.5, twist = 90, titles = NULL, train = NULL){
  ## help functions:
  deg2rad <- function(deg) {(deg * pi) / (180)}
  addA = function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))
  ##

  ## sizes:
  mid = rep(c(0.5 - 0.5*sizeMiddle, 0.5 + 0.5*sizeMiddle), 2)
  sideMid = 3*(1-sizeMiddle) / 4
  sideDis = (1-sizeMiddle)/4
  ##

  ## init:
  nrTest = nrow(test)


  multiCoords = matrix(0, nrow(test) + 1, 4)
  angles = seq(0+twist,360+twist,length.out = nrTest+1)[2:(nrTest+1)]
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
      parValues2 = c(parValues, list(fig = multiCoords[1,], new = T, mar = c(0,2,1,2)+0.7))
      spider(test, colSpider = colSpider, alphaSpider = alphaSpider, colBorder = colBorder, colRec = NULL, alphaRec = alphaRec, colRecBorder = colRecBorder,
             rad = rad, cexM = cexM[2], cexP = cexP[2], parValues = parValues2, cexPoints = cexPoints, singlePanel = T, titles = NULL)
    } else {
      if(plt == 1) parValues2 = c(parValues, list(fig = multiCoords[plt+1,], mar = c(0,0,1,0)+0.2))
      else parValues2 = c(parValues, list(fig = multiCoords[plt+1,], new = T))
      try(spider(test[plt,,drop = F], colSpider = colSpider, alphaSpider = alphaSpider, colBorder = colBorder, colRec = colRec[plt], alphaRec = alphaRec, colRecBorder = colRecBorder[plt],
                 rad = rad, cexM = cexM[1], cexP = cexP[1], parValues = parValues2, cexPoints = cexPoints, singlePanel = F, titles = titles[plt], train = train[plt,,drop = F]))
    }
  }

}
