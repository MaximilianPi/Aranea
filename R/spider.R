#' Spider chart function
#' @param x1 data
#' @param x2 data to plot on top of x1
#' @param colRec color of x1 rectangular
#' @param alphaRec alpha of the rectangular
#' @param colRecBorder color of rectangular's border
#' @param titles title for the spider plot
#' @param stepsText labels for the radar lines
#' @param singlePanel plot everything in one plot
#' @param rectangular rectangular spider chart
#' @param colSpider color of the spider chart
#' @param alphaSpider alpha of the spider chart
#' @param colBorder color for the five rings of the chart
#' @param maxValues which x1/x2 maximal can get
#' @param minValues which x1/x2 minimal can get
#' @param rad radius for the charts, plot window is limited from -5 to 5 in x and y direction
#' @param cexSteps size of the stepsTexts
#' @param cexProcent size of the procent texts
#' @param parValues par(..) values to be called for each chart
#' @param cexPoints size of the points
#' @param circleLines number of circle lines
#' @export
#'
spider = function(x1 = NULL,x2 = NULL, colRec = "#ff9999", alphaRec = 0.5,colRecBorder = NULL, titles = NULL, stepsText = NULL,singlePanel = F,
                  rectangular = F,
                  colSpider = "#e6e6e6" ,alphaSpider = 0.5,
                  colBorder = c("#cccccc","#cccccc","#666798","#cccccc","#cccccc"),
                  maxValues = NULL,minValues = NULL,
                  rad = 5, cexSteps = 1.4, cexProcent = 0.8,
                  parValues = list(pty = "s"), cexPoints = 1.0, circleLines = 5){



  if(is.null(colRecBorder)) colRecBorder = colRec
  if(is.null(dim(x1))) x1 = matrix(x1, ncol = length(x1), nrow = 1)
  if(!is.null(x2)){
    if(is.null(dim(x2))) x2 = matrix(x2, ncol = length(x2), nrow = 1)
  }
  if(is.null(maxValues)) maxValues = rep(1, ncol(x1))
  if(is.null(minValues)) minValues = rep(0, ncol(x1))
  if(!is.null(colRec)) colRec2 = addA(colRec, 0.5*alphaRec)
  if(!is.null(colRec)) colRec = addA(colRec, alphaRec)
  if(length(colRec) == 1) {
    colRec = rep(colRec, nrow(x1))
    colRecBorder = rep(colRecBorder, nrow(x1))
  }

  ## scale:
  for(i in 1:ncol(x1)){
    x1[,i] = (x1[,i] + abs(minValues[i]))/(abs(minValues[i]) + maxValues[i])
    if(!is.null(x2)) x2[,i] = (x2[,i] + abs(minValues[i]))/(abs(minValues[i]) + maxValues[i])
    if(any(x1[,i] > maxValues[i])) stop("Max values are lower than actual values")
    if(any(x1[,i] < minValues[i])) stop("Min values are higher than actual values")

  }
  if(!is.null(x2)){
    if(nrow(x2) != nrow(x1)) stop("x1 and x2 do not have equal number of rows")
  }

  ## init:
  lineSeq = seq(rad*0.1,rad, length.out = 5)
  tmpSinglePanel = T
  do.call(par, parValues)
  nseg=1440
  nSeg = ncol(x1)

  procent = matrix(0,5,2)
  colSpider <- addA(colSpider, alphaSpider)
  #

  baseRadar = function() {

    lines = circleLines
    lineSeq = seq(rad*0.1,rad, length.out = lines)
    angles = seq(90,450,length.out = nSeg+1)[1:(nSeg)]

    plot(NULL, NULL, xlim = c(-5,5), ylim =c(-5,5),pty="s", axes = F, xlab = "", ylab = "")
    if(!rectangular)
      for(i in 1:length(lineSeq)){
        xx = lineSeq[i]*cos( seq(0,2*pi, length.out=nseg) )
        yy = lineSeq[i]*sin( seq(0,2*pi, length.out=nseg) )
        if(i == lines) polygon(xx,yy, col= colSpider, border = colBorder[lines], lty = 2, lwd = 1)
        else if(i == ceiling(lines)) polygon(xx,yy, border = colBorder[ceiling(lines)], lty = 2)
        else if(i == 1) polygon(xx,yy,  border = colBorder[lines], lty = 2)
        else polygon(xx,yy, border = colBorder[i], lty = 2)
      }
    else
      for(i in 1:length(lineSeq)){
        xx = cos(deg2rad(angles))*lineSeq[i]
        yy = sin(deg2rad(angles))*lineSeq[i]
        if(i == lines) polygon(xx,yy, col= colSpider, border = colBorder[lines], lty = 2, lwd = 1)
        else if(i == ceiling(lines)) polygon(xx,yy, border = colBorder[ceiling(lines)], lty = 2)
        else if(i == 1) polygon(xx,yy,  border = colBorder[lines], lty = 2)
        else polygon(xx,yy, border = colBorder[i], lty = 2)
      }

    for(counter in 1:length(angles)) {
      segments(x0 = cos(deg2rad(angles[counter]))*lineSeq[1],
               y0 =  sin(deg2rad(angles[counter]))*lineSeq[1],
               x1 = cos(deg2rad(angles[counter]))*rad ,
               y1 = sin(deg2rad(angles[counter]))*rad ,
               col = colBorder[5])
    }
  }


  ## plot rect
  for(data in 1:nrow(x1)){
    if(data == 1 || !singlePanel) baseRadar()
    valuesP = matrix(0,nSeg,2)
    textP = matrix(0,nSeg,2)
    valuesPtrain = matrix(0,nSeg,2)
    drTest = x1[data,,drop = F]
    drTrain = x2[data,,drop = F]
    for(i in 1:nSeg){
      valuesP[i,1] = cos(deg2rad(angles[i]))*drTest[1,i]*(rad-lineSeq[1]) +
        cos(deg2rad(angles[i]))*lineSeq[1]
      valuesP[i,2] = sin(deg2rad(angles[i]))*drTest[1,i]*(rad-lineSeq[1]) +
        sin(deg2rad(angles[i]))*lineSeq[1]

      if(!is.null(x2)){
        valuesPtrain[i,1] = cos(deg2rad(angles[i]))*drTrain[1,i]*(rad-lineSeq[1]) +
          cos(deg2rad(angles[i]))*lineSeq[1]
        valuesPtrain[i,2] = sin(deg2rad(angles[i]))*drTrain[1,i]*(rad-lineSeq[1]) +
          sin(deg2rad(angles[i]))*lineSeq[1]
      }

      textP[i,1] = cos(deg2rad(angles[i]))*1.0*rad
      textP[i,2] = sin(deg2rad(angles[i]))*1.08*rad
    }
    polygon(y = valuesP[,2], x = valuesP[,1], col = colRec[data],border = colRecBorder[data], lwd = 1.5)
    points(y = valuesP[,2], x = valuesP[,1], pch = 16, col = colRecBorder[data], cex = cexPoints)

    if(!is.null(x2)){
      polygon(y = valuesPtrain[,2], x = valuesPtrain[,1], col = colRec2[data],border = colRecBorder[data], lwd = 1.5, lty = 2)
      points(y = valuesPtrain[,2], x = valuesPtrain[,1], pch = 16, col = colRecBorder[data], cex = cexPoints, lty = 2)
    }

    ## Text
    if(data == 1 || !singlePanel){
      measures = stepsText
      if(!is.null(measures)){
        strl = max(sapply(measures,nchar))
        # measures = as.vector(sapply(measures, function(x, strl){
        #     if((strl - nchar(x)) > 0)return(do.call(paste0, args = as.list( x, c(rep(" ",strl - nchar(x))))))
        #     else return(x)
        #   } ,strl))
        # textP[,1][7] = textP[,1][7] - 0.3
        # textP[,2][7] = textP[,2][7] - 0.3
        pos = sapply(angles, function(x) {
          if(x >= 45 && x <= 95 ) return(3)
          if(x>95 && x<240) return(2)
          if(x>=240 && x<=285) return(1)
          if(x>285 &&x<=360) return(4)
          if(x<45) return(4)
        })
        text(x = textP[,1], y = textP[,2], labels = measures, xpd = T, font = 2, cex = cexSteps, pos = pos)
      }
      procent[,1] = 0.2
      procent[,2] = lineSeq
      text(x = procent[,1], y = procent[,2], labels = c("  0%", " 25%", " 50%", " 75%", "100%"),
           adj = c(-0.2,0.8), font = 2, cex = cexProcent)
      if(!is.null(titles)) title(main = titles, outer = F)
    }

    #

  }
  #


}


