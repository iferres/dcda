

plot.dcda <- function(x, 
                      hclustFun = hclust,
                      distFun,
                      margins = c(5, 5), 
                      edgePar = list(lwd = 1.5),
                      matColorFun = colorRampPalette(c('yellow2',
                                                       'deepskyblue3',
                                                       'darkmagenta')),
                      distColorFun = colorRampPalette(c('white',
                                                        'indianred2',
                                                        'midnightblue')),
                      cex.axis = 1L){
  
  nr <- nrow(x$mat)
  nc <- ncol(x$mat)
  hc <- hclustFun(x$dist)
  dhc <- as.dendrogram(hc)
  
  # Matrix: column ordering
  if (!missing(distFun)){
    dcol <- distFun(t(x$mat))
    hccol <- hclustfun(dcol)
    cord <- hccol$order
  }else{
    cord <- 1:ncol(mm)
  }

  mm <- x$mat[hc$order, cord]
  
  dm <- as.matrix(x$dist)
  dm <- dm[hc$order, rev(hc$order)]
  dm[which(lower.tri(dm)[, ncol(dm):1])] <- NA
  
  ### Start device functions ###
  op <- par(no.readonly = T)
  on.exit(dev.flush())
  on.exit(par(op), add = TRUE)
  
  dev.hold()
  
  
  ### Layout ###
  layout(mat = rbind(4:6,1:3),
         heights = c(1,4),
         widths = c(1,2,1))
  
  
  ### Dendrogram ###
  par(mar = c(margins[1], 0, 0.5, 0), yaxs= 'i')
  plot(dhc,
       horiz = TRUE,
       # frame.plot = F,
       leaflab = 'n',
       edgePar = edgePar,
       axes=F)
  
  ### Mat ###
  par(mar = c(margins[1],
              0,
              0.5,
              margins[2]))
  
  image(x = 1:nc,
        y = 1:nr,
        z = t(mm),
        xlim = 0.5 + c(0, nc),
        ylim = 0.5 + c(0, nr),
        frame.plot = F,
        col = matColorFun(max(mm)),
        yaxt = 'n',
        xaxt = 'n',
        # xaxs= 'r',
        ylab = '',
        xlab = '',
        useRaster = raster,
        xpd = T)
  axis(4,
       1:nr, 
       labels = rownames(mm),
       las=2, 
       tick = 0, 
       line = -1)
  
  
  ### Dist ###
  par(mar = c(margins[1],
              0,
              0.5,
              0.5))
  
  image(x = 1:dim(dm)[1],
        y = 1:dim(dm)[1],
        z = dm,
        xlim = 0.5 + c(0, dim(dm)[1]),
        ylim = 0.5 + c(0, dim(dm)[1]),
        frame.plot = F,
        col = distColorFun(round(max(dm, na.rm = T)*100)),
        yaxt = 'n',
        xaxt = 'n',
        # xaxs= 'r',
        ylab = '',
        xlab = '',
        useRaster = raster,
        xpd = T)
  axis(side = 1, 
       labels = rownames(dm), 
       at = 1:dim(dm)[1], 
       cex.axis = cex.axis,
       las=2)
  
  ###Keys###
  plot.new()
  par(mar = c(0, 0, margins[1], margins[2]))
  image(x = 1:100, y = 1, z = cbind(1:100), col = matColorFun(100))
  
  
  par(mar = c(0, 0, margins[1], margins[2]))
  image(x = 1:100, y = 1, z = cbind(1:100), col = distColorFun(100))
  
}