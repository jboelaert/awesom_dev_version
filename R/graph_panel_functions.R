

getPalette <- function(pal, n, reverse= F) {
  if(pal == "grey") {
    res <- grey(1:n / n)
  } else if(pal == "rainbow") { 
    res <- substr(rainbow(n), 1, 7) 
  } else if(pal == "heat") { 
    res <- substr(heat.colors(n), 1, 7) 
  } else if(pal == "terrain") { 
    res <- substr(terrain.colors(n), 1, 7) 
  } else if(pal == "topo") { 
    res <- substr(topo.colors(n), 1, 7) 
  } else if(pal == "cm") { 
    res <- substr(cm.colors(n), 1, 7) 
  } else if (pal == "viridis") {
    if (n == 1) {
      res <- substr(viridis::viridis(3), 1, 7)[1]
    } else if (n == 2) {
      res <- substr(viridis::viridis(3), 1, 7)[c(1,3)]
    } else 
      res <- substr(viridis::viridis(n), 1, 7)
  } else {
    if (n == 1) {
      res <- RColorBrewer::brewer.pal(3, pal)[1]
    } else if (n == 2) {
      res <- RColorBrewer::brewer.pal(3, pal)[c(1,3)]
    } else 
      res <- RColorBrewer::brewer.pal(n, pal)
  }
  if (length(res) == 1) 
    res <- list(res)
  if (reverse) 
    res <- rev(res)
  res
}




#' Plot dendogram for hierarchical clustering of SOM cells
#'
#' Plots a dendogram that that provides a quality measurement of the superclasses of the SOM
#' when using hierarchical clustering
#'
#' @param ok.som ```kohonen``` object, a SOM created by the ```som``` function.
#' @param ok.hclust hierarchical clustering object
#' @param input_kohSuperclass number of superclasses
#'
#' @return Dendogram plot of hierarchical superclass clusters
#'
#' @examples 
#' ## Build training data
#' dat <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### RNG Seed (for reproducibility)
#' ### Initialization (PCA grid)
#' init <- aweSOM::somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'), 
#'                        rlen = 100, alpha = c(0.05, 0.01), 
#'                        radius = c(6.08,-6.08), 
#'                        init = init, dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (hierarchical clustering)
#' superclust <- hclust(dist(ok.som$codes[[1]]), 'ward.D2')
#' ## Plot superclasses dendrogram
#' aweSOM::aweSOMdendrogram(ok.som, superclust, 2)
aweSOMdendrogram <- function(ok.som, ok.hclust, input_kohSuperclass){
  
  if (is.null(ok.som)) return()
  plot(ok.hclust, xlab= "", main= "")
  if (input_kohSuperclass > 1)
    rect.hclust(ok.hclust, k= input_kohSuperclass)
}








#' Screeplot 
#' 
#' Plots a screeplot that that provides a quality measurement of the quality of the superclasses of the SOM. 
#' Available for both PAM and hierarchical clustering. 
#'
#' @param ok.som ```kohonen``` object, a SOM created by the ```som``` function.
#' @param nclass number of superclasses to be visualized in the screeplot. Default is 2.
#' @param method Method used for clustering. Hierarchical clustering ("hierarchical") and PAM ("pam") clustering can be used. 
#' By default hierarchical clustering is applied.
#' @param hmethod Specifically for hierarchicical clustering, the clustering method can be specified. By default "ward.D2" is used
#' for other options, see the stats::hclust method documentation.
#'
#' @return Screeplot
#' @export
#'
#' @examples
#' ## Build training data
#' dat <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### RNG Seed (for reproducibility)
#' ### Initialization (PCA grid)
#' init <- somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'), 
#'                        rlen = 100, alpha = c(0.05, 0.01), 
#'                        radius = c(6.08,-6.08), 
#'                        init = init, dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (PAM clustering)
#' superclust <- cluster::pam(ok.som$codes[[1]], 2)
#' superclasses <- unname(superclust$clustering)
#' aweSOMscreeplot(ok.som, method = 'hierarchical', 
#'                 hmethod = 'ward.D2', nclass = 2)
aweSOMscreeplot <- function(ok.som, nclass= 2, method= "hierarchical", hmethod= "ward.D2"){
  if (is.null(ok.som)) return()
  
  if (method == "hierarchical")
    ok.hclust <- hclust(dist(ok.som$codes[[1]]), hmethod)
  
  ncells <- nrow(ok.som$codes[[1]])
  nvalues <- max(nclass, min(ncells, max(ceiling(sqrt(ncells)), 10)))
  clust.var <- sapply(1:nvalues, function(k) {
    if (method == "hierarchical") {
      clust <- cutree(ok.hclust, k)
    } else if (method == "pam") 
      clust <- cluster::pam(ok.som$codes[[1]], k)$clustering
    clust.means <- do.call(rbind, by(ok.som$codes[[1]], clust, colMeans))[clust, ]
    mean(rowSums((ok.som$codes[[1]] - clust.means)^2))
  })
  unexpl <- 100 * round(clust.var / 
                          (sum(apply(ok.som$codes[[1]], 2, var)) * (ncells - 1) / ncells), 3)
  plot(unexpl, t= "b", ylim= c(0, 100),
       xlab= "Nb. Superclasses", ylab= "% Unexpl. Variance")
  grid()                      
  abline(h= unexpl[nclass], col= 2)
  
}




#' Smooth Distance Plot
#'
#' Plots a visualization of the distances between the individual SOM cells. Visualizations for
#' hexagonal layout are biased with respect to the quadratic layout of the smooth distance plot.
#'
#' @param x ```kohonen``` object, a SOM created by the ```som``` function.
#' @param pal The color palette for visualizing distance. Default is "viridis"
#' @param reversePal Boolean whether color palette for variables is reversed. Default is FALSE
#'
#' @return Smooth distance plot
#' @export
#'
#' @examples
#' ok.data <- iris
#' ## Build training data
#' dat <- ok.data[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### RNG Seed (for reproducibility)
#' ### Initialization (PCA grid)
#' init <- somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'), 
#'                        rlen = 100, alpha = c(0.05, 0.01), 
#'                        radius = c(6.08,-6.08), init = init, 
#'                        dist.fcts = 'sumofsquares')
#' aweSOMsmoothdist(ok.som)
aweSOMsmoothdist <- function(x, pal= "viridis", reversePal= F) {
  if (is.null(x)) return(NULL)
  
  mapdist <- aweSOM::somDist(x)
  values <- matrix(rowMeans(mapdist$proto.data.dist.neigh, na.rm= T), 
                   x$grid$ydim, x$grid$xdim)
  filled.contour(1:x$grid$ydim, 1:x$grid$xdim,
                 values[, 1:x$grid$xdim],
                 color.palette= function(y) paste0(getPalette(pal, y, reversePal), "FF"))
  
}




#' Silhouette plot
#'
#' Plots a silhouette plot that provides a quality measurement of the superclasses of the SOM. 
#' Available for for both PAM and hierarchical clustering. 
#'
#' @param ok.som ```kohonen``` object, a SOM created by the ```som``` function.
#' @param ok.sc Computed super-classes object resulting from either PAM or hierarchical clustering 
#'
#' @return Silhouette plot for superclass clustering
#' @export
#'
#' @examples
#' ## Build training data
#' dat <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### RNG Seed (for reproducibility)
#' ### Initialization (PCA grid)
#' init <- somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'), 
#'                        rlen = 100, alpha = c(0.05, 0.01), 
#'                        radius = c(6.08,-6.08), init = init,
#'                        dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (PAM clustering)
#' superclust <- cluster::pam(ok.som$codes[[1]], 2)
#' superclasses <- unname(superclust$clustering)
#' aweSOMsilhouette(ok.som, superclasses)
aweSOMsilhouette <- function(ok.som, ok.sc){
  if (is.null(ok.som)) return()
  plot(cluster::silhouette(ok.sc, dist(ok.som$codes[[1]])), 
       main= "Silhouette of Cell Superclasses")
}










################################################################################
## d3-based plots
################################################################################



#################################
## Generate plot parameters to be passed to D3 functions
#################################


getPlotParams <- function(type, som, superclass, data, plotsize, varnames, 
                          normtype= c("range", "contrast"), palsc, palplot, 
                          cellNames, plotOutliers, reversePal, options= NULL, 
                          valueFormat) {
  
  ##########
  ## Common parameters for all plots
  ##########
  
  somsize <- nrow(som$grid$pts)
  clustering <- factor(som$unit.classif, 1:somsize)
  clust.table <- table(clustering)
  
  gridInfo <- list(nbLines= som$grid$xdim,
                   nbColumns= som$grid$ydim,
                   topology= ifelse(som$grid$topo == "rectangular", 
                                    'rectangular', "hexagonal"))
  n.sc <- length(unique(superclass))
  superclassColor <- getPalette(palsc, n.sc)
  res <- list(plotType= type, 
              sizeInfo= plotsize, 
              gridInfo= gridInfo, 
              superclass= superclass, 
              superclassColor= superclassColor, 
              cellNames= cellNames, 
              cellPop= unname(clust.table), 
              showAxes= options$showAxes)
  
  
  if (type %in% c("Pie", "CatBarplot")) {
    if (length(dim(data)) == 2) data <- data[, varnames]
    if (is.numeric(data)) if (length(unique(data)) > 30) data <- cut(data, 30)
    data <- as.factor(data)
    unique.values <- levels(data)
    nvalues <- nlevels(data)
  }
  
  if (type %in% c("Radar", "Line", "Barplot", "Boxplot", "Color", "Star")) {
    if (is.null(dim(data))) {
      data <- data.frame(data)
      colnames(data) <- varnames
    } else data <- as.data.frame(data)
    if (type == "Color") 
      data <- as.data.frame(sapply(data, as.numeric))
    
    nvar <- length(varnames)
    
    ##########
    ## Compute normalized values for mean/median/prototype to use in plots
    ##########
    
    if (type %in% c("Radar", "Line", "Barplot", "Color", "Star")) {
      if (all(varnames %in% colnames(som$codes[[1]]))) {
        prototypes <- som$codes[[1]][, varnames]
      } else
        prototypes <- som$codes[[1]]
      
      ## realValues are the one displayed in the text info above the plot
      if (valueFormat == "mean") {
        realValues <- do.call(rbind, lapply(split(data, clustering), 
                                            function(x) {
                                              if (!nrow(x)) return(rep(NA, nvar))
                                              unname(round(colMeans(x), 3))
                                            }))
      } else if (valueFormat == "median") {
        realValues <- do.call(rbind, lapply(split(data, clustering), 
                                            function(x) {
                                              if (!nrow(x)) return(rep(NA, nvar))
                                              unname(round(apply(x, 2, median), 3))
                                            }))
      } else if (valueFormat == "prototypes") {
        realValues <- unname(as.data.frame(prototypes))
      }
        
      if (normtype == "range") { 
        ## "Range" normalization : data/proto range to [0,1], then means
        if (valueFormat == "prototypes") {
          normDat <- as.data.frame(sapply(as.data.frame(prototypes), function(x) .05 + .9 * (x - min(x)) / (max(x) - min(x))))
        } else {
          normDat <- as.data.frame(sapply(data, function(x) .05 + .9 * (x - min(x)) / (max(x) - min(x))))
        }
        
        if(valueFormat == "mean"){ 
          normValues <- unname(lapply(split(normDat, clustering), 
                                      function(x) {
                                        if (!nrow(x)) return(rep(NA, nvar))
                                        unname(colMeans(x))
                                      }))
        } else if(valueFormat == "median"){
          normValues <- unname(lapply(split(normDat, clustering), 
                                      function(x) {
                                        if (!nrow(x)) return(rep(NA, nvar))
                                        unname(apply(x, 2, median))
                                      }))
        } else if(valueFormat == "prototypes"){
          normValues <- unname(as.list(as.data.frame(t(normDat))))
        }
        realValues <- unname(as.list(as.data.frame(t(realValues))))
        
      } else if (normtype == "contrast") {
        ## "Contrast" normalization : means on data, then range(means) -> [0,1]
        
        normValues <- apply(realValues, 2, function(x) 
          .05 + .9 * (x - min(x, na.rm= T)) / (max(x, na.rm= T) - min(x, na.rm= T)))
        normValues <- unname(as.list(as.data.frame(t(normValues))))
        realValues <- unname(as.list(as.data.frame(t(realValues))))
        
      } else if(normtype == "same"){
        ## "Same scale" normalization : global 0-1 scale, on obs/protos
        
        if (valueFormat %in% c("mean", "median")) {
          normDat <- as.data.frame(sapply(data, function(x) .05 + .9 * (x - min(data)) / (max(data) - min(data))))
          if(valueFormat == "mean"){
            normValues <- unname(lapply(split(normDat, clustering), 
                                        function(x) {
                                          if (!nrow(x)) return(rep(NA, nvar))
                                          unname(colMeans(x))
                                        }))
          } else if(valueFormat == "median"){
            normValues <- unname(lapply(split(normDat, clustering), 
                                        function(x) {
                                          if (!nrow(x)) return(rep(NA, nvar))
                                          unname(apply(x, 2, median))
                                        }))
          }
        } else if(valueFormat == "prototypes"){
          normDat <- as.data.frame(sapply(as.data.frame(prototypes), 
                                          function(x) .05 + .9 * (x - min(prototypes)) / (max(prototypes) - min(prototypes))))
          normValues <- unname(as.list(as.data.frame(t(normDat))))
        }
        realValues <- unname(as.list(as.data.frame(t(realValues))))
      }
      
      if (type == "Color") {
        ## 8 colors (equal-sized bins of values) of selected palette
        normValues <- do.call(rbind, normValues)
        normValues <- apply(normValues, 2, function(x) 
          getPalette(palplot, 8, reversePal)[cut(x, seq(.049, .951, length.out= 9))])
        normValues[is.na(normValues)] <- "#FFFFFF"
      }
    } else if (type == "Boxplot") {
      if (normtype == "same") {
        normDat <- (data - min(data)) / (max(data) - min(data))
      } else {
        normDat <- as.data.frame(sapply(data, function(x) (x - min(x)) / (max(x) - min(x))))
      }
      data <- as.data.frame(apply(data, 2, as.numeric))
    }
  } else if (type == "UMatrix") {
    proto.gridspace.dist <- kohonen::unit.distances(som$grid)
    proto.dataspace.dist <- as.matrix(dist(som$codes[[1]]))
    proto.dataspace.dist[round(proto.gridspace.dist, 3) > 1] <- NA
    proto.dataspace.dist[proto.gridspace.dist == 0] <- NA
    realValues <- round(unname(rowMeans(proto.dataspace.dist, na.rm= T)), 4)
    normValues <- (realValues - min(realValues)) / (max(realValues) - min(realValues))
    normValues <- getPalette(palplot, 8, reversePal)[cut(normValues , seq(-.001, 1.001, length.out= 9))]
    varnames <- "Mean distance to neighbours"
    type <- "Color"
    res$plotType <- "Color"
  }
  
  
  ##########
  ## Generate plot-type specific list of arguments
  ##########
  

  if (type == "Hitmap") {
    res$normalizedValues <- unname(.9 * sqrt(clust.table) / sqrt(max(clust.table)))
    res$realValues <- unname(clust.table)
  } else if (type %in% c("Radar", "Line", "Barplot", "Color", "Star")) {
    if (type != "Color") {
      res$nVars <- nvar
      res$labelColor <- getPalette(palplot, nvar, reversePal)
    }
    res$label <- varnames
    res$normalizedValues <- normValues
    res$realValues <- realValues
    res$isCatBarplot <- FALSE
    res$showSuperclass <- TRUE
  } else if (type == "Boxplot") {
    res$nVars <- nvar
    res$label <- varnames
    res$labelColor <- getPalette(palplot, nvar, reversePal)
    
    boxes.norm <- lapply(split(normDat, clustering), boxplot, plot= F)
    boxes.real <- lapply(split(data, clustering), boxplot, plot= F)
    res$normalizedValues <- unname(lapply(boxes.norm, function(x) unname(as.list(as.data.frame(x$stats)))))
    res$realValues <- unname(lapply(boxes.real, function(x) unname(as.list(as.data.frame(x$stats)))))
    
    if (plotOutliers) {
      res$normalizedExtremesValues <- unname(lapply(boxes.norm, function(x) unname(split(x$out, factor(x$group, levels= 1:nvar)))))
      res$realExtremesValues <- unname(lapply(boxes.real, function(x) as.list(unname(split(x$out, factor(x$group, levels= 1:nvar))))))
    } else {
      res$normalizedExtremesValues <- unname(lapply(boxes.norm, function(x) lapply(1:nvar, function(y) numeric(0))))
      res$realExtremesValues <- unname(lapply(boxes.real, function(x) lapply(1:nvar, function(y) numeric(0))))
    }
  } else if (type == "Pie") {
    res$nVars <- nvalues
    res$label <- unique.values
    res$labelColor <- getPalette(palplot, nvalues, reversePal)
    if (options$equalSize) {
      res$normalizedSize <- rep(.9, length(clust.table))
    } else
      res$normalizedSize <- unname(.9 * sqrt(clust.table) / sqrt(max(clust.table)))
    res$realSize <- unname(clust.table)
    res$normalizedValues <- unname(lapply(split(data, clustering), 
                                          function(x) {
                                            if (!length(x)) return(rep(1/nvalues, nvalues))
                                            unname(table(x) / length(x))
                                          }))
    res$realValues <- unname(lapply(split(data, clustering), 
                                    function(x) unname(table(x))))
    
  } else if (type == "CatBarplot") {
    res$nVars <- nvalues
    res$isCatBarplot <- TRUE
    res$label <- unique.values
    res$labelColor <- getPalette(palplot, nvalues, reversePal)
    res$realValues <- unname(lapply(split(data, clustering), 
                                    function(x) unname(table(x))))
    if (normtype == "contrast") {
      maxValue <- max(do.call(c, lapply(split(data, clustering), 
                                        function(x) {
                                          if (!length(x)) return(rep(0, nvalues))
                                          unname(table(x) / length(x))
                                        })))
    } else maxValue <- 1
    res$normalizedValues <- unname(lapply(split(data, clustering), 
                                          function(x) {
                                            if (!length(x)) return(rep(0, nvalues))
                                            .05 + .9 * unname(table(x) / length(x)) / maxValue
                                          }))
    res$plotType <- "Barplot"
  }
  
  res
}


#################################
## Core widget function, render D3 plot in an htmlwidget
#################################

aweSOMwidget <- function(ok.som, ok.sc, ok.clust, ok.data, ok.trainrows, 
                         graphType= "Hitmap", 
                         plotNames= "(rownames)", plotVarMult= NULL, plotVarOne= NULL, 
                         plotOutliers= T, plotEqualSize= F,
                         contrast= "contrast", average_format= "mean",
                         plotSize= 100, 
                         palsc= "Set3", palplot= "viridis", plotRevPal= F,
                         width = NULL, height = NULL, elementId = NULL) {
  
  if (is.null(ok.som))
    return(NULL)
  
  ok.clust <- ok.som$unit.classif
  plot.data <- ok.data[ok.trainrows, ]
  if(is.null(plot.data)) return(NULL)
  
  # Obs names per cell for message box
  if (is.null(plotNames)) return(NULL) 
  if (plotNames == "(rownames)") {
    plotNames.var <- rownames(plot.data)
  } else {
    plotNames.var <- as.character(plot.data[, plotNames])
  }
  
  cellNames <- unname(lapply(split(plotNames.var, factor(ok.clust, levels= 1:nrow(ok.som$codes[[1]]))), 
                             function(x) paste(x, collapse= ", "))) # "&#13;&#10;" "<br />"
  
  
  
  if (graphType %in% c("Radar", "Star", "Barplot", "Boxplot", "Line")) {
    if (is.null(plotVarMult)) return(NULL)
    plotVar <- plotVarMult
    data <- plot.data[, plotVar]
  } else if (graphType %in% c("Color", "Pie", "CatBarplot")) {
    if (is.null(plotVarOne)) return(NULL)
    plotVar <- plotVarOne
    data <- plot.data[, plotVar]
  } else if (graphType %in% c("Hitmap")) {
    plotVar <- NULL
    data <- NULL
  } else if (graphType %in% c("Names")) {
    plotVar <- NULL
    data <- as.character(plot.data[, plotVarOne])
  }
  
  options <- list(equalSize= plotEqualSize)
  
  plotParams <- getPlotParams(graphType, ok.som, ok.sc,  
                              data, plotSize, plotVar, contrast,
                              palsc, palplot, cellNames,
                              plotOutliers, plotRevPal, options, 
                              average_format)
  
  # create the widget
  htmlwidgets::createWidget("aweSOMwidget", plotParams, elementId = elementId, 
                            width = plotSize, height = plotSize, package = "aweSOM", 
                            sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth = "100%", defaultHeight = "auto", padding= 0))
}

## htmlwidgets - shiny binding
#' @export
aweSOMoutput <- function(outputId, width = "100%", height = "auto") {
  htmlwidgets::shinyWidgetOutput(outputId, "aweSOMwidget", width, height, package = "aweSOM")
}
#' @export
renderaweSOM <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, aweSOMoutput, env, quoted = TRUE)
}

aweSOMwidget_html = function(id, style, class, ...){
  htmltools::tags$div(id = id, class = class, 
                      style= paste0(style, "display:block; margin:auto; margin-top:5px; margin-bottom:5px;"))
}

#################################
## Console-callable function for interactive plots
#################################


#' Interactive SOM visualizations
#'
#' Plot interactive visualizations of self-organizing maps (SOM), as an html
#' page. The plot can represent general map informations, or selected
#' categorical or numeric variables (not necessarily the ones used during
#' training). Hover over the map to focus on the selected cell or variable, and
#' display further information.
#'
#' @param som ```kohonen``` object, a SOM created by the ```som``` function.
#' @param type character, the plot type. The default "Hitmap" is a population
#'   map. "UMatrix" plots the average distance of each cell to its neighbors, on
#'   a color scale. "Radar", "Barplot", "Boxplot", "Star" and "Line" are for
#'   numeric variables. "Color" (heat map) is for a single numeric variable.
#'   "Pie" (pie chart) and "CatBarplot" are for a single categorical (factor)
#'   variable.
#' @param data data.frame containing the variables to plot. This is typically
#'   not the training data, but rather the unscaled original data, as it is
#'   easier to read the results in the original units, and this allows to plot
#'   extra variables not used in training. If not provided, the training data is
#'   used.
#' @param variables character vector containing the names of the variable(s) to
#'   plot. The selected variables must be numeric for types "Radar", "Barplot",
#'   "Boxplot", "Star", "Color" and "Line", or factor for types "Pie" and
#'   "CatBarplot". If not provided, all columns of data will be selected. If a 
#'   numeric variable is provided to a "Pie" or "CatBarplot", it will be split 
#'   into a maximum of 30 classes.
#' @param superclass integer vector, the superclass of each cell of the SOM.
#' @param obsNames character vector, names of the observations to be displayed
#'   when hovering over the cells of the SOM. Must have a length equal to the
#'   number of data rows. If not provided, the row names of data will be used.
#' @param scales character, controls the scaling of the variables on the plot.
#'   The default "constrast" maximizes the displayed contrast by scaling the
#'   displayed heights of each variable from minimum to maximum of the displayed
#'   value. Alternatively, "range" uses the minimum and maximum of the
#'   observations for each variable, and "same" displays all variables on the
#'   same scale, using the global minimum and maximum of the data.
#' @param values character, the type of value to be displayed. The default
#'   "mean" uses the observation means (from data) for each cell. Alternatively,
#'   "median" uses the observation medians for each cell, and "prototypes" uses
#'   the SOM's prototypes values.
#' @param size integer, plot size, in pixels. Default 400.
#' @param palsc character, the color palette used to represent the superclasses
#'   as background of the cells. Default is "Set3". Can be "viridis", "grey",
#'   "rainbow", "heat", "terrain", "topo", "cm", or any palette name of the
#'   RColorBrewer package.
#' @param palvar character, the color palette used to represent the variables.
#'   Default is "viridis", available choices are the same as the ones for palsc.
#' @param palrev logical, whether color palette for variables is reversed.
#'   Default is FALSE.
#' @param boxOutliers logical, whether outliers in "Boxplot" are displayed.
#'   Default is TRUE.
#' @param pieEqualSize logical, whether "Pie" should display pies of equal size.
#'   The default FALSE displays pies with areas proportional to the number of
#'   observations in the cells.
#' @param elementId user-defined elementId of the widget, can be useful for
#'   user extensions when embedding the result in an html page.
#'
#' @return Returns an object of class htmlwidget.
#'
#' @examples
#' ## Build training data
#' dat <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### RNG Seed (for reproducibility)
#' ### Initialization (PCA grid)
#' init <- aweSOM::somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'),
#'                        rlen = 100, alpha = c(0.05, 0.01),
#'                        radius = c(6.08,-6.08), init = init,
#'                        dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (PAM clustering)
#' superclust <- cluster::pam(ok.som$codes[[1]], 2)
#' superclasses <- unname(superclust$clustering)
#'
#' ## Population map ('Hitmap')
#' aweSOMplot(som = ok.som, type = 'Hitmap', superclass = superclasses)
#'            
#' ## Plots for numerical variables
#' variables <- c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")
#' ## Radar
#' aweSOMplot(som = ok.som, type = 'Radar', data = iris, 
#'            variables= variables, superclass = superclasses)
#' ## Barplot (numeric variables)
#' aweSOMplot(som = ok.som, type = 'Barplot', data = iris, 
#'            variables= variables, superclass = superclasses)
#'
#' ## Plots for categorial variables (iris species, not used for training)
#' ## Pie
#' aweSOMplot(som = ok.som, type = 'Pie', data = iris, 
#'            variables= "Species", superclass = superclasses)
#' # Barplot (categorical variables)
#' aweSOMplot(som = ok.som, type = 'CatBarplot', data = iris, 
#'            variables= "Species", superclass = superclasses)

aweSOMplot <- function(som, type= c("Hitmap", "UMatrix", "Radar", "Barplot", 
                                    "Boxplot", "Star", "Line", "Color",
                                    "Pie", "CatBarplot"), 
                       data= NULL, variables= NULL, superclass= NULL, 
                       obsNames= NULL,
                       scales= c("contrast", "range", "same"), 
                       values= c("mean", "median", "prototypes"),
                       size= 400, 
                       palsc=  c("Set3", "viridis", "grey", "rainbow", "heat", "terrain", 
                                 "topo", "cm", rownames(RColorBrewer::brewer.pal.info)), 
                       palvar= c("viridis", "grey", "rainbow", "heat", "terrain", 
                                 "topo", "cm", rownames(RColorBrewer::brewer.pal.info)), 
                       palrev= FALSE,
                       boxOutliers= TRUE, pieEqualSize= FALSE,
                       elementId= NULL) {

  type <- match.arg(type)
  scales <- match.arg(scales)
  values <- match.arg(values)
  palsc <- match.arg(palsc)
  palvar <- match.arg(palvar)
  
  if (!("kohonen" %in% class(som)))
    stop("`som` argument must be a `kohonen` object, created by `kohonen::som`")
  
  if (type %in% c("Radar", "Barplot", "Boxplot", "Star", "Line", "Color", "Pie", "CatBarplot")) {
    if (is.null(data)) ## If no data, fall back on training data
      data <- som$data[[1]]
    
    if (nrow(data) != nrow(som$data[[1]])) 
      stop("`data` must have the same number of rows as the training data.")
    
    if (is.null(variables)) ## If no variables, fall back on all columns
      variables <- colnames(data)
    
    if (! all(variables %in% colnames(data))) 
      stop(paste0("Variables < ", 
                  paste(variables[! (variables %in% colnames(data))], collapse= " , "),
                  " > not found in data"))
    
    if (type %in% c("Color", "Pie", "CatBarplot")) {
      if (length(variables) > 1) {
        warning(paste0(type, " : Multiple variables provided, only the first one will be plotted."))
        variables <- variables[1]
      }
    }
    
    if (type %in% c("Radar", "Barplot", "Boxplot", "Star", "Line", "Color")) {
      var.num <- sapply(variables, function(i) is.numeric(data[, i]))
      if (!all(var.num)) {
        warning(paste0("Only numeric variables can be plotted by ", type, 
                       ", variables < ", paste(variables[!var.num], collapse= " , "), 
                       " > will be dropped."))
        variables <- variables[var.num]
      }
    }
      
  }
  
  data <- as.data.frame(data)[variables]
  
  if (is.null(superclass)) {
    superclass <- rep(1, nrow(som$grid$pts))
  } else if (length(superclass) != nrow(som$grid$pts)) {
    warning("`superclass` must have a length equal to the number of cells on the map. \
            No superclass will be plotted.")
    superclass <- rep(1, nrow(som$grid$pts))
  }
  superclass <- unname(superclass)

  obsClust <- som$unit.classif

  if (is.null(obsNames)) {
    if (is.null(rownames(data))) {
      obsNames <- as.character(1:nrow(som$data[[1]]))
    } else obsNames <- rownames(data)
  } else if (length(obsNames) != nrow(data)) {
    warning("`obsNames` must have a length equal to the number of rows of `data`.")
    if (is.null(rownames(data))) {
      obsNames <- as.character(1:nrow(som$data[[1]]))
    } else obsNames <- rownames(data)
  }
  obsNames <- unname(lapply(split(obsNames, factor(obsClust, levels= 1:nrow(som$codes[[1]]))), 
                            function(x) paste(x, collapse= ", ")))
  
  options <- list(equalSize= pieEqualSize)
  
  plotParams <- getPlotParams(type, som, superclass, data, size, 
                              variables, scales, palsc, palvar, obsNames,
                              boxOutliers, palrev, options, values)

  ## Create the widget
  res <- htmlwidgets::createWidget(
    "aweSOMwidget", plotParams, elementId = elementId, 
    width = size, height = size, package = "aweSOM", 
    sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth = "100%", 
                                             defaultHeight = "auto", padding= 0))

  ## Add elements for legend and messages
  if(is.null(res$elementId)) {
    res$elementId <- paste0(
      'aweSOMwidget-', paste(format(as.hexmode(sample(256, 10, replace = TRUE) - 1), 
                                    width = 2), collapse = ""))
  }
  res <- htmlwidgets::prependContent(res, htmltools::tag("h4", list(id= paste0(res$elementId, "-info"))))
  res <- htmlwidgets::prependContent(res, htmltools::tag("h4", list(id= paste0(res$elementId, "-message"))))
  res <- htmlwidgets::appendContent(res, htmltools::tag("p", list(id= paste0(res$elementId, "-names"))))
  if (type %in% c("Barplot", "Boxplot", "Radar", "Pie", "CatBarplot"))
    res <- htmlwidgets::appendContent(res, htmltools::tag("svg", list(id= paste0(res$elementId, "-legend"), width = "100%")))
  
  res
}


