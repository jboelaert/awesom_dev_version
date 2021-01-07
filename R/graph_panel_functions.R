

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
#' Creates a dendogram that that provides a quality measurement of the quality of the clustering of each SOM cell 
#' for hierarchical clustering
#'
#' @param ok.som SOM data object
#' @param ok.hclust hierarchical clustering object
#' @param input_kohSuperclass number of superclasses
#'
#' @return Dendogram plot of hierarchical superclass clustering
#'
#' @examples 
#' ok.data <- iris
#' ## Build training data
#' dat <- ok.data[,c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" )]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### RNG Seed (for reproducibility)
#' ### Initialization (PCA grid)
#' init <- aweSOM::somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'), rlen = 100, alpha = c(0.05, 0.01), radius = c(6.08,-6.08), init = init, dist.fcts = 'sumofsquares')
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
#' Creates a screeplot that that provides a quality measurement of the quality of the clustering of each SOM cell 
#' for both PAM and hierarchical clustering. 
#'
#' @param ok.som SOM data object
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
#' #' ok.data <- iris
#' ## Build training data
#' dat <- ok.data[,c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" )]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### RNG Seed (for reproducibility)
#' ### Initialization (PCA grid)
#' init <- aweSOM::somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'), rlen = 100, alpha = c(0.05, 0.01), radius = c(6.08,-6.08), init = init, dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (PAM clustering)
#' superclust <- cluster::pam(ok.som$codes[[1]], 2)
#' superclasses <- unname(superclust$clustering)
#' aweSOM::aweSOMscreeplot(ok.som, method = 'hierarchical', hmethod = 'ward.D2', nclass = 2)
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
#' TBD
#'
#' @param x SOM data object
#' @param pal The color palette for visualizing distance. Default is "viridis"
#' @param reversePal Boolean whether color palette for variables is reversed. Default is FALSE
#'
#' @return Smooth distance plot
#' @export
#'
#' @examples
#' ok.data <- iris
#' ## Build training data
#' dat <- ok.data[,c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" )]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### RNG Seed (for reproducibility)
#' ### Initialization (PCA grid)
#' init <- aweSOM::somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'), rlen = 100, alpha = c(0.05, 0.01), radius = c(6.08,-6.08), init = init, dist.fcts = 'sumofsquares')
#' aweSOM::aweSOMsmoothdist(ok.som)
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
#' Create a silhouette plot that provides a quality measurement of the quality of the clustering of each SOM cell 
#' for both PAM and hierarchical clustering. 
#'
#' @param ok.som SOM data object
#' @param ok.sc Computed super-classes object resulting from either PAM or hierarchical clustering 
#'
#' @return Silhouette plot for superclass clustering
#' @export
#'
#' @examples
#' #' ok.data <- iris
#' ## Build training data
#' dat <- ok.data[,c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" )]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### RNG Seed (for reproducibility)
#' ### Initialization (PCA grid)
#' init <- aweSOM::somInit(dat,4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'), rlen = 100, alpha = c(0.05, 0.01), radius = c(6.08,-6.08), init = init, dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (PAM clustering)
#' superclust <- cluster::pam(ok.som$codes[[1]], 2)
#' superclasses <- unname(superclust$clustering)
#' aweSOM::aweSOMsilhouette(ok.som, superclasses)
aweSOMsilhouette <- function(ok.som, ok.sc){
  if (is.null(ok.som)) return()
  cluster:::plot.silhouette(cluster::silhouette(ok.sc, dist(ok.som$codes[[1]])), 
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
  clustering <- factor(som$unit.classif, 1:nrow(som$grid$pts))
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
  
  
  if (type %in% c("Camembert", "CatBarplot")) {
    if (is.numeric(data)) if (length(unique(data)) > 100) data <- cut(data, 100)
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
        
      } else if(normtype == "no_contrast"){
        ## "No contrast" normalization : global 0-1 scale, on obs/protos
        
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
      if (normtype == "no_contrast") {
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
  } else if (type == "Camembert") {
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

#' @import htmlwidgets
#' @export
aweSOMwidget <- function(ok.som, ok.sc, ok.clust, ok.data, ok.trainrows, 
                         graphType= "Hitmap", 
                         plotNames= "(rownames)", plotVarMult= NULL, plotVarOne= NULL, 
                         plotOutliers= T, plotEqualSize= F,
                         contrast= "contrast", average_format= "mean",
                         plotSize= 100, 
                         palsc= "Set3", palplot= "viridis", plotRevPal= F,
                         width = NULL, height = NULL, elementId = NULL) {
  
  if (is.null(ok.som) | !(graphType %in% c("Radar", "Camembert", "CatBarplot",
                                           "Barplot", "Boxplot", 
                                           "Color", "Star", 
                                           "Hitmap", "Line", 
                                           "Names", "UMatrix")))
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
  } else if (graphType %in% c("Color", "Camembert", "CatBarplot")) {
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


#' SOM interactive visualizations
#'
#' This function allows to create interactive visualizations of self-organizing maps (SOM) as html-widgets. 
#' The following types of visualizations can be displayed that reflect upon distribution of observations per cell and 
#' distributions of variables per cell: hitmap, radar, barplot, boxplot, lineplot. Interactive behavior enables
#' the display of the respective observations per cell as well as further statistical information on 
#' selected variables when hovering over these.
#'
#' @param ok.som SOM data object
#' @param ok.sc superclasses
#' @param ok.data SOM training dataset
#' @param omitRows Select to omit specific rows in the ok.data argument.
#' @param graphType The graph type. Either "hitmap", "radar", "barplot", "boxplot" ,or "lineplot". Default is "hitmap"
#' @param plotNames Select variable by which observations per each cell are displayed as you hover over a cell. Default is "(rownames)"
#' @param plotVarMult Plotted variables. Concerning the radar, barplot, boxplot, lineplot, this argument allows selecting the variables
#' which should be displayed within the SOM visualization as a vector of characters indicating the names of the variables. Concering
#' visualizations of categorical variables, only one variable can be passed to the functon.
#' @param plotVarOne Julien you probabyl will mege this one so I leave it blank
#' @param plotOutliers Indicating whether outlier variables in the "boxplot" SOM visualization are displayed. Default is TRUE (outliers are displayed)
#' @param plotEqualSize ???
#' @param contrast Controls the scaling of the variables. Default is "constrast" which scales each of the variables indepenently. 
#' Alternatively, same scales can be used where all variables are displayed on the identical scaled based on global minimum and 
#' maximum values by using "no_constrast". Or the observations range by using "range"
#' @param average_format The type of average used for the visualizated variables. Default option is "mean", alternatively 
#' "median" can be used to displayed the median value of the variables or "prototype" which is the value of the SOM cell protoype
#' @param plotSize Plot size of the SOM visualization measured as integer value in pixel. Default is 100.
#' @param palsc The color palette for visualizing superclasses of SOM. Default is "Set3"
#' @param palplot The  color palette for visualizing variables in SOM cells. Default is "viridis"
#' @param plotRevPal Boolean whether color palette for variables is reversed. Default is FALSE
#'
#' @return Interactive html-widget visualizing a SOM.
#' @export
#'
#' @examples
#' ok.data <- iris
#' ## Build training data
#' dat <- ok.data[,c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" )]
#' ### Scale training data
#' dat <- scale(dat)
#' ## Train SOM
#' ### RNG Seed (for reproducibility)
#' ### Initialization (PCA grid)
#' init <- aweSOM::somInit(dat, 4, 4)
#' ok.som <- kohonen::som(dat, grid = kohonen::somgrid(4, 4, 'hexagonal'), rlen = 100, alpha = c(0.05, 0.01), radius = c(6.08,-6.08), init = init, dist.fcts = 'sumofsquares')
#' ## Group cells into superclasses (PAM clustering)
#' superclust <- cluster::pam(ok.som$codes[[1]], 2)
#' superclasses <- unname(superclust$clustering)
#' variables <- c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")
#' #Hitmap
#' aweSOM::aweSOMplot(ok.som = ok.som, ok.sc = superclasses, ok.data = ok.data, graphType = 'Hitmap', plotSize = 100)
#' #Radar
#' aweSOM::aweSOMplot(ok.som = ok.som, ok.sc = superclasses, ok.data = ok.data, graphType = 'Radar', plotVarMult = variables, plotSize = 100)
#' #Barplot
#' aweSOM::aweSOMplot(ok.som = ok.som, ok.sc = superclasses, ok.data = ok.data,  graphType = 'Barplot',  plotVarMult = variables,  plotSize = 100)
#' #Boxplot
#' aweSOM::aweSOMplot(ok.som = ok.som, ok.sc = superclasses, ok.data = ok.data, graphType = 'Boxplot', plotVarMult = variables, plotSize = 100)
#' #Lineplot
#' aweSOM::aweSOMplot(ok.som = ok.som, ok.sc = superclasses, ok.data = ok.data, graphType = 'Line', plotVarMult = variables, plotSize = 100)
aweSOMplot <- function(ok.som, ok.sc= NULL, ok.data, omitRows= NULL, 
                       graphType= "Hitmap", 
                       plotNames= "(rownames)", plotVarMult= NULL, plotVarOne= NULL, 
                       plotOutliers= T, plotEqualSize= F,
                       contrast= "contrast", average_format= "mean",
                       plotSize= 100, 
                       palsc= "Set3", palplot= "viridis", plotRevPal= F, 
                       elementId= NULL) {
  ok.trainrows <- rep(T, nrow(ok.data))
  if (length(omitRows) > 0) ok.trainrows[omitRows] <- F
  
  if (is.null(ok.sc)) ok.sc <- rep(1, nrow(ok.data))
  ok.sc <- unname(ok.sc)

  res <- aweSOMwidget(ok.som, ok.sc = ok.sc, ok.data = ok.data, 
                      ok.trainrows = ok.trainrows, graphType = graphType, 
                      plotNames = plotNames,
                      plotVarMult= plotVarMult, plotVarOne= plotVarOne, 
                      plotOutliers = plotOutliers, plotEqualSize = plotEqualSize, 
                      contrast = contrast, average_format = average_format, 
                      plotSize = plotSize, 
                      palsc = palsc, palplot = palplot, plotRevPal = plotRevPal, 
                      elementId = elementId)
  
  elementId <- res$elementId
  if(is.null(elementId)) {
    elementId <- paste0('aweSOMwidget-', htmlwidgets:::createWidgetId())
    res$elementId <- elementId
  }
  
  res <- htmlwidgets::prependContent(res, htmltools::tag("h4", list(id= paste0(elementId, "-info"))))
  res <- htmlwidgets::prependContent(res, htmltools::tag("h4", list(id= paste0(elementId, "-message"))))
  res <- htmlwidgets::appendContent(res, htmltools::tag("p", list(id= paste0(elementId, "-names"))))
  if (graphType %in% c("Barplot", "Boxplot", "Radar", "Camembert", "CatBarplot"))
    res <- htmlwidgets::appendContent(res, htmltools::tag("svg", list(id= paste0(elementId, "-legend"), width = "100%")))

  res
}


