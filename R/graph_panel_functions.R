library(matrixStats)
library(FIACH)
library(RJSONIO)



#' D3-based plot data generation
#' Function generates a list object containing data for D3-based plots
#' @param type 
#' @param som 
#' @param superclass 
#' @param data 
#' @param plotsize 
#' @param varnames 
#' @param normtype 
#' @param palsc 
#' @param palplot 
#' @param cellNames 
#' @param plotOutliers 
#' @param reversePal 
#' @param options 
#' @param the.average_format 
#'
#' @return
#'
#' @examples
getPlotParams <- function(type, som, superclass, data, plotsize, varnames, 
                          normtype= c("range", "contrast"), palsc, palplot, 
                          cellNames, plotOutliers, reversePal, options= NULL, 
                          the.average_format) {
  
  ## Paramètres communs à tous les graphiques
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
              saveToPng= TRUE, #<- previously true
              sizeInfo= plotsize, 
              gridInfo= gridInfo, 
              superclass= superclass, 
              superclassColor= superclassColor, 
              cellNames= cellNames, 
              cellPop= unname(clust.table))
  
  ## Traitement data si besoin :
  if (type %in% c("Camembert", "CatBarplot")) {
    if (is.numeric(data)) if (length(unique(data)) > 100) data <- cut(data, 100)
    data <- as.factor(data)
    unique.values <- levels(data)
    nvalues <- nlevels(data)
  } else if (type %in% c("Radar", "Line", "Barplot", "Boxplot", "Color", "Star")) {
    if (is.null(dim(data))) {
      data <- data.frame(data)
      colnames(data) <- varnames
    } else data <- as.data.frame(data)
    if (type == "Color") 
      data <- as.data.frame(sapply(data, as.numeric))
    
    nvar <- length(varnames)
    
    if (type %in% c("Radar", "Line", "Barplot", "Color", "Star")) {
      #browser()      
      ## Means by cell
      if (normtype == "range") { 
        ## "Range" normalization : data range to [0,1], then means
        normDat <- as.data.frame(sapply(data, function(x) .05 + .9 * (x - min(x)) / (max(x) - min(x))))
        #for the prototypes data
        prototypes_data <- as.data.frame(som$codes)
        normDat_prototypes <- as.data.frame(sapply(prototypes_data, function(x) .05 + .9 * (x - min(x)) / (max(x) - min(x))))
       

        if(the.average_format == "mean"){ #using input$average_format does not work since this is not within the server function context
          normValues <- unname(lapply(split(normDat, clustering), 
                                      function(x) {
                                        if (!nrow(x)) return(rep(NA, nvar))
                                        unname(colMeans(x))
                                      }))
          realValues <- unname(lapply(split(data, clustering), 
                                      function(x) {
                                        if (!nrow(x)) return(rep(NA, nvar))
                                        unname(round(colMeans(x), 3)) #is this what is supposed to be edited to pass other data?
                                      }))
        }
        
        
        if(the.average_format == "median"){
          normValues <- unname(lapply(split(normDat, clustering), 
                                      function(x) {
                                        if (!nrow(x)) return(rep(NA, nvar))
                                        unname(colMedian(x))
                                      }))
          realValues <- unname(lapply(split(data, clustering), 
                                      function(x) {
                                        if (!nrow(x)) return(rep(NA, nvar))
                                        unname(round(colMedian(x), 3)) #is this what is supposed to be edited to pass other data?
                                      }))
        }
        
        if(the.average_format == "prototypes"){
          normValues <- unname(split((unname(normDat_prototypes)), seq(nrow(normDat_prototypes))))
          normValues <- lapply(normValues, as.numeric)
          realValues <-   unname(split((unname(prototypes_data)), seq(nrow(prototypes_data))))
          realValues <- lapply(realValues, as.numeric)
        }

      } else if (normtype == "contrast") {
        
        if(the.average_format == "mean"){
        
        ## "Contrast" normalization : means on data, then range(means) -> [0,1]
        realValues <- do.call(rbind, lapply(split(data, clustering), 
                                            function(x) {
                                              if (!nrow(x)) return(rep(NA, nvar))
                                              unname(round(colMeans(x), 3)) # matrixStats for colMedians
                                            }))
        normValues <- apply(realValues, 2, function(x) 
          .05 + .9 * (x - min(x, na.rm= T)) / (max(x, na.rm= T) - min(x, na.rm= T)))
        realValues <- unname(as.list(as.data.frame(t(realValues))))
        normValues <- unname(as.list(as.data.frame(t(normValues))))
        }
        
        if(the.average_format == "median"){
          realValues <- do.call(rbind, lapply(split(data, clustering), 
                                              function(x) {
                                                if (!nrow(x)) return(rep(NA, nvar))
                                                unname(round(colMedian(x), 3)) # matrixStats for colMedians
                                              }))
          normValues <- apply(realValues, 2, function(x) 
            .05 + .9 * (x - min(x, na.rm= T)) / (max(x, na.rm= T) - min(x, na.rm= T)))
          realValues <- unname(as.list(as.data.frame(t(realValues))))
          normValues <- unname(as.list(as.data.frame(t(normValues))))
        }
        
        
        if(the.average_format == "prototypes"){
          prototypes_data <- as.data.frame(som$codes) #<-not sure if this is correct calculytion here for contrast?! 
          normDat_prototypes <- as.data.frame(sapply(prototypes_data, function(x) .05 + .9 * (x - min(x)) / (max(x) - min(x))))
          normValues <- unname(split((unname(normDat_prototypes)), seq(nrow(normDat_prototypes))))
          normValues <- lapply(normValues, as.numeric)
          realValues <-   unname(split((unname(prototypes_data)), seq(nrow(prototypes_data))))
          realValues <- lapply(realValues, as.numeric)
        }

      }
      
      else if(normtype == "no_contrast"){

        if(the.average_format == "mean"){
          
        normDat <- as.data.frame(sapply(data, function(x) .05 + .9 * (x - min(data)) / (max(data) - min(data))))
        normValues <- unname(lapply(split(normDat, clustering), 
                                    function(x) {
                                      if (!nrow(x)) return(rep(NA, nvar))
                                      unname(colMeans(x))
                                    }))
        realValues <- do.call(rbind, lapply(split(data, clustering), 
                                            function(x) {
                                              if (!nrow(x)) return(rep(NA, nvar))
                                              unname(round(colMeans(x), 3))
                                            }))
        }
        
        
        if(the.average_format == "median"){
          normDat <- as.data.frame(sapply(data, function(x) .05 + .9 * (x - min(data)) / (max(data) - min(data))))
          normValues <- unname(lapply(split(normDat, clustering), 
                                      function(x) {
                                        if (!nrow(x)) return(rep(NA, nvar))
                                        unname(colMedian(x))
                                      }))
          realValues <- do.call(rbind, lapply(split(data, clustering), 
                                              function(x) {
                                                if (!nrow(x)) return(rep(NA, nvar))
                                                unname(round(colMedian(x), 3))
                                              }))
        }
        
        
        if(the.average_format == "prototypes"){
          
          prototypes_data <- as.data.frame(som$codes) #<-not sure if this is correct calculytion here for contrast?! 
          normDat_prototypes <- as.data.frame(sapply(prototypes_data, function(x) .05 + .9 * (x - min(prototypes_data)) / (max(prototypes_data) - min(prototypes_data)))) #<- again not sure
          normValues <- unname(split((unname(normDat_prototypes)), seq(nrow(normDat_prototypes))))
          normValues <- lapply(normValues, as.numeric)
          realValues <-   unname(split((unname(prototypes_data)), seq(nrow(prototypes_data))))
          realValues <- lapply(realValues, as.numeric)
         }
        }
      
      
      if (type == "Color") {
        ## 8 colors (equal-sized bins of values) of selected palette
        normValues <- do.call(rbind, normValues)
        normValues <- apply(normValues, 2, function(x) 
          getPalette(palplot, 8, reversePal)[cut(x, seq(.049, .951, length.out= 9))])
        normValues[is.na(normValues)] <- "#FFFFFF"
      }
    } else if (type == "Boxplot") {
      normDat <- as.data.frame(sapply(data, function(x) .05 + .9 * (x - min(x)) / (max(x) - min(x))))
      normValues <- unname(lapply(split(normDat, clustering), 
                                  function(x) {
                                    if (!nrow(x)) return(rep(NA, nvar))
                                    unname(colMeans(x))
                                  }))
      normDat <- as.data.frame(sapply(data, function(x) (x - min(x)) / (max(x) - min(x))))
      data <- as.data.frame(apply(data, 2, as.numeric)) # To prevent weird JS error (when a type is in integer)
    }
  }
  
  
  ## Paramètres spécifiques :
  if (type == "Camembert") {
    res$parts <- nvalues
    res$label <- unique.values
    res$labelColor <- getPalette(palplot, nvalues, reversePal)
    if (options$equalSize) {
      res$pieNormalizedSize <- rep(.9, length(clust.table))
    } else
      res$pieNormalizedSize <- unname(.9 * sqrt(clust.table) / sqrt(max(clust.table)))
    res$pieRealSize <- unname(clust.table)
    res$pieNormalizedValues <- unname(lapply(split(data, clustering), 
                                             function(x) {
                                               if (!length(x)) return(rep(1/nvalues, nvalues))
                                               unname(table(x) / length(x))
                                             }))
    res$pieRealValues <- unname(lapply(split(data, clustering), 
                                       function(x) unname(table(x))))
    
    #print(res$labelColor)
    
  } else if (type == "CatBarplot") {
    res$nbBatons <- nvalues
    res$isHist <- FALSE
    res$isCatBarplot <- TRUE
    res$label <- unique.values
    res$labelColor <- getPalette(palplot, nvalues, reversePal)
    res$batonRealValues <- unname(lapply(split(data, clustering), 
                                         function(x) unname(table(x))))
    if (normtype == "contrast") {
      maxValue <- max(do.call(c, lapply(split(data, clustering), 
                                        function(x) {
                                          if (!length(x)) return(rep(0, nvalues))
                                          unname(table(x) / length(x))
                                        })))
    } else maxValue <- 1
    res$batonNormalizedValues <- unname(lapply(split(data, clustering), 
                                               function(x) {
                                                 if (!length(x)) return(rep(0, nvalues))
                                                 .02 + .98 * unname(table(x) / length(x)) / maxValue
                                               }))
  } else if (type == "Radar") {
    res$parts <- nvar
    res$label <- varnames
    res$labelColor <- getPalette(palplot, nvar, reversePal)
    res$radarNormalizedSize <- unname(clust.table > 0)
    res$radarRealSize <- unname(clust.table)
    res$radarNormalizedValues <- normValues
    res$radarRealValues <- realValues
  } else if (type == "Hitmap") {
    res$hitmapNormalizedValues <- unname(.9 * sqrt(clust.table) / sqrt(max(clust.table))) #taken care
    res$hitmapRealValues <- unname(clust.table)
  } else if (type == "Line") {
    res$nbPoints <- nvar
    res$label <- varnames
    res$lineNormalizedValues <- normValues
    res$lineRealValues <- realValues    
  } else if (type == "Barplot") {
    res$nbBatons <- nvar
    res$isHist <- FALSE
    res$isCatBarplot <- FALSE
    res$label <- varnames
    res$labelColor <- getPalette(palplot, nvar, reversePal)
    res$batonNormalizedValues <- normValues
    res$batonRealValues <- realValues
  } else if (type == "Boxplot") {
    res$nbBox <- nvar
    res$label <- varnames
    res$labelColor <- getPalette(palplot, nvar, reversePal)
    
    boxes.norm <- lapply(split(normDat, clustering), boxplot, plot= F)
    boxes.real <- lapply(split(data, clustering), boxplot, plot= F)
    res$boxPlotNormalizedValues <- unname(lapply(boxes.norm, function(x) unname(as.list(as.data.frame(x$stats)))))
    res$boxPlotRealValues <- unname(lapply(boxes.real, function(x) unname(as.list(as.data.frame(x$stats)))))
    if (plotOutliers) {
      res$boxNormalizedExtremesValues <- json_edits(unname(lapply(boxes.norm, function(x) unname(split(x$out, factor(x$group, levels= 1:nvar))))))
      res$boxRealExtremesValues <- json_edits(unname(lapply(boxes.real, function(x) as.list(unname(split(x$out, factor(x$group, levels= 1:nvar)))))))
      

      
      
    } else {
      res$boxNormalizedExtremesValues <- json_edits(unname(lapply(boxes.norm, function(x) lapply(1:nvar, function(y) numeric(0)))))
      res$boxRealExtremesValues <- json_edits(unname(lapply(boxes.real, function(x) lapply(1:nvar, function(y) numeric(0)))))
    }
  } else if (type == "Color") {
    res$activate <- TRUE
    res$colorNormalizedValues <- normValues
    res$colorRealValues <- realValues   
    res$label <- varnames
  } else if (type == "Star") {
    res$nbSommet <- nvar
    res$label <- varnames
    res$starPlotNormalizedValues <- normValues
    res$starPlotRealValues <- realValues
  } else if (type == "Names") {
    res$wordClouds <- unname(split(data, clustering))
    res$nbWord <- unname(sapply(res$wordClouds, length))
  }
  
  if (type == "CatBarplot")
    res$plotType <- "Barplot"
  
  res
}





#' Generate R-based legend
#'
#' @param plot.data 
#' @param input_plotNames 
#' @param ok.clust 
#' @param input_graphType 
#' @param input_plotVarMult 
#' @param input_plotVarOne 
#' @param ok.som 
#' @param input_plotEqualSize 
#' @param input_contrast 
#' @param input_average_format 
#' @param ok.sc 
#' @param input_plotSize 
#' @param input_palsc 
#' @param input_palplot 
#' @param input_plotOutliers 
#' @param input_plotRevPal 
#'
#' @return
#' @export
#'
#' @examples
the.legend.function <- function(plot.data, input_plotNames, ok.clust, input_graphType, input_plotVarMult, input_plotVarOne,
                                ok.som, input_plotEqualSize, input_contrast, input_average_format, ok.sc,
                                input_plotSize, input_palsc, input_palplot,
                                input_plotOutliers, input_plotRevPal){
  
  
  if (input_plotNames == "(rownames)") {
    plotNames.var <- rownames(plot.data)
  } else 
    plotNames.var <- as.character(plot.data[, input_plotNames])
  cellNames <- unname(lapply(split(plotNames.var, ok.clust), 
                             function(x) paste(sort(x), collapse= ", "))) # "&#13;&#10;" "<br />"
  
  
  
  if (input_graphType %in% c("Radar", "Star", "Barplot", "Boxplot", "Line")) {
    if (is.null(input_plotVarMult)) return(NULL)
    plotVar <- input_plotVarMult
    data <- plot.data[, plotVar]
  } 
  
  else if (input_graphType %in% c("Color", "Camembert", "CatBarplot")) {
    if (is.null(input_plotVarOne)) return(NULL)
    plotVar <- input_plotVarOne
    data <- plot.data[, plotVar]
  } 
  
  
  else if (input_graphType %in% c("Hitmap")) {
    plotVar <- NULL
    data <- NULL
  } 
  
  else if (input$graphType %in% c("Names")) {
    plotVar <- NULL
    data <- as.character(plot.data[, input_plotVarOne])
  } 
  
  else if (input_graphType == "UMatrix") {
    plotVar <- NULL
    proto.gridspace.dist <- as.matrix(dist(ok.som$grid$pts))
    proto.dataspace.dist <- as.matrix(dist(ok.som$codes[[1]]))
    proto.dataspace.dist[round(proto.gridspace.dist, 3) > 1] <- NA
    proto.dataspace.dist[proto.gridspace.dist == 0] <- NA
    data <- rowMeans(proto.dataspace.dist, na.rm= T)[ok.clust]
    plotVar <- "Mean distance to neighbours"
  }
  
  options <- list(equalSize= input_plotEqualSize)
  
  graphType <- ifelse(input_graphType == "UMatrix", "Color", input_graphType)
  res <- getPlotParams(graphType, ok.som, ok.sc,  
                       data, input_plotSize, plotVar, input_contrast,
                       input_palsc, input_palplot, cellNames,
                       input_plotOutliers, input_plotRevPal, 
                       options, input_average_format)
  
  legend_data <- data.frame(cbind(res$label, res$labelColor, seq(1,length(res$label))))
  grab_legend(ggplot(data = legend_data, aes(x = X3, y = X3))+
                geom_tile(aes(fill = X2))+
                scale_fill_identity(guide = "legend", labels = res$label)+
                theme(legend.position = "bottom",
                      legend.title = element_blank(),
                      legend.key.size = unit(2,"line"),
                      legend.text = element_text(size = 20)))
  
}





#' Generate plot color palets
#' Function that generates color pallets
#' @param pal 
#' @param n 
#' @param reverse 
#'
#' @return
#'
#' @examples
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
#' @param ok.som SOM data object
#' @param ok.hclust hierarchical clustering object
#' @param input_kohSuperclass number of Kohonen superclasses
#'
#' @return Dendogram plot of hierarchical clustering
#'
#' @examples
aweSOMdendrogram <- function(ok.som, ok.hclust, input_kohSuperclass){
  
  if (is.null(ok.som)) return()
  plot(ok.hclust, xlab= "", main= "")
  if (input_kohSuperclass > 1)
    rect.hclust(ok.hclust, k= input_kohSuperclass)
}




#' Plot screeplot for clustering of superclasses
#'
#' @param ok.som 
#' @param ok.hclust 
#' @param input_kohSuperclass 
#'
#' @return
#' @export
#'
#' @examples
aweSOMscreeplot <- function(ok.som, ok.hclust, input_kohSuperclass){
  ## TODO : adapt for PAM
  
  if (is.null(ok.som)) return()
  ncells <- nrow(ok.som$codes[[1]])
  nvalues <- max(input_kohSuperclass, min(ncells, max(ceiling(sqrt(ncells)), 10)))
  clust.var <- sapply(1:nvalues, function(k) {
    clust <- cutree(ok.hclust, k)
    clust.means <- do.call(rbind, by(ok.som$codes[[1]], clust, colMeans))[clust, ]
    mean(rowSums((ok.som$codes[[1]] - clust.means)^2))
  })
  unexpl <- 100 * round(clust.var / 
                          (sum(apply(ok.som$codes[[1]], 2, var)) * (ncells - 1) / ncells), 3)
  plot(unexpl, t= "b", ylim= c(0, 100),
       xlab= "Nb. Superclasses", ylab= "% Unexpl. Variance")
  grid()                      
  abline(h= unexpl[input_kohSuperclass], col= 2)
  
}



#' Smooth distance plot
#'
#' @param ok.som 
#' @param ok.dist 
#' @param input_palplot 
#' @param input_plotRevPal 
#'
#' @return
#' @export
#'
#' @examples
aweSOMsmoothdist <- function(ok.som, ok.dist, input_palplot, input_plotRevPal){
  if (is.null(ok.som)) return()
  
  values <- matrix(rowMeans(ok.dist$proto.data.dist.neigh, na.rm= T), 
                   ok.som$grid$ydim, ok.som$grid$xdim)
  filled.contour(1:ok.som$grid$ydim, 1:ok.som$grid$xdim, #function generating the plot
                 #only accepts square grid
                 values[, ok.som$grid$xdim:1],
                 
                 
                 color.palette= function(x) paste0(getPalette(input_palplot, x, input_plotRevPal), "FF"))
  
}




#' Abstraction plot
#'
#' @param ok.som 
#' @param ok.traindat 
#' @param input_plotAbstrCutoff 
#' @param input_palplot 
#' @param input_plotRevPal 
#'
#' @return
#' @export
#'
#' @examples
plot.abstraction <- function(ok.som, ok.traindat, input_plotAbstrCutoff, input_palplot, input_plotRevPal){
  if (is.null(ok.som)) return()
  
  somcodes <- ok.som$codes[[1]]
  nsomvars <- ncol(somcodes)
  nsomnodes <- nrow(somcodes)
  gridpoints <- ok.som$grid$pts
  
  nodeweights <- apply(somcodes, 2, function(x) {
    y <- (x - min(x)) / (max(x) - min(x))
    y^2 / sum(y^2)
    # exp(3 * y) / sum(exp(3 * y))
  })
  varcoords <- t(nodeweights) %*% gridpoints
  
  adjweights <- aggregate(.~somcell, FUN = function(x) sum(x - min(0, min(x))),
                          na.action= na.pass,
                          data.frame(ok.traindat$dat, 
                                     somcell= ok.som$unit.classif))
  if (any(! 1:nrow(gridpoints) %in% ok.som$unit.classif)) {
    losers <- which(!(1:nrow(gridpoints) %in% ok.som$unit.classif))
    for (ilose in losers) {
      darow <- as.data.frame(t(c(ilose, rep(0, ncol(ok.traindat$dat)))))
      colnames(darow) <- colnames(adjweights)
      if (ilose == 1) {
        adjweights <- rbind(darow, adjweights)
      } else
        adjweights <- rbind(adjweights[1:(ilose-1), ], 
                            darow, 
                            adjweights[ilose:(nrow(adjweights)), ])
    }
  }
  adjweights <- as.matrix(apply(adjweights[, -1], 2, function(y) y^2 / sum(y^2)))
  adjcoords <- t(adjweights) %*% gridpoints
  adjweightscut <- adjweights
  adjweightscut[adjweights < input_plotAbstrCutoff] <- 0
  
  vargraph <- igraph::graph_from_adjacency_matrix(crossprod(nodeweights),
                                                  "undirected", diag= F, weighted= T)
  vargraph.louvain <- igraph::communities(igraph::cluster_louvain(vargraph))
  varclust <- sapply(colnames(nodeweights),
                     function(x) for (i in 1:length(vargraph.louvain))
                       if (x %in% vargraph.louvain[[i]]) return(i))
  dacolors <- unlist(getPalette(input_palplot, length(unique(varclust)), input_plotRevPal)[varclust])
  
  
  #ggplot scheme to generate the code
  
  gg <- ggplot2::ggplot(data.frame(as.data.frame(gridpoints),
                                   pop= as.vector(table(factor(ok.som$unit.classif,
                                                               levels = 1:nsomnodes)))),
                        ggplot2::aes(x, y)) + ggplot2::scale_y_reverse() +
    ggplot2::theme_void() + 
    ggforce::geom_circle(ggplot2::aes(r= .1 + .4 * (sqrt(pop) - min(sqrt(pop))) / (max(sqrt(pop)) - min(sqrt(pop))), 
                                      x0= x, y0= y), fill= "white", 
                         show.legend = F, inherit.aes = F) +
    ggplot2::geom_segment(mapping= ggplot2::aes(x, y, xend= xend, yend= yend,
                                                color= dacolor, linetype= datype,
                                                size= daweight, alpha= daweight),
                          inherit.aes = F, show.legend = F,
                          data= data.frame(x= rep(gridpoints[, 1], each= nsomvars),
                                           y= rep(gridpoints[, 2], each= nsomvars),
                                           xend= rep(adjcoords[, 1], times= nsomnodes),
                                           yend= rep(adjcoords[, 2], times= nsomnodes),
                                           dacolor= rep(dacolors, times= nsomnodes),
                                           datype= rep(colnames(somcodes), times= nsomnodes),
                                           daweight= as.vector(t(adjweightscut)))[t(adjweightscut) != 0, ]) +
    ggplot2::geom_point(ggplot2::aes(color= dacolors),
                        data= as.data.frame(adjcoords, dacolors),
                        size= 3, pch= 24, show.legend = F) +
    ggrepel::geom_label_repel(ggplot2::aes(color= dacolors, label= activite,
                                           size= taille, x= x, y= y),
                              inherit.aes = F, show.legend = F,
                              data= data.frame(adjcoords, dacolors,
                                               activite= rownames(adjcoords),
                                               taille= 2* (1 - colSums(adjweights^2)))) +
    ggplot2::coord_fixed()
  print(gg)
}






#' Silhouette plot for PAM clustering
#'
#' @param ok.som 
#' @param ok.pam_clust 
#' @param input_sup_clust_method 
#'
#' @return
#' @export
#'
#' @examples
plot.pam_silhouette <- function(ok.som, ok.pam_clust, input_sup_clust_method){
  if (is.null(ok.som)) return()
  
  
  #browser()
  #since the silhouette only works out for pam
  if(input_sup_clust_method == "hierarchical") return()
  
  
  library(factoextra)
  
  fviz_silhouette(ok.pam_clust, palette = "jco",
                  ggtheme = theme_classic(), title="PAM-Clusters")
  
  
}








#' Help function
#' Probably I should integrate this somewhere else
#' @param test 
#'
#' @return
#' @export
#'
#' @examples
json_edits <- function(test){
  for(index in seq(1, length(test))){
    for(index_2 in seq(1, length(test[[index]]))){
      if(length(test[[index]][[index_2]]) == 0){
        test[[index]][[index_2]] <- 0
      }}}
  return(test) 
}




## htmlwidgets binding
#' @import htmlwidgets
#' @export
aweSOMwidget <- function(ok.som, ok.sc, ok.clust, ok.data, ok.trainrows, 
                         graphType= "Hitmap", 
                         plotNames= "(rownames)", plotVarMult= NULL, plotVarOne= NULL, 
                         plotOutliers= T, plotEqualSize= F,
                         contrast= "contrast", average_format= "mean",
                         plotSize= 100, 
                         palsc= "Set3", palplot= "viridis", plotRevPal= F,
                         width = NULL, height = NULL) {
  
  if (is.null(ok.som) | !(graphType %in% c("Radar", "Camembert", "CatBarplot",
                                           "Barplot", "Boxplot", 
                                           "Color", "Star", 
                                           "Hitmap", "Line", 
                                           "Names", "UMatrix")))
    return(NULL) # si on n'a pas calculé, on donne NULL à JS
  
  ok.clust <- ok.som$unit.classif
  plot.data <- ok.data[ok.trainrows, ]
  if(is.null(plot.data)) return(NULL)
  # Obs names per cell for message box
  if (is.null(plotNames)){
    return(NULL)
    
  } 
  if (plotNames == "(rownames)") {
    plotNames.var <- rownames(plot.data)
  } 
  else {
    plotNames.var <- as.character(plot.data[, plotNames])
  }
  
  cellNames <- unname(lapply(split(plotNames.var, ok.clust), 
                             function(x) paste(sort(x), collapse= ", "))) # "&#13;&#10;" "<br />"
  
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
  } else if (graphType == "UMatrix") {
    plotVar <- NULL
    proto.gridspace.dist <- as.matrix(dist(ok.som$grid$pts))
    proto.dataspace.dist <- as.matrix(dist(ok.som$codes[[1]]))
    proto.dataspace.dist[round(proto.gridspace.dist, 3) > 1] <- NA
    proto.dataspace.dist[proto.gridspace.dist == 0] <- NA
    data <- rowMeans(proto.dataspace.dist, na.rm= T)[ok.clust]
    plotVar <- "Mean distance to neighbours"
  }
  
  options <- list(equalSize= plotEqualSize)
  
  graphType <- ifelse(graphType == "UMatrix", "Color", graphType)
  
  plotParams <- aweSOM:::getPlotParams(graphType, ok.som, ok.sc,  
                                       data, plotSize, plotVar, contrast,
                                       palsc, palplot, cellNames,
                                       plotOutliers, plotRevPal, options, 
                                       average_format)
  
  # create the widget
  htmlwidgets::createWidget("aweSOMwidget", plotParams, width = width, height = height, package = "aweSOM")
}

aweSOMplot <- function(ok.som, ok.sc, ok.data, ok.trainrows, 
                       graphType= "Hitmap", 
                       plotNames= "(rownames)", plotVarMult= NULL, plotVarOne= NULL, 
                       plotOutliers= T, plotEqualSize= F,
                       contrast= "contrast", average_format= "mean",
                       plotSize= 100, 
                       palsc= "Set3", palplot= "viridis", plotRevPal= F) {
  res <- aweSOMwidget(ok.som, ok.sc = ok.sc, ok.data = ok.data, 
                      ok.trainrows = ok.trainrows, graphType = graphType, 
                      plotNames = plotNames,
                      plotVarMult= plotVarMult, plotVarOne= plotVarOne, 
                      plotOutliers = plotOutliers, plotEqualSize = plotEqualSize, 
                      contrast = contrast, average_format = average_format, 
                      plotSize = plotSize, 
                      palsc = palsc, palplot = palplot, plotRevPal = plotRevPal)
  res <- htmlwidgets::prependContent(res, htmltools::tag("a", list(id= "downloadLink")))
  res <- htmlwidgets::prependContent(res, htmltools::tag("p", list(id= "theWidget")))
  res <- htmlwidgets::prependContent(res, htmltools::tag("h4", list(id= "cell-info")))
  res <- htmlwidgets::prependContent(res, htmltools::tag("h4", list(id= "plot-message")))
  res <- htmlwidgets::appendContent(res, htmltools::tag("p", list(id= "plot-names")))
  res
}


## htmlwidgets - shiny binding
#' @export
aweSOMoutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "aweSOMwidget", width, height, package = "aweSOM")
}
#' @export
renderaweSOM <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, aweSOMoutput, env, quoted = TRUE)
  # htmlwidgets::shinyRenderWidget(expr, aweSOMoutput, env, quoted = F)
}



