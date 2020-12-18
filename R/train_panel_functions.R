




#' Title
#'
#' @param input_trainbutton i.e.  an action button 
#' @param input_trainscale i.e. an checkboxInput
#' @param ok.data i.e. the data object storing the data frame  ok.data object
#' @param varSelected i.e. selected variables to train SOM on, vector with TRUE/FALSE values
#' @param varWeights  i.e. the weights assigned to these variables (format vector of length of variables no.)
#' when no weights assigned c(1,1,1,1,...)
#'
#' @return
#'
#' @examples
ok.traindat.function <- function(input_trainscale, ok.data, varSelected, varWeights){
  
  if (is.null(ok.data)) return(NULL)
  err.msg <- NULL
  codeTxt <- list()
  
  dat <- ok.data[, varSelected]
  varWeights <- varWeights[varSelected]

  # Generate reproducible code
  codeTxt$sel <- paste0("dat <- ok.data[, c('", 
                        paste(colnames(ok.data)[varSelected], collapse= "', '"), "')]\n",
                        if (any(varWeights != 1)) {
                          paste0("varWeights <- c(", 
                                 paste(colnames(ok.data)[varSelected], 
                                       " = ", varWeights, collapse= ", "), 
                                 ")\n")
                        })
  

  # Check that all variables are numeric, otherwise message and convert
  varNumeric <- sapply(dat, is.numeric)
  if (any(!varNumeric)) {
    err.msg$numeric <- paste0("Variables < ",
                              paste(colnames(dat)[!varNumeric], collapse= ", "),
                              " > are not natively numeric, and will be forced to numeric.",
                              " (This is probably a bad idea.)")
    dat[, !varNumeric] <- as.data.frame(sapply(dat[, !varNumeric], as.numeric))
    codeTxt$numeric <- paste0("varNumeric <- sapply(dat, is.numeric)\n", 
                              "dat[, !varNumeric] <- as.data.frame(sapply(dat[, !varNumeric], as.numeric))\n")
  }
  
  # Remove NAs
  nrow.withNA <- nrow(dat)
  dat <- as.matrix(na.omit(dat))
  if (nrow(dat) < nrow.withNA) {
    err.msg$NArows <- paste(nrow.withNA - nrow(dat), 
                            "observations contained missing values, and were removed.")
    codeTxt$NArows <- "dat <- as.matrix(na.omit(dat))\n"
  }
  if (nrow(dat) == 0) {
    err.msg$NArows <- "All observations contain missing values, training impossible."
    return(list(dat= NULL, msg= err.msg))
  }
  
  # Check for constant variables (if so, exclude and message)
  varConstant <- apply(dat, 2, sd, na.rm= T) == 0
  if (any(varConstant)) {
    err.msg$constant <- paste0("Variables < ",
                               ifelse(sum(varConstant) == 1, 
                                      colnames(dat)[varConstant], 
                                      paste(colnames(dat)[varConstant], collape= ", ")),
                               " > are constant, and will be removed for training.")
    dat <- dat[, !varConstant]
    varWeights <- varWeights[!varConstant]
    codeTxt$constant <- paste0("varConstant <- apply(dat, 2, sd, na.rm= T) == 0\n", 
                               "dat <- dat[, !varConstant]\n", 
                               if (any(varWeights != 1)) paste0("varWeights <- varWeights[!varConstant]\n"))
    if (sum(!varConstant) < 2) {
      err.msg$allconstant <- "Less than two selected non-constant variables, training impossible."
      return(list(dat= NULL, msg= err.msg))
    }
  }
  
  ## Scale variables and apply normalized weights
  if (input_trainscale) dat <- scale(dat)
  varWeights <- length(varWeights) * varWeights / sum(varWeights)
  dat <- t(sqrt(varWeights) * t(dat))
  codeTxt$scale <- paste0(ifelse(input_trainscale, "### Scale training data\ndat <- scale(dat)\n", ""), 
                          if (any(varWeights != 1)) paste0(
                            "### Apply (standardized) weights\n",
                            "varWeights <- length(varWeights) * varWeights / sum(varWeights)\n", 
                            "dat <- t(sqrt(varWeights) * t(dat))\n"))
  
  codeTxt$traindat <- paste0(codeTxt$sel, 
                             if (! is.null(codeTxt$numeric)) {
                               paste0("### Warning: ", err.msg$numeric, "\n", 
                                      codeTxt$numeric)
                             },
                             if (! is.null(codeTxt$NArows)) {
                               paste0("### Warning: ", err.msg$NArows, "\n", 
                                      codeTxt$NArows)
                             },
                             if (! is.null(codeTxt$constant)) {
                               paste0("### Warning: ", err.msg$constant, "\n", 
                                      codeTxt$constant)
                             },
                             codeTxt$scale)
  
  return(list(dat= dat, msg= err.msg, codeTxt= codeTxt))
}


## Compute SOM initialization 
somInit <- function(traindat, nrows, ncols, method= c("pca.sample", "pca", "random")) {
  method <- match.arg(method)
  if (method == "random") {
    init <- traindat[sample(nrow(traindat), nrows * ncols, replace= T), ]
  } else if (method %in% c("pca.sample", "pca")) {
    # the most detailed grid axis is assigned to the first component
    if (nrows >= ncols) {
      x.ev <- 1
      y.ev <- 2
    } else {
      x.ev <- 2
      y.ev <- 1
    }
    # perform PCA
    traindata.pca <- prcomp(traindat, center= F, scale.= F)
    init.x <- seq(from= quantile(traindata.pca$x[,x.ev], .025), 
                  to= quantile(traindata.pca$x[,x.ev], .975),
                  length.out= nrows)
    init.y <- seq(from= quantile(traindata.pca$x[,y.ev], .025), 
                  to= quantile(traindata.pca$x[,y.ev], .975),
                  length.out= ncols)
    init.base <- as.matrix(expand.grid(x= init.x, y= init.y)) # here a hex variant could be created instead if hex topology
    
    if (method == "pca.sample") {
      ## As in SOMbrero, init to observations closest to a 2D PCA grid
      closest.obs <- apply(init.base, 1, function(point) 
        which.min(colSums((t(traindata.pca$x[,c(x.ev,y.ev)])-point)^2)))
      init <- traindat[closest.obs,]
    } else if (method == "pca") {
      ## Pure PCA grid
      init <- tcrossprod(init.base, traindata.pca$rotation[, 1:2])
    }
  } 
  init
}



#' Train SOM
#'
#' @param ok.traindat return object from ok.traindat.function
#' @param input_trainSeed random seed
#' @param input_kohInit "pca" for PCA initialization
#' @param input_kohDimy integer indicating y dimension of SOM
#' @param input_kohDimx integer indicating x dimension of SOM
#' @param input_kohTopo "hexagonal" or "recantgular" for geometrical structure of SOM
#' @param input_trainRlen integer. 100?
#' @param input_trainAlpha1 integer/double
#' @param input_trainAlpha2 integer/double
#' @param input_trainRadius1 integer/double
#' @param input_trainRadius2 integer/double
#'
#' @return res object
#' @export
#'
#' @examples ok.som.function(ok.traindat = ok.traindat, input_trainSeed = 61929, 
#'                           input_KohInit = "pca", input_kohDimy= 10, input_kohDimx = 10,
#'                           input_kohTopo = "hexagonal", input_trainRlen = 100,
#'                           input_trainAlpha1 = 0.05, input_trainAlpha2 = 0.01, 
#'                           input_trainRadius1 = 6.08, input_trainRadius2 = -6.08,)
ok.som.function <- function(ok.traindat,  input_trainSeed, input_kohInit,
                            input_kohDimy, input_kohDimx, input_kohTopo, input_trainRlen,
                            input_trainAlpha1, input_trainAlpha2, input_trainRadius1,
                            input_trainRadius2){
  ## TODO : set default values if this function is to be exported
  
  dat <- ok.traindat$dat # uses the list created/returned by the previous function
  if (is.null(dat)) return(NULL)
  codeTxt <- list()

  isolate({
    ## Initialization
    set.seed(input_trainSeed)
    codeTxt$seed <- paste0("set.seed(", input_trainSeed, ")\n")
    
    codeTxt$init <- paste0("### Initialization\n", 
                           "init <- aweSOM::somInit(dat, ", input_kohDimx, ", ", 
                           input_kohDimy, 
                           if (input_kohInit != "pca.sample") {
                             paste0(", method= '", input_kohInit, "'")
                           }, 
                           ")\n")
    init <- somInit(dat, input_kohDimx, input_kohDimy, input_kohInit)
    
    ## Train SOM
    res <- kohonen::som(dat, 
                        grid= kohonen::somgrid(input_kohDimx, input_kohDimy, 
                                               input_kohTopo), 
                        rlen= input_trainRlen, 
                        alpha= c(input_trainAlpha1, input_trainAlpha2), 
                        radius= c(input_trainRadius1, input_trainRadius2), 
                        init= init, dist.fcts= "sumofsquares")

    res$codeTxt <- paste0("### RNG Seed (for reproducibility)\n", 
                          codeTxt$seed,
                          codeTxt$init, 
                          "### Training\n", 
                          "ok.som <- kohonen::som(dat, grid = kohonen::somgrid(", 
                          input_kohDimx, ", ", input_kohDimy, ", '", 
                          input_kohTopo, "'), rlen = ", input_trainRlen, 
                          ", alpha = c(", input_trainAlpha1, ", ", 
                          input_trainAlpha2, "), radius = c(", 
                          input_trainRadius1, ",", input_trainRadius2, 
                          "), init = init, dist.fcts = 'sumofsquares')\n")
    
    ## save seed (a new seed will be set after training)
    res$seed <- input_trainSeed

    res
  })
}


#' Distance measures
#'
#' @param x ```kohonen``` object, a SOM created by the ```som``` function.
#'
#' @return list with distance measures
#'
#' @examples somDist(x = ok.som)
somDist <- function(x){
  if (is.null(x)) return(NULL)
  proto.gridspace.dist <- kohonen::unit.distances(x$grid, F)
  proto.dataspace.dist <- as.matrix(dist(x$codes[[1]]))
  neigh <- round(proto.gridspace.dist, 3) == 1
  proto.dataspace.dist.neigh <- proto.dataspace.dist
  proto.dataspace.dist.neigh[!neigh] <- NA
  list(proto.grid.dist= proto.gridspace.dist, 
       neigh.matrix= neigh, 
       proto.data.dist= proto.dataspace.dist, 
       proto.data.dist.neigh= proto.dataspace.dist.neigh)
}


#' Calculate SOM quality measures
#'
#' @param ok.som SOM object created by ok.som.function
#' @param traindat matrix containing the training data
#'
#' @return list with quality measures
#'
#' @examples somQuality(ok.som  = ok.som, traindat = traindat)
#' 
somQuality <- function(ok.som, traindat){
  if(!is.null(ok.som)) {
    ok.dist <- somDist(ok.som)
    
    ## BMU, Squared distance from obs to BMU
    bmu <- ok.som$unit.classif
    sqdist <- rowSums((traindat - ok.som$codes[[1]][bmu, ])^2)
    
    ## Quantization error
    err.quant <- mean(sqdist)
    
    ## Interclass variance ratio
    totalvar <- sum(apply(traindat, 2, var)) * 
      (nrow(traindat) - 1) / nrow(traindat)
    err.varratio <- 100 - round(100 * err.quant / totalvar, 2)
    
    ## Topographic error
    bmu2 <- apply(traindat, 1, function(row) {
      dist <- colSums((t(ok.som$codes[[1]]) - row)^2)
      order(dist)[2]
    })
    err.topo <- mean(!ok.dist$neigh.matrix[cbind(bmu, bmu2)])
    
    ## Kaski-Lagus error
    err.kaski <- e1071::allShortestPaths(ok.dist$proto.data.dist.neigh)$length[cbind(bmu, bmu2)]
    err.kaski <- mean(err.kaski + sqrt(sqdist))
    
    ## Distribution of individuals in cells
    cellpop <- table(factor(ok.som$unit.classif, levels= 1:nrow(ok.som$grid$pts)))
    
    res <- list(err.quant= err.quant, err.varratio= err.varratio, 
                err.topo= err.topo, err.kaski= err.kaski, cellpop= cellpop)
    class(res) <- "somQual"
    res
  }
}



