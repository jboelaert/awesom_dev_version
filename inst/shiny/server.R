################################################################################
## Global Variables
################################################################################

## List of possible plots, by "what" type
plotChoices <- list(MapInfo= c("Population map"= "Hitmap",
                               "Superclass Dendrogram"= "Dendrogram",
                               "Superclass Scree plot"= "Screeplot",
                               "Superclass Silhouette"= "Silhouette",
                               "Neighbour distance"= "UMatrix", 
                               "Smooth distance"= "SmoothDist"), 
                    Numeric= c("Circular Barplot"= "Circular", 
                               "Barplot"= "Barplot", 
                               "Boxplot"= "Boxplot",
                               "Line plot"= "Line", 
                               "Radar chart"= "Radar",
                               "Heat map (Color)"= "Color"), 
                    Categorical= c("Pie"= "Pie", 
                                   "Barplot"= "CatBarplot"))


help_messages <- list(import_data_panel = HTML("<h3>Working with aweSOM</h3> <br>
                          Use this interface to train and visualize self-organizing maps (SOM, aka Kohonen maps).
                          Use the tabs above in sequence : <br>
                          <strong>Import Data:</strong> Import the data to analyze<br>
                          <strong>Train:</strong> Train the SOM on selected variables<br>
                          <strong>Plot:</strong> Visualize the trained SOM <br>
                          <strong>Export Data:</strong> Export the trained SOM or clustered data <br>
                          <strong>R Script:</strong> Generate the R script to reproduce your analysis in R <br>
                          <strong>About:</strong> Further information on this application <br>"),
                      train_panel = HTML("<h3>Advanced Training Options</h3> <br>
                          <strong>Initialization:</strong> Method for prototype initialization. \
                          'PCA Obs' takes as prototypes the observations that are closest to \
                          the nodes of a 2d grid placed along the first two components of a PCA. \
                          The 'PCA' method uses the nodes instead of the observations.\
                          The 'Random Obs' method samples random observations.<br>
                          <strong>Rlen:</strong>  Number of times the complete data set will be presented to the network. <br>
                          <strong>Alpha:</strong> Learning rate. <br>
                          <strong>Radius:</strong> Neighborhood Radius. <br>
                          <strong>Random Seed:</strong> Seed of the pseudo-random number generator. \
                          This allows the results to be reproduced in later work.<br>
                          See help(kohonen::som) in R for more details about the training options."),
                      help_contrast = HTML("<h3>Variables scales</h3> <br>
                                           <strong>Contrast:</strong> maximum contrast. Scales the heights of each variable from minimum to maximum of the mean/median/prototype.<br>
                                           <strong>Observations Range:</strong>  Scales the heights of each variable from minimum to maximum of the observations.<br>
                                           <strong>Same Scales:</strong> All heights are displayed on the same scale, using the global minimum and maximum of the observations.<br>"),
                      help_average_format =  HTML("<h3>Values</h3> <br>
                                            What value to display <br>
                                           <strong>Observation Means:</strong> Means of observations per cell <br>
                                           <strong>Observation Medians:</strong> Medians of observations per cell <br>
                                           <strong>Prototypes:</strong> Prototype values per cell <br>")
)


################################################################################
## Main server function
################################################################################

shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  #############################################################################
  ## Panel "Import Data"
  #############################################################################

  # Current imported data
  ok.data <- reactive({
    if(input$file_type == "csv_txt"){ 
      imported_file_object <- aweSOM:::import.csv.txt(
        input_dataFile = input$dataFile ,input_header = input$header, 
        input_sep = input$sep, input_quote = input$quote, input_dec = input$dec,
        input_encoding = input$encoding, 
        input_dataFile_datapath = input$dataFile$datapath)
    } else if(input$file_type == "excel_xlsx"){
      imported_file_object <-  aweSOM:::import.excel_xlsx(
        input_dataFile = input$dataFile, input_column_names = input$column_names, 
        input_trim_spaces = input$trim_spaces, 
        input_range_specified_bol = input$range_specified_bol, 
        input_range_specs = input$range_specs,
        input_worksheet_specified_bol = input$worksheet_specified_bol,
        input_worksheet_specs = input$worksheet_specs, 
        input_dataFile_datapath = input$dataFile$datapath,
        input_rows_to_skip = input$rows_to_skip)
    } else if(input$file_type == "excel_xls"){
      imported_file_object <- aweSOM:::import.excel_xls(
        input_dataFile = input$dataFile, 
        input_column_names_xls = input$column_names_xls,
        input_trim_spaces_xls = input$trim_spaces_xls,
        input_range_specified_bol_xls = input$range_specified_bol_xls,
        input_range_specs_xls = input$range_specs_xls,
        input_worksheet_specified_bol_xls = input$worksheet_specified_bol_xls,
        input_worksheet_specs_xls = input$worksheet_specs_xls,
        input_dataFile_datapath = input$dataFile$datapath,
        input_rows_to_skip_xls = input$rows_to_skip_xls)
    } else if(input$file_type == "spss"){
      imported_file_object <- aweSOM:::import.spss(
        input_dataFile = input$dataFile, 
        input_dataFile_datapath = input$dataFile$datapath, 
        input_skip_spss = input$skip_spss)
    } else if(input$file_type == "stata"){
      imported_file_object <- aweSOM:::import.stata(
        input_dataFile = input$dataFile, 
        input_dataFile_datapath = input$dataFile$datapath)
    } else if(input$file_type == "sas_data"){
      imported_file_object <- aweSOM:::import.sas.data(
        input_dataFile = input$dataFile, 
        input_dataFile_datapath = input$dataFile$datapath)
    }
    
    isolate({
      values$codetxt$dataread <- 
        paste0("## Import Data\n", 
               "# setwd('/path/to/datafile/directory') ## Uncomment this line and set the path to the datafile's directory\n",
               imported_file_object[[2]])
    })
    imported_file_object[[1]]
  })
  
  
  # data preview table
  output$dataView <- DT::renderDataTable({
    d.input <- ok.data()
    if (is.null(d.input)) return(NULL)
    data.frame(rownames= rownames(d.input), d.input)
  })
  
  
  

  # data import message
  output$dataImportMessage <- renderUI({
    if (is.null(input$dataFile)) 
      return(h4("Data preview should appear here after import."))
    if (! is.null(input$dataFile) & is.null(ok.data())) 
      return(h4("Error during import: try different import parameters, and check that file is a text or csv table."))
    HTML("<h4> Data imported, proceed to Train panel. </h4> <br/>")
    
  })
  
  
  
  
  
  #############################################################################
  ## Panel "Train"
  #############################################################################
  
  # Update train variable slection on data change
  output$trainVarSelect <- renderUI({
    if (is.null(ok.data())) return()
    if (is.null(values$trainVarSelect)) {
      selectVars <- sapply(ok.data(), class) %in% c("integer", "numeric")
      values$trainVarSelect <- 
        paste0("<select name='trainVarChoice' multiple size='", min(50, max(10, ncol(ok.data()) / 2)) ,"'>",
               paste0("<option value='", colnames(ok.data()), "'", ifelse(selectVars, "selected", ""),">", colnames(ok.data()), "</option>", collapse= ""), 
               "</select>")
    }
    HTML(values$trainVarSelect)
  })
  
  # Update train variables options on variable selection change
  output$trainVarOptions <-renderUI({
    if (is.null(ok.data())) return()
    varclass <- sapply(ok.data()[, input$trainVarChoice], class)
    if (is.null(varclass)) return()
    isnum <- varclass %in% c("integer", "numeric")
    names(isnum) <- names(varclass) <- input$trainVarChoice
    lapply(input$trainVarChoice, function(var) {
      fluidRow(fluidRow(column(3, numericInput(paste0("trainVarWeight", var), NULL, value= 1, min= 0, max= 1e3)), 
                 column(6, checkboxInput(paste0("trainVarChoice", var), var, value= TRUE)),  
                 column(3, p(varclass[var]))),
        if(!isnum[var]) 
          fluidRow(column(3), 
                   column(9, selectInput(paste0("trainVarLevels", var), NULL,
                                         levels(as.factor(ok.data()[, var])), 
                                         levels(as.factor(ok.data()[, var])), 
                                         multiple= TRUE))))
    })
  })
  
  
  # Update train variable choice on button click
  observeEvent(input$varNum, {
    if (is.null(ok.data())) return()
    selectVars <- sapply(ok.data(), class) %in% c("integer", "numeric")
    values$trainVarSelect <- 
      paste0("<select name='trainVarChoice' multiple size='", min(50, max(10, ncol(ok.data()) / 2)) ,"'>",
             paste0("<option value='", colnames(ok.data()), "'", ifelse(selectVars, "selected", ""),">", colnames(ok.data()), "</option>", collapse= ""), 
             "</select>")
  })
  observeEvent(input$varAll, {
    if (is.null(ok.data())) return()
    values$trainVarSelect <- 
      paste0("<select name='trainVarChoice' multiple size='", min(50, max(10, ncol(ok.data()) / 2)) ,"'>",
             paste0("<option value='", colnames(ok.data()), "' selected>", colnames(ok.data()), "</option>", collapse= ""), 
             "</select>")
  })
  observeEvent(input$varNone, {
    if (is.null(ok.data())) return()
    values$trainVarSelect <- 
      paste0("<select name='trainVarChoice' multiple size='", min(50, max(10, ncol(ok.data()) / 2)) ,"'>",
             paste0("<option value='", colnames(ok.data()), "'>", colnames(ok.data()), "</option>", collapse= ""), 
             "</select>")
  })

  # Update grid dimension on data update
  observe({
    if (is.null(ok.data())) return()
    tmp.dim <- max(4, min(10, ceiling(sqrt(nrow(ok.data()) / 10))))
    updateNumericInput(session, "kohDimx", value= tmp.dim)
    updateNumericInput(session, "kohDimy", value= tmp.dim)
  })
  # Update training radius on change of grid
  observe({
    if (is.null(ok.data())) return()
    tmpgrid <- kohonen::somgrid(input$kohDimx, input$kohDimy, input$kohTopo)
    tmpgrid$n.hood <- ifelse(input$kohTopo == "hexagonal", "circular", "square")
    radius <- round(unname(quantile(kohonen::unit.distances(tmpgrid, FALSE), .67)), 2)
    updateNumericInput(session, "trainRadius1", value= radius)
    updateNumericInput(session, "trainRadius2", value= -radius)
  })
  
  
 

  ## Create training data when button is hit
  ok.traindat <- reactive({
    if (input$trainbutton == 0) return(NULL)
    input$retrainButton

    isolate({
      if (is.null(ok.data())) return(NULL)

      err.msg <- NULL
      codeTxt <- list()
      
      varSelected <- as.logical(sapply(paste0("trainVarChoice", colnames(ok.data())),
                                       function(var) isTRUE(input[[var]])))
      varWeights <- sapply(paste0("trainVarWeight", colnames(ok.data())),
                           function(var) ifelse(length(input[[var]]), input[[var]], 0))
      varSelected <- varSelected & varWeights > 0 & colnames(ok.data()) %in% input$trainVarChoice
      if (sum(varSelected) < 2)
        return(list(dat= NULL, msg= "Select at least two variables (with non-zero weight)."))

      dat <- ok.data()[, varSelected]
      varWeights <- varWeights[varSelected]
      varNumeric <- sapply(dat, is.numeric)
      
      # Generate reproducible code
      codeTxt$sel <- paste0(
        if (any(varNumeric)) {
          paste0('numVars <- c("',
                 paste(colnames(dat)[varNumeric], collapse= '", "'), '")\n')
        },
        if (any(!varNumeric)) {
          paste0('catVars <- c("',
                 paste(colnames(dat)[!varNumeric], collapse= '", "'), '")\n')
        },
        "selec.data <- import.data[, ",
        if (any(varNumeric) & any(!varNumeric)) {
          "c(numVars, catVars)]\n"
        } else if (any(varNumeric)) {
          "numVars]\n"
        } else if (any(!varNumeric)) {
          "catVars]\n"
        },
        if (any(varWeights != 1)) {
          paste0("varWeights <- c(", 
                 paste0(colnames(dat), " = ", varWeights, collapse= ", "), ")\n")
        })
      
      # Remove rows with NAs in training variables
      nrow.withNA <- nrow(dat)
      dat <- na.omit(dat)
      if (nrow(dat) < nrow.withNA) {
        err.msg$NArows <- paste(nrow.withNA - nrow(dat), 
                                "observations contained missing values, and were removed.")
        codeTxt$NArows <- paste0("NArows <- rowSums(is.na(selec.data)) > 0\n", 
                                 "selec.data <- selec.data[!NArows, ]\n")
      }
      if (nrow(dat) == 0) {
        err.msg$NArows <- "All observations contain missing values, training impossible."
        return(list(dat= NULL, msg= err.msg))
      }
      
      # Check for constant variables (if so, exclude and message)
      # varConstant <- apply(dat, 2, sd, na.rm= TRUE) == 0
      varConstant <- apply(dat, 2, function(x) all(x == x[1], na.rm= TRUE))
      if (any(varConstant)) {
        err.msg$constant <- paste0("Variables < ",
                                   ifelse(sum(varConstant) == 1, 
                                          colnames(dat)[varConstant], 
                                          paste(colnames(dat)[varConstant], collape= ", ")),
                                   " > are constant, and will be removed for training.")
        dat <- dat[, !varConstant]
        varWeights <- varWeights[!varConstant]
        varNumeric <- varNumeric[!varConstant]
        codeTxt$constant <- paste0("varConstant <- apply(selec.data, 2, sd, na.rm= TRUE) == 0\n", 
                                   "selec.data <- selec.data[, !varConstant]\n", 
                                   if (any(varWeights != 1)) paste0("varWeights <- varWeights[!varConstant]\n"))
        if (sum(!varConstant) < 2) {
          err.msg$allconstant <- "Less than two selected non-constant variables, training impossible."
          return(list(dat= NULL, msg= err.msg))
        }
      }
      
      non1weights <- any(varWeights != 1)
      
      # Transform non-numeric variables to dummies, excluding dropped levels
      if (any(!varNumeric)) {
        datQual <- aweSOM::cdt(dat[!varNumeric])
        varWeightsQual <- rep(varWeights[!varNumeric], 
                              times= sapply(dat[!varNumeric], function(x) nlevels(as.factor(x))))
        varLevels <- do.call(c, lapply(colnames(dat)[!varNumeric], 
                                       function(x) paste0(x, "_", input[[paste0("trainVarLevels", x)]])))
        droppedLevels <- colnames(datQual)[! colnames(datQual) %in% varLevels]
        varWeightsQual <- varWeightsQual[colnames(datQual) %in% varLevels]
        datQual <- datQual[, colnames(datQual) %in% varLevels]
        dat <- as.matrix(dat[varNumeric])
        varWeights <- varWeights[varNumeric]
        codeTxt$qualDummies <- paste0("### Transform categorical variables into dummies\n",
                                      "cat.data <- cdt(selec.data[, catVars])\n")
      }

      if (any(varNumeric)) {
        ## Scale variables and apply weights
        if (input$trainscale) dat <- scale(dat)
        dat <- t(t(dat) * sqrt(varWeights))
      }      
      if (any(!varNumeric)) {
        if (input$trainscale) datQual <- t(t(datQual) / sqrt(colMeans(datQual)))
        datQual <- t(t(datQual) * sqrt(varWeightsQual))
        dat <- cbind(dat, datQual)
      }
      
      if (input$trainscale) {
        codeTxt$scale <- paste0("### Scale training variables\n",
                                if (any(varNumeric)) {
                                  "num.data <- scale(selec.data[, numVars])\n"
                                }, 
                                if (any(!varNumeric)) {
                                  "cat.data <- t(t(cat.data) / sqrt(colSums(cat.data)))\n"
                                })
      } else codeTxt$scale <- "num.data <- as.matrix(selec.data[, numVars])\n"
      
      if (non1weights) {
        codeTxt$scale <- paste0(
          codeTxt$scale, 
          "### Apply variables weights\n",
          if (any(varNumeric)) "num.data <- t(t(num.data) * sqrt(varWeights[numVars]))\n",
          if (any(!varNumeric)) 
            paste0("qual.weights <- rep(varWeights[catVars], times= sapply(selec.data[catVars], function(x) nlevels(as.factor(x))))\n", 
                   "cat.data <- t(t(cat.data) * sqrt(qual.weights))\n"))
      }
      
      codeTxt$join <- paste0(
        if (any(!varNumeric)) if (length(droppedLevels) > 0) {
          paste0("### Drop unselected factor levels\n", 
                 'cat.data <- cat.data[, ! colnames(cat.data) %in% c("', 
                 paste0(droppedLevels, collapse= '", "'), '")]\n')
        }, 
        "### Finalize training data and plotting data\n", 
        if (any(!varNumeric) & !any(varNumeric)) {
          "train.data <- cat.data\n"
        } else if (any(!varNumeric) & any(varNumeric)) {
          "train.data <- cbind(num.data, cat.data)\n"
        } else {
          "train.data <- num.data\n"
        },
        if (any(!varNumeric)) {
          paste0("plot.data <- cbind(import.data, cdt(import.data[catVars]))", 
                 if (!is.null(codeTxt$NArows)) "[!NArows, ]","\n")
        } else {
          paste0("plot.data <- import.data", 
                 if (!is.null(codeTxt$NArows)) "[!NArows, ]","\n")
        })
      
      values$codetxt$traindat <- 
        paste0("\n## Build training data\n",
               codeTxt$sel, 
               if (! is.null(codeTxt$numeric)) {
                 paste0("### Warning: ", err.msg$numeric, "\n", codeTxt$numeric)},
               if (! is.null(codeTxt$NArows)) {
                 paste0("### Warning: ", err.msg$NArows, "\n", codeTxt$NArows)},
               if (! is.null(codeTxt$constant)) {
                 paste0("### Warning: ", err.msg$constant, "\n", codeTxt$constant)},
               codeTxt$qualDummies, codeTxt$scale, codeTxt$join)
      
      list(dat= dat, msg= err.msg)
    })
  })


  ## Train SOM when button is hit (triggered by change in ok.traindat)
  ok.som <- reactive({
    dat <- ok.traindat()
    # if (is.null(ok.traindat())) return(NULL)
    # if (is.null(ok.traindat()$dat)) return(NULL)
    if (is.null(dat)) return(NULL)
    if (is.null(dat$dat)) return(NULL)
    dat <- dat$dat
    
    isolate({
      ## Repro code
      values$codetxt$train <- 
        paste0("\n## Train SOM\n", 
               "### RNG Seed (for reproducibility)\n", 
               "set.seed(", input$trainSeed, ")\n",
               "### Initialization\n", 
               'init <- somInit(train.data, ', input$kohDimx, ', ', input$kohDimy, 
               if (input$kohInit != "pca.sample") {
                 paste0(', method= "', input$kohInit, '"')
               }, 
               ")\n",
               "### Training\n", 
               "the.som <- kohonen::som(train.data, grid = kohonen::somgrid(", 
               input$kohDimx, ", ", input$kohDimy, ', "', 
               input$kohTopo, '"), rlen = ', input$trainRlen, 
               ", alpha = c(", input$trainAlpha1, ", ", 
               input$trainAlpha2, "), radius = c(", 
               input$trainRadius1, ",", input$trainRadius2, 
               '), init = init, dist.fcts = "sumofsquares")\n')
      
      ## Initialization
      set.seed(input$trainSeed)
      init <- aweSOM::somInit(dat, input$kohDimx, input$kohDimy, input$kohInit)
      
      ## Train SOM
      res <- kohonen::som(dat,
                 grid= kohonen::somgrid(input$kohDimx, input$kohDimy, 
                                        input$kohTopo), 
                 rlen= input$trainRlen, 
                 alpha= c(input$trainAlpha1, input$trainAlpha2), 
                 radius= c(input$trainRadius1, input$trainRadius2), 
                 init= init, dist.fcts= "sumofsquares")
      
      ## Save seed
      res$seed <- input$trainSeed
    })
    ## After training, set new seed value in training panel
    updateNumericInput(session, "trainSeed", value= sample(1e5, 1))
    res
  })
  
  ## Get observations clustering when ok.som changes
  ok.clust <- reactive({
    factor(ok.som()$unit.classif, 1:nrow(ok.som()$grid$pts))
  })
  
  ## Compute superclasses when ok.som or superclass options changes
  ok.hclust <- reactive({
    if(!is.null(ok.som())){
      hclust(dist(ok.som()$codes[[1]]), input$sup_clust_hcmethod)
    }
  })
  
  ok.pam_clust <- reactive({
    if(!is.null(ok.som())){
      cluster::pam(ok.som()$codes[[1]], input$kohSuperclass)
    }
  })
  
  
  ## Assign superclasses to cells
  ok.sc <- eventReactive(c(ok.som(), input$kohSuperclass, 
                           input$sup_clust_method, input$sup_clust_hcmethod), {
    if(is.null(ok.som())) return(NULL)

    if (input$sup_clust_method == "hierarchical") {
      superclasses <- unname(cutree(ok.hclust(), input$kohSuperclass))
      
      values$codetxt$sc <- paste0("## Group cells into superclasses (hierarchical clustering)\n", 
                                  'superclust <- hclust(dist(the.som$codes[[1]]), "', 
                                  input$sup_clust_hcmethod, '")\n',
                                  "superclasses <- unname(cutree(superclust, ", 
                                  input$kohSuperclass, "))\n")
    } else {
      superclasses <- unname(ok.pam_clust()$clustering)
      
      values$codetxt$sc <- paste0("## Group cells into superclasses (PAM clustering)\n", 
                                  "superclust <- cluster::pam(the.som$codes[[1]], ", 
                                  input$kohSuperclass, ")\n",
                                  "superclasses <- unname(superclust$clustering)\n")
    }
    
    values$codetxt$sc <- paste0(values$codetxt$sc,
                                "## Apply clusterings to observations\n",
                                "obs.class <- the.som$unit.classif\n",
                                "obs.superclass <- superclasses[obs.class]\n")
    
    superclasses
  })
  
  
  ## Current training vars
  ok.trainvars <- eventReactive(ok.traindat(), {
    colnames(ok.traindat()$dat)
  })
  
  
  ## Current training rows (no NA)
  ok.trainrows <- eventReactive(ok.trainvars(), {
    rowSums(is.na(ok.data()[input$trainVarChoice])) == 0
  })
  
  
  ## Training message
  output$Message <- renderPrint({
    if (is.null(ok.data())) return(cat("Import data to train a SOM."))
    if (!is.null(ok.traindat()$msg)) {
      cat(paste0("********** Warning: **********\n", 
                 paste("* ", ok.traindat()$msg, collapse= "\n"), 
                 "\n******************************\n\n"))
    }
    if (is.null(ok.som())) 
      return(cat("No map trained yet, click Train button."))
    
    cat("## SOM summary:\n")
    summary(ok.som())
    isolate(cat(paste0("Training options: rlen = ", input$trainRlen, 
                       " ; alpha = (", input$trainAlpha1, ", ", input$trainAlpha2, ") ; ",
                       "radius = (", input$trainRadius1, ", ", input$trainRadius2, "), ", 
                       "random seed = ", ok.som()$seed, ".\n")))

    aweSOM::somQuality(ok.som(), ok.traindat()$dat)
  })
  
 
  
  #############################################################################
  ## Panel "Plot"
  #############################################################################
  
  
  ## Download interactive plot (download widget)
  output$downloadInteractive <- downloadHandler(
    filename= paste0(Sys.Date(), "-aweSOM.html"), 
    content= function(file) {
      if (is.null(ok.som())) return(NULL)
      widg <- aweSOM::aweSOMplot(som= ok.som(), type = input$graphType,
                                 data = ok.data(), 
                                 variables = if (input$graphType %in% c("Color", "Pie", "CatBarplot")) {
                                   input$plotVarOne
                                 } else {
                                   input$plotVarMult
                                 },
                                 superclass = ok.sc(), 
                                 obsNames = if (input$plotNames != "(rownames)") {
                                   input$plotNames
                                 } else {
                                   NULL
                                 }, 
                                 scales = input$contrast, 
                                 values = input$average_format, 
                                 size = input$plotSize, palsc = input$palsc, 
                                 palvar = input$palplot, palrev = input$plotRevPal, 
                                 showAxes = input$plotAxes, 
                                 transparency = input$plotTransparency, 
                                 boxOutliers = input$plotOutliers,
                                 showSC = input$plotShowSC, 
                                 pieEqualSize = input$plotEqualSize)
      htmlwidgets::saveWidget(widg, file = file)
    }) 
  
  
  ## Update plot type choices on plot "what" selection
  observe({
    input$graphWhat
    isolate({
      if (is.null(ok.sc())) return(NULL)
      updateSelectInput(session, "graphType", choices= plotChoices[[input$graphWhat]])
    })
  })
  
  ## Update max nb superclasses
  observe({
    som <- ok.som()
    updateNumericInput(session, "kohSuperclass", max= som$grid$xdim * som$grid$ydim)
  })
  
  ## ok.plotdata : contains ok.data and the dummy-encoded training qual variables
  ok.plotdata <- eventReactive(ok.trainvars(), {
    if (all(ok.trainvars() %in% colnames(ok.data()))) return(ok.data()) 
    qualVar <- sapply(input$trainVarChoice, function(x) !is.numeric(ok.data()[x]))
    cbind(ok.data(), aweSOM::cdt(ok.data()[input$trainVarChoice[qualVar]]))
  })

  ## Update variable selection for graphs, if necessary
  observeEvent(ok.som(), {
    changeVars <- TRUE
    if (! is.null(values$previous.trainvars)) {
      if (all(c(input$plotVarMult, input$plotVarOne) %in% colnames(ok.data())))
          changeVars <- FALSE
    }
    if (changeVars) {
      updateSelectInput(session, "plotVarOne", NULL, choices= colnames(ok.data()), 
                        selected= input$trainVarChoice[1])
      tmp.numeric <- sapply(ok.plotdata(), is.numeric)
      updateSelectInput(session, "plotVarMult", NULL,
                        choices= colnames(ok.plotdata())[tmp.numeric],
                        # selected= ok.trainvars()[tmp.numeric[ok.trainvars()]])
                        selected= ok.trainvars())
      values$previous.trainvars <- ok.trainvars()
    }
  }) 
  
  ## Rearrange variables order if "Arrange" button is hit
  observeEvent(input$plotArrange, {
    vars <- input$plotVarMult
    dat <- ok.plotdata()
    if (length(vars) >= 2) {
      if (input$average_format == "mean") {
        cellValues <- do.call(rbind, lapply(split(dat[, vars], ok.som()$unit.classif), 
                                            colMeans))
      } else if (input$average_format == "median") { 
        cellValues <- do.call(rbind, lapply(split(dat[, vars], ok.som()$unit.classif), 
                                            function(x) apply(x, 2, median)))
      } else if (input$average_format == "prototypes") { 
        if (! all(vars %in% colnames(ok.som()$codes[[1]]))) return(NULL)
        cellValues <- ok.som()$codes[[1]][, vars]
      }
      
      if (input$contrast == "range") {
        for (i in vars) cellValues[, i] <- (cellValues[, i] - min(dat[, i])) / (max(dat[, i]) - min(dat[, i]))
      } else if (input$contrast == "contrast") {
        for (i in vars) cellValues[, i] <- (cellValues[, i] - min(cellValues[, i])) / (max(cellValues[, i]) - min(cellValues[, i]))
      }
      
      arrange <- kernlab::kpca(t(cellValues))@rotated[, 1]
      updateSelectInput(session, "plotVarMult", selected = vars[order(arrange)])
    }
  })
  
  ## Populate observation names selector
  output$plotNames <- renderUI({
    if (is.null(ok.data())) return(NULL)
    isolate({
      tmp.numeric <- sapply(ok.data(), is.numeric)
      fluidRow(column(4, p("Observation names:")), 
               column(8, selectInput("plotNames", NULL,
                                     choices= c("(rownames)", colnames(ok.data())),
                                     selected= "(rownames)")))
    })
  })
    
  ## Dendrogram
  output$plotDendrogram <- renderPlot({
    if (input$sup_clust_method != "hierarchical") return(NULL)
    values$codetxt$plot <- paste0("\n## Plot superclasses dendrogram\n", 
                                  "aweSOMdendrogram(superclust, ", 
                                  input$kohSuperclass, ")\n")
    aweSOM::aweSOMdendrogram(ok.hclust(), input$kohSuperclass)
    }, width = reactive({input$plotSize / 4 + 500}),
    height = reactive({input$plotSize / 4 + 500}))
  
  
  ## Scree plot
  output$plotScreeplot <-  renderPlot({
    values$codetxt$plot <- paste0("\n## Plot superclasses scree plot\n", 
                                  'aweSOMscreeplot(the.som, method = "', 
                                  input$sup_clust_method, '", ', 
                                  if (input$sup_clust_method == "hierarchical") {
                                    paste0('hmethod = "', input$sup_clust_hcmethod, '", ')
                                  },
                                  "nclass = ", input$kohSuperclass, ")\n")
    aweSOM::aweSOMscreeplot(ok.som(), input$kohSuperclass, input$sup_clust_method, input$sup_clust_hcmethod)
  },
  width = reactive({input$plotSize / 4 + 500}),
  height = reactive({input$plotSize / 4 + 500}))
  

  ## Silhouette plot
  output$plotSilhouette <- renderPlot({
    values$codetxt$plot <- paste0("\n## Plot superclasses silhouette plot\n", 
                                  "aweSOMsilhouette(the.som, superclasses)\n")
    aweSOM::aweSOMsilhouette(ok.som(), ok.sc())
  },
  width = reactive({input$plotSize / 4 + 500}),
  height = reactive({input$plotSize / 4 + 500}))
  
  
  ## Smooth distance plot
  output$plotSmoothDist <-  renderPlot({
    values$codetxt$plot <- paste0("\n## Plot smooth neighbour distances\n", 
                                  "aweSOMsmoothdist(the.som",
                                  if (input$palplot != "viridis") {
                                    paste0(', pal = "', input$palplot, '"')
                                  },
                                  if (input$plotRevPal) {
                                    ", reversePal = TRUE"
                                  },
                                  ")\n")
    aweSOM::aweSOMsmoothdist(som = ok.som(), pal = input$palplot, reversePal = input$plotRevPal)
  },
  width = reactive({(input$plotSize / 4 + 500) * 1.1}), # not the most elegant solution yet to get the plot squared but it does the job
  height = reactive({input$plotSize / 4 + 500 }))
  
  ## warning for smooth distance hex based plot
  output$smooth_dist_warning <- renderText({
    if(input$kohTopo == "hexagonal"){ 
      return("Warning: the smooth distance plot is inaccurate for hexagonal grids.") 
    }
  })
  
  
  ## Fancy JS plots through widget
  output$theWidget <- aweSOM:::renderaweSOM({
    if (is.null(input$plotNames)) return(NULL) # Prevents error due to not-yet loaded UI element, for reproducible script
    if (is.null(ok.som())) return(NULL)
    if (input$graphType %in% c("Circular", "Barplot", "Line", "Radar"))
      if (input$average_format == "prototypes") if(!any(input$plotVarMult %in% ok.trainvars())) {
        warning("Prototypes plot: none of the selected variables are training variables.")
        return(NULL)
      }
    
    ## Reproducible script for plot
    values$codetxt$plot <- paste0(
      "\n## Interactive plot\n", 
      'aweSOMplot(som = the.som, type = "', input$graphType, '", ', 
      if (! (input$graphType %in% c("Hitmap", "UMatrix"))) {
        "data = plot.data, "
      }, 
      "\n",
      if (input$graphType %in% c("Circular", "Barplot", "Boxplot", "Line", "Radar")) {
        paste0('           variables = c("', paste(input$plotVarMult, collapse= '", "'), '"),\n')
      },
      if (input$graphType %in% c("Color", "Pie", "CatBarplot")) {
        paste0('           variables = "', input$plotVarOne, '",\n')
      },
      "           superclass = superclasses, ", 
      if (input$plotNames != "(rownames)") {
        paste0("obsNames = ok.data$", input$plotNames, ", ")
      }, 
      "\n",
      if (input$graphType %in% c("Circular", "Line", "Barplot", "Boxplot", "Color", "UMatrix", "Radar") && input$contrast != "contrast") {
        paste0('           scales = "', input$contrast, '",\n')
      },
      if (input$graphType %in% c("Circular", "Line", "Barplot", "Boxplot", "Color", "UMatrix", "Radar") && input$average_format != "mean") {
        paste0('           values = "', input$average_format, '",\n')
      },
      if (input$palsc != "Set3") {
        paste0('           palsc = "', input$palsc, '", \n')
      },
      if (input$palplot != "viridis") {
        paste0('           palvar = "', input$palplot, '", \n')
      }, 
      if (input$plotRevPal) {
        paste0("           palrev = ", input$plotRevPal, ", \n")
      },
      if (input$graphType == "Boxplot" && !input$plotOutliers) {
        "           boxOutliers = FALSE,\n"
      },
      if (input$graphType %in% c("Color", "UMatrix") && !input$plotShowSC) {
        "           showSC = FALSE,\n"
      },
      if (input$graphType %in% c("Hitmap", "Circular", "Barplot", "Boxplot", "CatBarplot", "Radar") && !input$plotTransparency) {
        "           transparency = FALSE,\n"
      },
      if (input$graphType %in% c("Circular", "Line", "Barplot", "Boxplot", "CatBarplot", "Radar") && !input$plotAxes) {
        "           showAxes = FALSE,\n"
      },
      if (input$graphType == "Pie" && input$plotEqualSize) {
        paste0("           plotEqualSize = TRUE,\n") 
      },
      "           size = ", input$plotSize, ")")
    
    aweSOM:::aweSOMwidget(ok.som= ok.som(), 
                          ok.sc= ok.sc(), 
                          ok.data= ok.plotdata(), 
                          ok.trainrows= ok.trainrows(), 
                          graphType= input$graphType, 
                          plotNames= input$plotNames, 
                          plotVarMult= input$plotVarMult, 
                          plotVarOne= input$plotVarOne, 
                          plotSize= input$plotSize, 
                          plotOutliers= input$plotOutliers,
                          plotEqualSize= input$plotEqualSize,
                          plotShowSC= input$plotShowSC, 
                          contrast= input$contrast, 
                          average_format= input$average_format,
                          palsc= input$palsc, 
                          palplot= input$palplot, 
                          plotRevPal= input$plotRevPal, 
                          plotAxes= input$plotAxes,
                          plotTransparency= input$plotTransparency)
  })
  

  
  #############################################################################
  ## Panel "Clustered Data"
  #############################################################################
  
  # Update choices for rownames column
  output$clustVariables <- renderUI({
    if (is.null(ok.sc())) return()
    isolate(selectInput(inputId= "clustVariables", label= NULL, multiple= TRUE,
                        choices= c("rownames", "Superclass", "SOM.cell", colnames(ok.data())),
                        selected= c("rownames", "Superclass", "SOM.cell", colnames(ok.data())[1])))
  })
  
  # Update choices for rownames column on button clicks
  observe({  
    input$clustSelectNone
    if (is.null(ok.sc())) return()
    updateSelectInput(session, "clustVariables", selected= c("rownames", "Superclass", "SOM.cell"))
  })
  
  
  observe({
    input$clustSelectTrain
    if (is.null(ok.sc())) return()
    updateSelectInput(session, "clustVariables", 
                      selected= c("rownames", "Superclass", "SOM.cell", isolate(ok.trainvars())))
  })
  observe({
    input$clustSelectAll
    if (is.null(ok.sc())) return()
    updateSelectInput(session, "clustVariables", 
                      selected= c("rownames", "Superclass", "SOM.cell", isolate(colnames(ok.data()))))
  })
  

  # Current clustered data table
  ok.clustTable <- reactive({
    if (is.null(ok.sc()) | is.null(input$clustVariables)) return()
    res <- data.frame(isolate(ok.data()), SOM.cell= NA, Superclass= NA)
    res$rownames <- rownames(isolate(ok.data()))
    isolate({
      traindat <- ok.traindat()$dat
      res[rownames(traindat), "traindat"] <- rownames(traindat)
      res[rownames(traindat), "SOM.cell"] <- ok.clust()
      res[rownames(traindat), "Superclass"] <- ok.sc()[ok.clust()]
    })
    res[, input$clustVariables]
  })

  # Display clustered data  
  output$clustTable <- DT::renderDataTable(ok.clustTable())

  # Download clustered data
  output$clustDownload <- 
    downloadHandler(filename= paste0("aweSOM-clust-", Sys.Date(), ".csv"), 
                    content= function(con) write.csv(ok.clustTable()[, colnames(ok.clustTable()) != "rownames"], con)) 
  
  # Download som object (rds)
  output$somDownload <- 
    downloadHandler(filename= paste0("aweSOM-som-", Sys.Date(), ".rds"), 
                    content= function(con) saveRDS(ok.som(), con)) 
  
  
  ### HELP MESSAGES
  observeEvent(input$help_message_training, {
    showNotification(help_messages$train_panel, type = "message", duration = 60 ) 
  })
  
  observeEvent(input$help_message_intro_to_aweSOM, {
    showNotification(help_messages$import_data_panel, type = "message", duration = 60 ) 
  })
  
  observeEvent(input$help_contrast, {
    showNotification(help_messages$help_contrast, type = "message", duration = 60 ) 
  })
  
  observeEvent(input$help_average_format, {
    showNotification(help_messages$help_average_format, type = "message", duration = 60 ) 
  })
  
  
  
  
  #############################################################################
  ## Panel "Reproducible code"
  #############################################################################
  
  reprocode <- reactive({
    paste0("library(aweSOM) # (version 1.0.1) \n\n",
           values$codetxt$dataread, 
           values$codetxt$traindat, 
           values$codetxt$train,
           if (!is.null(ok.som())) paste0(
             "\n## Quality measures\n",
             "somQuality(the.som, train.data)\n\n",
             values$codetxt$sc, 
             values$codetxt$plot))
  })
  
  output$codeTxt <- renderText(reprocode())
  
  output$copycode <- renderUI({
    rclipboard::rclipButton("copycodebtn", "Copy to clipboard", reprocode())
  })
  
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "aweSOM-report.html",
    content = function(file) {
      if (is.null(ok.som())) return(NULL)
      # Copy the report file to a temporary directory before processing it
      tempReport <- file.path(tempdir(), "reproducible_code.Rmd")
      file.copy("reproducible_code.Rmd", tempReport, overwrite = TRUE)
      
      # Knit the document and eval it in a child of the global environment (this
      # isolates the code in the document from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = list(code = values, ok.data = ok.data()),
                        envir = new.env(parent = globalenv())
      )
    }
  )

})
