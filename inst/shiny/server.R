## 27/04/2016 : Shiny SOM
library(aweSOM)
options(shiny.maxRequestSize=2^30) # Max filesize

## TODO : remove these library calls, to be replaced by package::function inline
library(cluster)
library(prettycode)
library(tidyverse)
library(jsonify) #to get the getPlotParams object in JSON format
library(GGally) #for the legend



################################################################################
## Global Variables
################################################################################

## List of possible plots, by "what" type
plotChoices <- list(MapInfo= c("Population map"= "Hitmap",
                               # "Names"= "Names", 
                               "Superclass Dendrogram"= "Dendrogram",
                               "Superclass Scree plot"= "Screeplot",
                               "Neighbour distance"= "UMatrix", 
                               "Silhouette"= "Silhouette",
                               "Smooth distance"= "SmoothDist", 
                               "Abstraction"= "Abstraction"), 
                    Numeric= c("Radar"= "Radar", 
                               "Barplot"= "Barplot", 
                               "Boxplot"= "Boxplot",
                               "Line"= "Line", 
                                "Star"= "Star", # uncomment to active the star plot
                               "Heat"= "Color"), 
                    Categorical= c("Pie"= "Camembert", 
                                   "Barplot"= "CatBarplot"))


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
      imported_file_object <- aweSOM:::ok.data.function.csv.txt(
        input_dataFile = input$dataFile ,input_header = input$header, 
        input_sep = input$sep, input_quote = input$quote, input_dec = input$dec,
        input_encoding = input$encoding, 
        input_dataFile_datapath = input$dataFile$datapath)
    } else if(input$file_type == "excel_xlsx"){
      imported_file <-  aweSOM:::ok.data.function.excel_xlsx(
        input_dataFile = input$dataFile, input_column_names = input$column_names, 
        input_trim_spaces = input$trim_spaces, 
        input_range_specified_bol = input$range_specified_bol, 
        input_range_specs = input$range_specs,
        input_worksheet_specified_bol = input$worksheet_specified_bol,
        input_worksheet_specs = input$worksheet_specs, 
        input_dataFile_datapath = input$dataFile$datapath,
        input_rows_to_skip = input$rows_to_skip)
    } else if(input$file_type == "excel_xls"){
      imported_file_object <- aweSOM:::ok.data.function.excel_xls(
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
      imported_file_object <- aweSOM:::ok.data.function.spss(
        input_dataFile = input$dataFile, 
        input_dataFile_datapath = input$dataFile$datapath)
    } else if(input$file_type == "stata"){
      imported_file_object <- aweSOM:::ok.data.function.stata(
        input_dataFile = input$dataFile, 
        input_dataFile_datapath = input$dataFile$datapath)
    } else if(input$file_type == "sas_data"){
      imported_file_object <- aweSOM:::ok.data.function.sas.data(
        input_dataFile = input$dataFile, 
        input_dataFile_datapath = input$dataFile$datapath)
    }
    
    values$codetxt$dataread <- imported_file_object[[2]]
    imported_file_object[[1]]
  })
  
  
  # data preview table
  output$dataView <- DT::renderDataTable({
     d.input <- ok.data()
    if (is.null(d.input)){
      NULL
      
    }
      data.frame(rownames= rownames(d.input), d.input)
  })
  
  
  

  # data import message
  output$dataImportMessage <- renderUI({
    if (is.null(input$dataFile)) 
      return(h4("Data preview should appear here after import."))
    if (! is.null(input$dataFile) & is.null(ok.data())) 
      return(h4("Error during import: try different import parameters, and check that file is a text or csv table."))
  })
  
  
  
  
  
  ### check placement in server.R for this element
  output$downloadData_interactive <- downloadHandler(
       filename = function() {
         paste0("interactive_download", gsub(pattern = " ", replacement = "_", Sys.time()),  ".html")
       },
       content = 
         function(file) {
           file.copy("export/awesom_exported_version.html", file)
       }
     )
  
  
  
  
  
  
  

  #############################################################################
  ## Panel "Train"
  #############################################################################
  
  # Update train variable options on data change
  output$trainVarOptions <-renderUI({
    if (is.null(ok.data())) return()
    varclass <- sapply(ok.data(), class)
    names(varclass) <- colnames(ok.data())
    isnum <- varclass %in% c("integer", "numeric")
    names(isnum) <- names(varclass) <- colnames(ok.data())
    
    lapply(colnames(ok.data()), function(var) {
      fluidRow(column(2, numericInput(paste0("trainVarWeight", var), NULL, value= 1, min= 0, max= 1e3)), 
               column(8, checkboxInput(paste0("trainVarChoice", var), var, unname(isnum[var]))),  
               column(2, p(varclass[var])))
    })
  })
  
  
  # Update train variable choice on button click
  observe({
    
    input$varNum
    if (is.null(ok.data())) return()
    selectVars <- sapply(ok.data(), class) %in% c("integer", "numeric")
    names(selectVars) <- colnames(ok.data())
    lapply(colnames(ok.data()), function(var) {
      updateCheckboxInput(session, paste0("trainVarChoice", var), value= unname(selectVars[var]))
    })
  })
  observe({
    input$varAll
    if (is.null(ok.data())) return()
    lapply(colnames(ok.data()), function(var) {
      updateCheckboxInput(session, paste0("trainVarChoice", var), value= T)
    })
  })
  observe({
    input$varNone
    if (is.null(ok.data())) return()
    euss <- rep(F, ncol(ok.data()))
    names(euss) <- colnames(ok.data())
    lapply(colnames(ok.data()), function(var) {
      updateCheckboxInput(session, paste0("trainVarChoice", var), value= unname(euss[var]))
    })
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
    tmpgrid <- class::somgrid(input$kohDimx, input$kohDimy, input$kohTopo)
    tmpgrid$n.hood <- ifelse(input$kohTopo == "hexagonal", "circular", "square")
    radius <- round(unname(quantile(kohonen::unit.distances(tmpgrid, FALSE), .67)), 2)
    updateNumericInput(session, "trainRadius1", value= radius)
    updateNumericInput(session, "trainRadius2", value= -radius)
  })
  
  
 

  ok.traindat <- reactive({
    if (input$trainbutton == 0) return(NULL)
    isolate({
      #to make externalized functions more useable this part is placed outside of the (Jan)
      varSelected <- as.logical(sapply(paste0("trainVarChoice", colnames(ok.data())), 
                                       function(var) input[[var]]))
      varWeights <- sapply(paste0("trainVarWeight", colnames(ok.data())), 
                           function(var) input[[var]])
      
      varSelected <- varSelected & varWeights > 0

      if (sum(varSelected) < 2) 
        return(list(dat= NULL, msg= "Select at least two variables (with non-zero weight)."))
      
      return_traindat <- aweSOM:::ok.traindat.function(input_trainscale = input$trainscale, 
                                              ok.data = ok.data(), 
                                              varSelected = varSelected,
                                              varWeights = varWeights)
      values$codetxt_traindat <- return_traindat$codeTxt
      return_traindat
    })
  })
  
  
  ## Train SOM when button is hit (triggered by change in ok.traindat)

  ok.som <- reactive({
    dat <- ok.traindat()
    if (is.null(dat$dat)) {
      return(NULL)
    }
    
    res <- aweSOM:::ok.som.function(ok.traindat = dat, 
                           input_trainSeed = input$trainSeed, 
                           input_kohInit = input$kohInit,
                           input_kohDimy = input$kohDimy, 
                           input_kohDimx = input$kohDimx, 
                           input_kohTopo = input$kohTopo, 
                           input_trainRlen = input$trainRlen,
                           input_trainAlpha1 = input$trainAlpha1, 
                           input_trainAlpha2 = input$trainAlpha2, 
                           input_trainRadius1 = input$trainRadius1, 
                           input_trainRadius2 = input$trainRadius2)
    values$codetxt$train <- res$codeTxt
    
    ## After training, set new seed value in training panel
    updateNumericInput(session, "trainSeed", value= sample(1e5, 1))
    res
  })
  
  ## Get clustering when ok.som changes
  ok.clust <- reactive({
    factor(ok.som()$unit.classif, 1:nrow(ok.som()$grid$pts))
  })
  
  
  
  ## Compute superclasses when ok.som or superclass options changes
  ok.hclust <- reactive({
    if(!is.null(ok.som())){
      hclust(dist(ok.som()$codes[[1]]), "ward.D2")
    }
  })
  
  ok.pam_clust <- reactive({
    if(!is.null(ok.som())){
      cluster::pam(ok.som()$codes[[1]], input$kohSuperclass)
    }
  })
  
  
  ## Assign superclasses to cells
  ok.sc <- reactive({

    if(is.null(ok.som())) return(NULL)
    
    if (input$sup_clust_method == "hierarchical") {
      superclasses <- unname(cutree(ok.hclust(), input$kohSuperclass))
      
      values$codetxt$sc <- paste0("## Group cells into superclasses (hierarchical clustering)\n", 
                                  "superclust <- hclust(dist(ok.som$codes[[1]]), 'ward.D2')\n",
                                  "superclasses <- unname(cutree(superclust, ", 
                                  input$kohSuperclass, "))\n")
    } else {
      superclasses <- unname(ok.pam_clust()$clustering)
      
      values$codetxt$sc <- paste0("## Group cells into superclasses (PAM clustering)\n", 
                                  "superclust <- cluster::pam(ok.som$codes[[1]], ", 
                                  input$kohSuperclass, ")\n",
                                  "superclasses <- unname(superclust$clustering)\n")
    }
    
    superclasses
  })
  
  
  ## Current training vars
  ok.trainvars <- reactive({
    if (is.null(ok.som())) return(NULL)
    isolate(colnames(ok.traindat()$dat))
  })
  
  
  ## Current training rows (no NA)
  ok.trainrows <- reactive({
    if (is.null(ok.som())) return(NULL)
    isolate(rowSums(is.na(ok.data()[, ok.trainvars()])) == 0)
  })
  
  
  ## Current map distances and quality measures, triggered by ok.som
  ok.dist <- reactive({
    somDist(ok.som = ok.som())
  })
  ok.qual <- reactive({
    somQuality(ok.som = ok.som(), traindat = ok.traindat()$dat)
  })
  
  ## Training message
  output$Message <- renderPrint({
    if (!is.null(ok.traindat()$msg)) {
      cat(paste0("********** Warning: **********\n", 
                 paste("* ", ok.traindat()$msg, collapse= "\n"), 
                 "\n******************************\n\n"))
    }
    if (is.null(ok.qual())) 
      return(cat("No map trained yet, click Train button."))
    
    cat("## SOM summary:\n")
    summary(ok.som())
    isolate(cat(paste0("Training options: rlen = ", input$trainRlen, 
                       " ; alpha = (", input$trainAlpha1, ", ", input$trainAlpha2, ") ; ",
                       "radius = (", input$trainRadius1, ", ", input$trainRadius2, "), ", 
                       "random seed = ", ok.som()$seed, ".")))
    
    cat("\n\n## Quality measures:\n")
    cat("* Quantization error     : ", ok.qual()$err.quant, "\n")
    cat("* (% explained variance) : ", ok.qual()$err.varratio, "\n")
    cat("* Topographic error      : ", ok.qual()$err.topo, "\n")
    cat("* Kaski-Lagus error      : ", ok.qual()$err.kaski, "\n")
    cat("\n## Number of obs. per map cell:")
    table(factor(ok.som()$unit.classif, 
                 levels= 1:nrow(ok.som()$grid$pts)))
  })
  
 
  
  
  
  
  

  
  
  #############################################################################
  ## Panel "Graph"
  #############################################################################
  
  
  
  
  #############################################################################
  # WORK TBD
  #############################################################################
  ## Update plot type choices on plot "what" selection
  #try using it without shiny
  observe({
    input$graphWhat
    isolate({
      if (is.null(ok.sc())) return(NULL)
      updateSelectInput(session, "graphType", choices= plotChoices[[input$graphWhat]])
    })
  })
  
  ## Update max nb superclasses
 
  
  #try using it without shiny
   observe({
     som <- ok.som()
     updateNumericInput(session, "kohSuperclass", max= som$grid$xdim * som$grid$ydim)
   })
  
  
  

  
  

  ## Update variable selection for graphs
  output$plotVarOne <- renderUI({
    if (is.null(ok.som())) return(NULL)
    isolate({
      fluidRow(column(4, p("Plot variable:")), 
               column(8, selectInput("plotVarOne", NULL, choices= colnames(ok.data()), 
                                     selected= ok.trainvars()[1])))
    })
    
  })
  
  
  
  
  
  
  
  output$plotVarMult <- renderUI({
    if (is.null(ok.som())) return(NULL)
    isolate({
      tmp.numeric <- sapply(ok.data(), is.numeric)
      fluidRow(column(4, p("Plot variables:")), 
               column(8, selectInput("plotVarMult", NULL, multiple= T,
                                     choices= colnames(ok.data())[tmp.numeric],
                                     selected= ok.trainvars()[tmp.numeric[ok.trainvars()]])))

    })
  })
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
    
  
  #Code function important for dendogram and further plots
  ## Dendrogram
  output$plotDendrogram <- renderPlot({
    values$codetxt$plot <- paste0("\n## Plot superclasses dendrogram\n", 
                                  "aweSOM::aweSOMdendrogram(ok.som, superclust, ", 
                                  input$kohSuperclass, ")\n")
    aweSOMdendrogram(ok.som(), ok.hclust(), input_kohSuperclass = input$kohSuperclass)
    }, width = reactive({input$plotSize + 500}),
  height = reactive({input$plotSize + 500}))
  
  
  ## Scree plot
  output$plotScreeplot <-  renderPlot({
    values$codetxt$plot <- paste0("\n## Plot superclasses scree plot\n", 
                                  "aweSOM::aweSOMscreeplot(ok.som, superclust, ", 
                                  input$kohSuperclass, ")\n")
    aweSOMscreeplot(ok.som(), ok.hclust(), input_kohSuperclass = input$kohSuperclass)
  },
  width = reactive({input$plotSize + 500}),
  height = reactive({input$plotSize + 500}))
  
  ## Smooth distance plot
  output$plotSmoothDist <-  renderPlot({
    values$codetxt$plot <- paste0("\n## Plot smooth distances\n", 
                                  "aweSOM::aweSOMsmoothdist(ok.som, superclust, ", 
                                  input$kohSuperclass, ")\n")
    aweSOMsmoothdist(ok.som = ok.som(), ok.dist = ok.dist(), input_palplot = input$palplot, 
                    input_plotRevPal = input$plotRevPal)
    
    },
  width = reactive({(input$plotSize + 500) * 1.1}), # not the most elegant solution yet to get the plot squared but it does the job
  height = reactive({input$plotSize + 500 }))

  ## Abstraction plot
  output$plotAbstraction <-renderPlot({
    plot.abstraction(ok.som = ok.som(), ok.traindat = ok.traindat(),
                     input_plotAbstrCutoff = input$plotAbstrCutoff,
                     input_palplot = input$palplot,
                     input_plotRevPal = input$plotRevPal)
  },
  width = reactive({input$plotSize + 500}),
  height = reactive({input$plotSize + 500}))
  
  
  
  ## R-Based legend
  
  output$theLegend <- renderPlot({
    
    
    if (is.null(ok.som()) | !(input$graphType %in% c("Radar",
                                                             "Camembert", "CatBarplot",
                                                             "Barplot", "Boxplot",
                                                             "Color", "Star",
                                                            "Line",
                                                             "Names", "UMatrix")))
      return(NULL) # si on n'a pas calculé, on donne NULL à JS

    plot.data <- isolate(ok.data()[ok.trainrows(), ])
    if(is.null(plot.data)) return(NULL)
    
    # Obs names per cell for message box
    if (is.null(input$plotNames)){
      return(NULL)
    }

    aweSOM:::the.legend.function(plot.data = plot.data, 
                                 input_plotNames = input$plotNames, ok.clust = ok.clust(), 
                                 input_graphType = input$graphType, input_plotVarMult = input$plotVarMult, 
                                 input_plotVarOne = input$plotVarOne,
                                 ok.som = ok.som(), input_plotEqualSize = input$plotEqualSize, 
                                 input_contrast = input$contrast, input_average_format = input$average_format, 
                                 ok.sc = ok.sc(),
                                 input_plotSize = input$plotSize, input_palsc = input$palsc, input_palplot = input$palplot,
                                 input_plotOutliers = input$plotOutliers, input_plotRevPal = input$plotRevPal)
  },
  width = reactive({input$plotSize + 900}))
  
  
  
  # ## Fancy JS Plots
  # output$thePlot <- reactive({
  #   if (is.null(ok.som()) | !(input$graphType %in% c("Radar", 
  #                                                    "Camembert", "CatBarplot",
  #                                                    "Barplot", "Boxplot", 
  #                                                    "Color", "Star", 
  #                                                    "Hitmap", "Line", 
  #                                                    "Names", "UMatrix")))
  #     return(NULL) # si on n'a pas calculé, on donne NULL à JS
  # 
  #   plot.data <- isolate(ok.data()[ok.trainrows(), ])
  #   if(is.null(plot.data)) return(NULL)
  #   # Obs names per cell for message box
  #   if (is.null(input$plotNames)){
  #     return(NULL)
  #     
  #   } 
  #   if (input$plotNames == "(rownames)") {
  #     plotNames.var <- rownames(plot.data)
  #   } 
  #   else {
  #     plotNames.var <- as.character(plot.data[, input$plotNames])
  #   }
  #   
  #   cellNames <- unname(lapply(split(plotNames.var, ok.clust()), 
  #                       function(x) paste(sort(x), collapse= ", "))) # "&#13;&#10;" "<br />"
  #   
  #   if (input$graphType %in% c("Radar", "Star", "Barplot", "Boxplot", "Line")) {
  #     if (is.null(input$plotVarMult)) return(NULL)
  #     plotVar <- input$plotVarMult
  #     data <- plot.data[, plotVar]
  #   } else if (input$graphType %in% c("Color", "Camembert", "CatBarplot")) {
  #     if (is.null(input$plotVarOne)) return(NULL)
  #     plotVar <- input$plotVarOne
  #     data <- plot.data[, plotVar]
  #   } else if (input$graphType %in% c("Hitmap")) {
  #     plotVar <- NULL
  #     data <- NULL
  #   } else if (input$graphType %in% c("Names")) {
  #     plotVar <- NULL
  #     data <- as.character(plot.data[, input$plotVarOne])
  #   } else if (input$graphType == "UMatrix") {
  #     plotVar <- NULL
  #     proto.gridspace.dist <- as.matrix(dist(ok.som()$grid$pts))
  #     proto.dataspace.dist <- as.matrix(dist(ok.som()$codes[[1]]))
  #     proto.dataspace.dist[round(proto.gridspace.dist, 3) > 1] <- NA
  #     proto.dataspace.dist[proto.gridspace.dist == 0] <- NA
  #     data <- rowMeans(proto.dataspace.dist, na.rm= T)[ok.clust()]
  #     plotVar <- "Mean distance to neighbours"
  #   }
  #   
  #   options <- list(equalSize= input$plotEqualSize)
  #   
  # 
  #   if(input$contrast == "contrast")  contrast <- "contrast"
  #   else if(input$contrast == "range") contrast <- "range"
  #   else if(input$contrast == "no_contrast") contrast <- "no_contrast" 
  #   
  #   the.average_format <- switch(input$average_format, "mean"= "mean", 
  #                                "median"= "median", "prototypes"= "prototypes")
  #   
  #   graphType <- ifelse(input$graphType == "UMatrix", "Color", input$graphType)
  #   
  #   values$codetxt$plot <- paste0("\n## Plot interactive ** graph \n", 
  #                                 "# aweSOM::aweSOMplot(the, arguments)")
  # 
  #   aweSOM:::getPlotParams(graphType, ok.som(), ok.sc(),  
  #                          data, input$plotSize, plotVar, contrast,
  #                          input$palsc, input$palplot, cellNames,
  #                          input$plotOutliers, input$plotRevPal, options, 
  #                          the.average_format)
  # })
  
  
  ## Fancy JS plots through widget
  output$theWidget <- aweSOM:::renderaweSOM({
    values$codetxt$plot <- paste0(
      "\n## Interactive plot\n", 
      "aweSOM::aweSOMplot(ok.som = ok.som, ok.sc = superclasses, ok.data = ok.data,\n", 
      "                   ok.trainrows = c(", paste(ok.trainrows(), collapse= ", "), "),\n", 
      "                   graphType = '", input$graphType, 
      "', plotNames = '", input$plotNames, "',\n",
      "                   plotVarMult = c('", paste(input$plotVarMult, collapse= "', '"), 
      "'), plotVarOne = '", input$plotVarOne, "',\n",
      "                   plotEqualSize = ", input$plotEqualSize, 
      ", contrast = '", input$contrast, "',\n",
      "                   average_format = '", input$average_format,
      "', plotSize = ", input$plotSize, ",\n",
      "                   palsc = '", input$palsc, 
      "', palplot = '", input$palplot, "', plotRevPal = ", input$plotRevPal, ")\n")
    
    aweSOM:::aweSOMwidget(ok.som= ok.som(), 
                          ok.sc= ok.sc(), 
                          ok.data= ok.data(), 
                          ok.trainrows= ok.trainrows(), 
                          graphType= input$graphType, 
                          plotNames= input$plotNames, 
                          plotVarMult= input$plotVarMult, 
                          plotVarOne= input$plotVarOne, 
                          plotOutliers= input$plotOutliers,
                          plotEqualSize= input$plotEqualSize,
                          contrast= input$contrast, 
                          average_format= input$average_format,
                          plotSize= input$plotSize, 
                          palsc= input$palsc, 
                          palplot= input$palplot, 
                          plotRevPal= input$plotRevPal)
  })
  
  
  ## warning for smooth distance hex based plot
  output$smooth_dist_warning <- renderPrint({
    if(input$kohTopo == "hexagonal"){ 
      
      print("This might be a biased version since the topology of a hexagonal grid cannnot be account
          for within this plot") #<-- prints also to the console which is rather stupid
      }
  })
  
  
  
  ## Visualize Silhouette Information from Clustering
  output$pam_silhouette <- renderPlot({
    plot.pam_silhouette(ok.som = ok.som(), ok.pam_clust = ok.pam_clust(), 
                        input_sup_clust_method = input$sup_clust_method)
  })
  

  # map cluster function -------------------------------------------------------------
  
  
  #############################################################################
  ## Panel "Clustered Data"
  #############################################################################
  
  # Update choices for rownames column
  output$clustVariables <- renderUI({
    if (is.null(ok.sc())) return()
    isolate(selectInput(inputId= "clustVariables", label= NULL, multiple= T,
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
  observeEvent(input$help_message_1, {
    showNotification("This provides a help message \n
                         It will pop-up and explain in different parts of the shiny app aweSOM 
                         what some of the settings imply and what is behind the overall magic. 
                         Actually, I only did dummy text right until here.", type = "message",
                     duration = 60 ) #<- closes after 60sek if not closed by user
  })
  
  
  observeEvent(input$help_message_2, {
    showNotification("This provides a help message specifically appearing at the 'enhanced constrast' setting.
                         It will pop-up and explain in different parts of the shiny app aweSOM 
                         what some of the settings imply and what is behind the overall magic. 
                         Actually, I only did dummy text right until here.", type = "message",
                     duration = 60 ) #<- closes after 60sek if not closed by user
  })
  
  
  
  
  #############################################################################
  ## Panel "Reproducible code"
  #############################################################################
  
  output$codeTxt <- renderText({
    paste0("\n## Import Data\n", 
           values$codetxt$dataread, 
           "\n## Build training data\n",
           values$codetxt_traindat$traindat, 
           "\n## Train SOM\n", 
           values$codetxt$train, "\n",
           values$codetxt$sc, 
           if (!is.null(ok.som())) paste0(
             "\n## Quality measures:\n",
             "ok.qual <- aweSOM::somQuality(ok.som, dat)\n",
             'cat("* Quantization error     : ", ok.qual$err.quant, "\\n",\n',
             '    "* (% explained variance) : ", ok.qual$err.varratio, "\\n",\n',
             '    "* Topographic error      : ", ok.qual$err.topo, "\\n",\n',
             '    "* Kaski-Lagus error      : ", ok.qual$err.kaski, "\\n")\n',
             '## Number of obs. per map cell:\n',
             'table(factor(ok.som$unit.classif, levels= 1:nrow(ok.som$grid$pts)))\n',
             values$codetxt$plot))
  })
  
  ## Trying to make the code more pretty with RMarkdown, failed (works, but not more pretty and messes with the css)
  # output$codeTxt2 <- shiny::renderUI({
  #   tmp <- paste0("\n## Import Data\n", 
  #          values$codetxt$dataread, 
  #          "\n## Build training data\n",
  #          values$codetxt_traindat$traindat, 
  #          "\n## Train SOM\n", 
  #          values$codetxt$train, "\n",
  #          values$codetxt$sc, 
  #          if (!is.null(ok.som())) paste0(
  #            "\n## Quality measures:\n",
  #            "ok.qual <- aweSOM::somQuality(ok.som, dat)\n",
  #            'cat("* Quantization error     : ", ok.qual$err.quant, "\\n",\n',
  #            '    "* (% explained variance) : ", ok.qual$err.varratio, "\\n",\n',
  #            '    "* Topographic error      : ", ok.qual$err.topo, "\\n",\n',
  #            '    "* Kaski-Lagus error      : ", ok.qual$err.kaski, "\\n")\n',
  #            '## Number of obs. per map cell:\n',
  #            'table(factor(ok.som$unit.classif, levels= 1:nrow(ok.som$grid$pts)))\n',
  #            values$codetxt$plot))
  #   HTML(markdown::markdownToHTML(text= knitr::knit(text= paste0("---\n", 
  #                                                           "title: 'aweSOMcode'\n", 
  #                                                           "output: hmtl_document\n", 
  #                                                           "---\n", 
  #                                                           "```{r eval=FALSE}\n", 
  #                                                           tmp, "\n```\n"), 
  #                                              quiet= T)))
  # })
  
})
  

