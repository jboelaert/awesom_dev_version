## 27/04/2016 : Shiny User Interface
# library(RColorBrewer)

## Organisation de la page
shinyUI(fluidPage(
  headerPanel(HTML("aweSOM")),
  
  tabsetPanel(
    #### Panel 'Welcome, Import Data'
    #########################################################################
    tabPanel("Import Data", 
             # includeHTML("js/hexbin.js"),
             # includeHTML("js/lodash.min.js"), 
             # includeHTML("js/d3.min.js"),
             includeHTML("js/svg_todataurl.js"),
             # includeHTML("js/box.js"),
             includeHTML("js/canvg.js"),
             includeHTML("js/rgbcolor.js"),
             # includeHTML("js/radar-chart-d3.js"),
             includeHTML("js/word-cloud.js"),
             
             
             # data_import -------------------------------------------------------------
             
             h2("Import your data"), 
             fluidRow(
               column(4,
                      wellPanel(
                        
                        selectInput('file_type', NULL, 
                                    choices =  c(
                                      ".csv or .txt"  = "csv_txt",
                                      "Microsoft Excel .xlsx"  = "excel_xlsx",
                                      "Microsoft Excel .xls" = "excel_xls",
                                      "SPSS FILE" = "spss",
                                      "SAS Data" = "sas_data",
                                      "Stata" = "stata"),
                                    selected = "csv_txt"
                                ),
                        
                        
                        fileInput('dataFile', 'Choose File'),
                        HTML("<b>Import parameters</b>"),
                        
                        
                        
                        conditionalPanel("input.file_type == 'csv_txt'",
                        
                        fluidRow(column(4, p('Header:')), 
                                 column(8, selectInput('header', NULL,
                                                       c("Auto", "Header",
                                                         "No Header"), 
                                                       "Auto"))),
                        fluidRow(column(4, p('Separator:')), 
                                 column(8, selectInput('sep', NULL,
                                                       c("Auto", "Comma ','",
                                                         "Semicolon ';'",
                                                         "Tab","Space"), 
                                                       "Auto"))),
                        fluidRow(column(4, p('Quote:')), 
                                 column(8, selectInput('quote', NULL,
                                                       c("None","Double Quote \"","Single Quote '"), 
                                                       'Double Quote "'))),
                        fluidRow(column(4, p('Decimal mark')), 
                                 column(8, selectInput('dec', NULL, 
                                                       c('Period "."', 'Comma ","'),
                                                       'Period "."'))),
                        fluidRow(column(4, p('File Encoding')), 
                                 column(8, selectInput('encoding', NULL, 
                                                       c("unknown", "UTF-8", "whatever else"),
                                                       "unknown")))),
                        
                        
                        
                        
                        
                        conditionalPanel("input.file_type == 'excel_xlsx'",
                                         
                                         
                                         fluidRow(column(4, p('Column names')), 
                                                  column(8, selectInput('column_names', NULL, 
                                                                        choices = c("Yes" = "TRUE",
                                                                                    "No" = "FALSE"),
                                                                        selected = TRUE ))),
                                         
                                         fluidRow(column(4, p('Skip rows')), 
                                                  column(8, numericInput('rows_to_skip', NULL, 0, min= 0))),
                                         
                                         
                                         fluidRow(column(4, p('Trim whitespaces')), 
                                                  column(8, selectInput('trim_spaces', NULL, 
                                                                        choices = c("Yes" = "TRUE",
                                                                                    "No" = "FALSE"),
                                                                        selected = TRUE ))),
                                         
                                         fluidRow(column(4, checkboxInput("worksheet_specified_bol", "Specify Worksheet", F))),
                                         conditionalPanel("input.worksheet_specified_bol",
                                                          textInput("worksheet_specs", NULL, "")
                                                          ),
                                         
                                         
                                         fluidRow(column(4, checkboxInput("range_specified_bol", "Specify Range", F))),
                                         conditionalPanel("input.range_specified_bol",
                                                          textInput("range_specs", NULL, "")
                                         )),
                        
                        
                        
                        #for the xls excel files add
                        conditionalPanel("input.file_type == 'excel_xls'",
                                         
                                         
                                         fluidRow(column(4, p('Column names')), 
                                                  column(8, selectInput('column_names_xls', NULL, 
                                                                        choices = c("Yes" = "TRUE",
                                                                                    "No" = "FALSE"),
                                                                        selected = TRUE ))),
                                         
                                         fluidRow(column(4, p('Skip rows')), 
                                                  column(8, numericInput('rows_to_skip_xls', NULL, 0, min= 0))),
                                         
                                         
                                         fluidRow(column(4, p('Trim whitespaces')), 
                                                  column(8, selectInput('trim_spaces_xls', NULL, 
                                                                        choices = c("Yes" = "TRUE",
                                                                                    "No" = "FALSE"),
                                                                        selected = TRUE ))),
                                         
                                         fluidRow(column(4, checkboxInput("worksheet_specified_bol_xls", "Specify Worksheet", F))),
                                         conditionalPanel("input.worksheet_specified_bol",
                                                          textInput("worksheet_specs_xls", NULL, "")
                                         ),
                                         
                                         
                                         fluidRow(column(4, checkboxInput("range_specified_bol_xls", "Specify Range", F))),
                                         conditionalPanel("input.range_specified_bol",
                                                          textInput("range_specs_xls", NULL, "")
                                         )),
                        
                        
                        
                        
                        
                        
                        
                        
                        conditionalPanel("input.file_type == 'spss'",
                                         ###specs and params here
                                         #encoding, user_na, skip, n_max
                                         fluidRow(column(4, p('Skip rows')), 
                                                  column(8, numericInput('skip_spss', NULL, 0, min= 0))),
                                         fluidRow(column(4, p('File Encoding')), 
                                                  column(8, selectInput('encoding_spss', NULL, 
                                                                        c("specified_in_file", "UTF-8"),
                                                                        "specified_in_file")))),
                                         
                                         
                                         
                                         
                       
                        
                        
                        conditionalPanel("input.file_type == 'sas_data'",
                                         ###specs and params here
                                         
                        ),
                        
                        
                        
                        conditionalPanel("input.file_type == 'stata'",
                                         ###specs and params here
                                         
                        ),
                        )),
               column(8, 
                      uiOutput("dataImportMessage"), 
                      DT::dataTableOutput("dataView"))
               
              
               )),
    
    
    
    
    
    
    
    tabPanel("Train", 
             wellPanel(fluidRow(column(2, h3("Map info:")),
                                column(10, verbatimTextOutput("Message")))),


             # map training -------------------------------------------------------------
             
             fluidRow(column(4, wellPanel( 
                 h3("Train new map:"), 
                 actionButton("trainbutton", "Train SOM"),
                 # h3("Train new map:"),
                 # actionButton("trainbutton", "Train Self-organizing Map"), 
                 # fluidRow(column(4, p("Map size (rows,columns)")), 
                 #          column(4, numericInput('kohDimx', NULL, 4, min= 1)), 
                 #          column(4, numericInput('kohDimy', NULL, 4, min= 1))),
                 # fluidRow(column(4, p("Topology")), 
                 #          column(8, selectInput('kohTopo', NULL, 
                 #                                c("hexagonal", "rectangular")))),
                 # fluidRow(column(3, p("Map layout (rows, columns, topology)")), 
                 #          column(3, numericInput('kohDimx', NULL, 4, min= 1)), 
                 #          column(3, numericInput('kohDimy', NULL, 4, min= 1)), 
                 #          column(3, selectInput('kohTopo', NULL, 
                 #                                c("hexagonal", "rectangular")))),
                 fluidRow(column(4, numericInput('kohDimx', "Rows", 4, min= 1)),
                          column(4, numericInput('kohDimy', "Cols", 4, min= 1)),
                          column(4, selectInput('kohTopo', "Topology",
                                                c("hexagonal", "rectangular")))),

                 checkboxInput("trainscale", "Scale training data", T), 
                 
                 
            
                 
                 fluidRow(column(4, checkboxInput("trainAdvanced", "Advanced options", F)),
                          column(4, actionButton("help_message_1", "", icon = icon("question"), width = NULL))),


                 conditionalPanel("input.trainAdvanced", 
                                  fluidRow(column(4, p("Initialization")), 
                                           column(8, selectInput("kohInit", NULL, 
                                                                 c("PCA"= "pca", "PCA Obs"= "pca.sample", 
                                                                   "Random Obs"= "random")))),
                                  fluidRow(column(4, p("rlen")), 
                                           column(8, numericInput("trainRlen", NULL, 100, 1, 1e6))),
                                  fluidRow(column(4, p("Alpha (start,stop)")), 
                                           column(4, numericInput("trainAlpha1", NULL, .05, 1e-6, 1e3)), 
                                           column(4, numericInput("trainAlpha2", NULL, .01, 1e-6, 1e3))),
                                  fluidRow(column(4, p("Radius (start,stop)")), 
                                           column(4, numericInput("trainRadius1", NULL, .05, 1e-6, 1e3)), 
                                           column(4, numericInput("trainRadius2", NULL, .05, 1e-6, 1e3))), 
                                  fluidRow(column(4, p("Random seed:")), 
                                           column(8, numericInput("trainSeed", NULL, sample(1e5, 1), 1, 1e9)))))), 
                 
                 # wellPanel(
                 #   h3("Import SOM:"),
                 #   fileInput("importmapbutton", "Import (RDS)"))),
               column(8, 
                      h3("Training variables:"),

                      # <- respective observers are in place at server.R
                      fluidRow(column(4, actionButton("varNum", "Select numeric variables")), 
                               column(4, actionButton("varAll", "Select all variables")), 
                               column(4, actionButton("varNone", "Unselect all variables"))),
                      p(),
                      fluidRow(column(2, HTML("<p><b>Weight</b></p>")), 
                               column(8, HTML("<p><b>Variable</b></p>")), 
                               column(2, HTML("<p><b>Type</b></p>"))),
                      uiOutput("trainVarOptions"),
                      
                      
                      ))), #<- where does this belong to, how does the update with the buttons above happen
    
    
    
    
    # map visuals -------------------------------------------------------------
    
    tabPanel("Graph", 
             fluidRow(column(4, 
                             ## Sélection du graphique et des variables
                             h4("Plot options:"),
                             fluidRow(column(6, selectInput("graphWhat", NULL, 
                                                            choices= c("General Information"= "MapInfo",
                                                                       "Numeric Variables"= "Numeric",
                                                                       "Categorical Variables"= "Categorical"), 
                                                            selected= "MapInfo")),
                                      column(6, selectInput("graphType", NULL, 
                                                            choices= c("Population map"= "Hitmap",
                                                                       "Superclass Dendrogram"= "Dendrogram",
                                                                       "Superclass Scree plot"= "Screeplot",
                                                                       "Silhouette"= "Silhouette",
                                                                       "Neighbour distance"= "UMatrix", 
                                                                       "Smooth distance"= "SmoothDist", 
                                                                       "Abstraction"= "Abstraction"
                                                            ), 
                                                            selected= "Hitmap"))),
                             
                             #question about that panel below
                             conditionalPanel('input.graphType == "Camembert" | input.graphType == "CatBarplot" | input.graphType == "Color" | input.graphType == "Names"', 
                                              uiOutput("plotVarOne")),
                             conditionalPanel(paste0('input.graphType == "Radar" | ', 
                                                     'input.graphType == "Line" | ', 
                                                     'input.graphType == "Barplot" | ', 
                                                     'input.graphType == "Boxplot" | ', 
                                                     'input.graphType == "Star"'), 
                                              uiOutput("plotVarMult")),
                             conditionalPanel('input.graphType != "Dendrogram" & input.graphType != "Screeplot" & input.graphType != "SmoothDist" & input.graphType != "Abstraction"',
                                              uiOutput("plotNames")),
                             checkboxInput("plotAdvanced", "Advanced options", F),
                             conditionalPanel("input.plotAdvanced", 
                                              h4("Advanced options"),
                                              conditionalPanel(paste0('input.graphType == "CatBarplot" | ', 
                                                                      'input.graphType == "Radar" | ', 
                                                                      'input.graphType == "Line" | ', 
                                                                      'input.graphType == "Barplot" | ', 
                                                                      'input.graphType == "Color" | ', 
                                                                      'input.graphType == "UMatrix" | ', 
                                                                      'input.graphType == "Star"'), 
                                                               
                                                               
                                              fluidRow(#column(4, checkboxInput("contrast", "Enhanced contrast", value= T)),
                                                       column(4, selectInput("contrast", NULL,
                                                                             choices = c("contrast" = "contrast",
                                                                                         "range" = "range",
                                                                                         "no_contrast" = "no_contrast"),
                                                                             selected = "contrast")),
                                                       
                                                       
                                                       column(4, actionButton("help_message_2", "", icon = icon("question"), width = NULL)))),     
                                              
                                              
                                              fluidRow(column(8, selectInput("average_format", NULL, 
                                                                             choices = c("mean" = "mean",
                                                                                         "median" = "median",
                                                                                         "prototypes" = "prototypes"),
                                                                             selected = "mean"))),
                                                    
                                              
                                              
                                              
                                              
                                              
                                              conditionalPanel('input.graphType == "Boxplot"', 
                                                               checkboxInput("plotOutliers", "Plot outliers", value= T)),
                                              conditionalPanel('input.graphType != "Color" & input.graphType != "UMatrix" & input.graphType != "SmoothDist"', 
                                                               fluidRow(column(4, p("Superclass palette")),
                                                                        column(8, selectInput("palsc", NULL, 
                                                                           c("viridis", "grey", "rainbow", "heat", "terrain", 
                                                                             "topo", "cm", rownames(RColorBrewer::brewer.pal.info)), 
                                                                           "Set3")))),
                                              fluidRow(column(4, p("Plots palette")),
                                                       column(8,selectInput("palplot", NULL, 
                                                          c("viridis", "grey", "rainbow", "heat", "terrain", 
                                                            "topo", "cm", rownames(RColorBrewer::brewer.pal.info)), 
                                                          "viridis"))), 
                                              checkboxInput("plotRevPal", "Reverse palette"), 
                                              conditionalPanel('input.graphType == "Camembert"', 
                                                               checkboxInput("plotEqualSize", "Equal pie sizes", F)), 
                                              conditionalPanel('input.graphType == "Abstraction"', 
                                                               numericInput("plotAbstrCutoff", "Links cut-off", 
                                                                            min= 0, max= 1, step = .01, value= 0))),
                             fluidRow(column(4, h4("Plot size:")), 
                                      column(8, sliderInput("plotSize", NULL, 10, 1000, value= 100))),
                             hr(),
                             h4("Superclasses:"),
                             fluidRow(column(3, numericInput('kohSuperclass', NULL, 2, min= 1)), 
                                      column(5, selectInput('sup_clust_method', NULL, 
                                                            c("hierarchical", "pam"))), 
                                      conditionalPanel('input.sup_clust_method == "hierarchical"', 
                                                       column(4, selectInput("sup_clust_hcmethod", NULL, 
                                                                             c("ward.D2", "ward.D", "single", "complete", 
                                                                               "average", "mcquitty", 
                                                                               "median", "centroid"))))),
                             hr(),
                             fluidRow(column(4, h4("Save:")),
                                      column(4, downloadButton('downloadLink', 'Static Map')), 
                                      column(4, downloadButton('downloadInteractive', "Interactive Map"))),
                             hr(),
                             fluidRow(column(4, h4("Roll the dice:")), 
                                      column(8, actionButton('retrainButton', "Train new SOM"))),
                             uiOutput("plotWarning")),
                      
                      
                             
                      column(8, 
                             ## Display only the chosen graph
                             conditionalPanel('input.graphType == "Dendrogram"', 
                                              plotOutput("plotDendrogram")),
                             conditionalPanel('input.graphType == "Screeplot"', 
                                              plotOutput("plotScreeplot")),
                             conditionalPanel('input.graphType == "SmoothDist"',
                                              #maybe here insert the hex-based grid warning
                                              uiOutput("smooth_dist_warning"),
                                              plotOutput("plotSmoothDist")),
                             conditionalPanel('input.graphType == "Abstraction"', 
                                              plotOutput("plotAbstraction")),
                             conditionalPanel('input.graphType == "Silhouette"', 
                                              plotOutput("pam_silhouette")),
                             
                             
                             
                             
                             
                             
                                              # plotly::plotlyOutput("plotAbstraction")),
                             
                             #for all other JS based plots refer to graphs.html
                             conditionalPanel('input.graphType != "Dendrogram" & input.graphType != "Screeplot" & input.graphType != "SmoothDist" & input.graphType != "Abstraction"', 
                                              # HTML('<a id="downloadLink">Download Image</a>'),
                                              HTML('<img id="fromcanvasPlot" />'),
                                              HTML('<h4 id="cell-info">Hover over the plot for information.</h4>'),
                                              HTML('<h4 id="plot-message">-</h4>'),
                                              # HTML('<div id="thePlot" class="shiny-Plot"><svg /></div>'), #JS plots placed here?!
                                              aweSOM:::aweSOMoutput("theWidget"),
                                              HTML('<br />'), 
                                              wellPanel(HTML('<p id="plot-names">Observation names will appear here.</p>')), plotOutput("theLegend")
                                              
                                              
                                              
                                              )
                             ))), 
    tabPanel("Clustered data", 
             
             fluidRow(
               column(4,
                      h3("Download full map"),
                      downloadButton("somDownload", "Download SOM (rds file)"),
                      h3("Download data, SOM cells, and superclasses."),
                      downloadButton("clustDownload", "Download Clustering (csv file)"),
                      h4("Selected variables"),
                      fluidRow(column(4, actionButton("clustSelectNone", "Unselect all")), 
                               column(4, actionButton("clustSelectTrain", "Select train")), 
                               column(4, actionButton("clustSelectAll", "Select all"))),
                      uiOutput("clustVariables")),
               column(8, DT::dataTableOutput("clustTable")))),
    
    tabPanel("Reproducible Scripts",
             verbatimTextOutput("codeTxt") #, htmlOutput("codeTxt2")
             )
    
    
  )
))
