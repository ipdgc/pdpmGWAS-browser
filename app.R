library(shiny)
library(data.table)
library(DT)
library(shinycssloaders)

refTable <- fread("refData/reference.txt.gz") # fread("data/reference.txt")
refTable$START <- as.numeric(refTable$START)
refTable$CHR <- as.numeric(refTable$CHR)
resList <- list.files("resData/")
pre <- c("base_", "surv_", "cont_")
#smallTableList <- list.files("smallTable/")



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("PD Progression Meta-GWAS Browser"),
   h4('Please provide the inputs and hit "Create Table" to begin.'),
   
   # Sidebar 
   sidebarLayout(
     sidebarPanel(
       # input for chromosome+basepair range, gene name, and rsID
       textInput("variant",
                 label = NULL,
                 placeholder = "chr1:14000-15000 or WASH7P or rs201055865"),
       # filter for p < 0.05
       checkboxInput("p05", "P < 0.05", value = T),
       # only appears when above is unchecked
       conditionalPanel("input.p05 == false",
                        p("PLEASE NOTE: unfiltered data may take much longer to process.",
                          style = "font-weight:bold")
                        ),
       # selecting phenotype
       radioButtons("phenotype",
                    label = "Phenotype",
                    choiceNames = list("Constipation",
                                    "Cognitive impairment",
                                    "Daytime Sleepiness",
                                    "Depression",
                                    "Dyskinesias",
                                   "Hyposmia",
                                   "Insomnia",
                                   "Motor fluctuations",
                                   "REM sleep Behavior Disorder",
                                   "Restless Legs Syndrome",
                                   "Hoehn and Yahr score",
                                   "Hoehn and Yahr score of 3 or more",
                                   "Mini-Mental State Examination",
                                   "Montreal Cognitive Assessment",
                                   "SEADL *",
                                   "SEADL 70 or less",
                                   span("UPDRS","part I", tags$sup("†")),
                                   "UPDRS part II",
                                   "UPDRS part III",
                                   "UPDRS part IV",
                                   "UPDRS Total"),
                    choiceValues = c("CONST",
                                     "DEMENTIA",
                                     "SLEEP",
                                     "DEPR",
                                     "DYSKINESIAS",
                                "HYPOSMIA",
                                "INS",
                                "MOTORFLUX",
                                "RBD",
                                "RL",
                                "HY",
                                "HY3",
                                "MMSE",
                                "MOCA",
                                "SEADL",
                                "SEADL70",
                                "UPDRS1_scaled",
                                "UPDRS2_scaled",
                                "UPDRS3_scaled",
                                "UPDRS4_scaled",
                                "UPDRS_scaled")),
       actionButton("start", "Create Table"),
       div(style = "margin-left:10px;font-size:small;margin-top:14px;",
          p("* the modified Schwab and England Activities of Daily Living Scale"),
          p(tags$sup("†"), "the Unified Parkinson’s Disease Rating Scale OR the Movement Disorder Society revised UPDRS. All scores are scaled at the baseline (UPDRS1-3) or during the course.")
          ),
       width = 3
     ),
      
      # Main panel, contains the tables, the download buttons, and the About text
      mainPanel(
        tabsetPanel(
          # DT + download button
          tabPanel(title = "Tables",
                   h4("Baseline analysis by logistic regression model"),
                   withSpinner(DTOutput("baseResultTable")),
                   downloadButton("qkDL1", "Download (CSV)"),
                   h4("Survival analysis by cox hazard model"),
                   withSpinner(DTOutput("survResultTable")),
                   downloadButton("qkDL2", "Download (CSV)"),
                   h4("The mean difference for continuous trait over time by linear mixed effect model"),
                   withSpinner(DTOutput("contResultTable")),
                   downloadButton("qkDL3", "Download (CSV)"),
                   p("Key:"),
                   tags$ul(style = "column-count:3;margin-top:14px",
                     tags$li(tags$strong("REF-"), "Reference allele"),
                     tags$li(tags$strong("ALT-"), "Alternative/Effect/Minor allele"),
                     tags$li(tags$strong("MAF-"), "Minor allele frequency"),
                     tags$li(tags$strong("FUNC-"), "Functional group of loci (e.g. exonic, intronic)"),
                     tags$li(tags$strong("BETA-"), "Beta or natural log of Odds Ratio"),
                     tags$li(tags$strong("SE-"), "Standard error"),
                     tags$li(tags$strong("N-"), "Number of participants"),
                     tags$li(tags$strong("NSTUDY-"), "Number of studies"),
                     tags$li(tags$strong("Isq-"), "I square"),
                     tags$li(tags$strong("NearGENE-"), "Genes nearby"),
                     tags$li(tags$strong("CADD_phred-"), "Percentile highest scores"),
                     tags$li(tags$strong("SIFT_pred-"), "D: Deleterious; T: tolerated"),
                     tags$li(tags$strong("Polyphen2_HDIV_pred-"), "D: Probably damaging; P: Possibly damaging; B: Benign")
                   )),
          # About panel
          tabPanel(title = "About",
                   p("Placeholder text. Final text should include a link to the preprint."))
        )
      )
   )
)

server <- function(input, output) {
  # executes when clicking "create table"
   mergedTable <- eventReactive(input$start, {
     # catching empty inputs and whitespaces
     if (input$variant == "" | grepl("^\\s+$", input$variant)) {
       showNotification("Please enter a chromosome and base-pair range, gene name, or rsID.",
                        duration = 10,
                        type = "error")
       break
     }
     if (grepl("\\s", input$variant)) {
       showNotification("Please remove all whitespace in the input.",
                        duration = 10,
                        type = "error")
       break
     }
     finalTable <- list()
     resTableMid <- list()
     variantInput <- isolate(input$variant)
     phenotypeInput <- isolate(as.character(input$phenotype))
     # detecting rsID
     if (grepl("^rs\\d+", variantInput, ignore.case = T)) {
       refTableMid <- refTable[grepl(variantInput, refTable$RSID)]
       # detecting chr:bp
     } else if (grepl("\\d{1,2}:\\d+", variantInput)) {
       chr <- gsub("(\\d+):.*", "\\1", variantInput)
       refTableMid <- refTable[refTable$CHR == chr]
       # if range
       if (grepl("-", variantInput)) {
         start <- as.numeric(gsub("\\d{1,2}:(\\d+)-\\d+", "\\1", variantInput))
         end <- as.numeric(gsub("\\d{1,2}:\\d+-(\\d+)", "\\1", variantInput))
         refTableMid <- refTableMid[inrange(refTableMid$START, start, end)]
         # if single loci
       } else {
         refTableMid <- refTableMid[refTableMid$START == as.numeric(gsub("\\d{1,2}:(\\d+)", "\\1", variantInput))]
       }
       # detecting neargene name
     } else {
       refTableMid <- refTable[grepl(paste0(variantInput, "$"), refTable$NearGENE) | grepl(paste0(variantInput, ";"), refTable$NearGENE)]
     }
     # list of results with the named phenotype
     midResList <- grep(phenotypeInput, resList, value = T)

     # function for reading the relevant result table
     freadResTable <- function () {
       folder <- ifelse(input$p05, "smallTable/", "resData/")
       nest <- list()
       for (i in 1:3) {
         nest[[i]] <-  if (T %in% grepl(pre[i], midResList)) {
           fread(paste0(folder, grep(pre[i], midResList, value = T)))
         } else {
           NULL
         }
       }
       return(nest)
     }
     
     # function for merging the tables into one for display
     finalTableMerge <- function () {
       nest <- list()
       for (i in 1:3) {
         nest[[i]] <- if (T %in% grepl(pre[[i]], midResList)) {merge(refTableMid, resTableMid[[i]], by = "SNP")} else {data.table()}
         #nest[[i]] <- nest[[i]][,c(1:7,13:18,8,9)]
       }
       return(nest)
     }
     resTableMid <- freadResTable()
     finalTable <- finalTableMerge()
     return(finalTable)
     })

   # outputs of rendered tables
   output$baseResultTable <- DT::renderDT(
     mergedTable()[[1]],
     options = list(lengthChange = FALSE)
   )
   
   output$survResultTable <- DT::renderDT(
     mergedTable()[[2]],
     options = list(lengthChange = FALSE)
   )
   
   output$contResultTable <- DT::renderDT(
     mergedTable()[[3]],
     options = list(lengthChange = FALSE)
   )
   
   # downloadHandlers
   output$qkDL1 <- downloadHandler(
     filename = function() {
       paste0("pdprogbiomBase", Sys.Date(), ".csv")
     },
     content = function(file) {
       write.csv(mergedTable()[[1]], file, row.names = FALSE)
     }
   )
   
   output$qkDL2 <- downloadHandler(
     filename = function() {
       paste0("pdprogbiomSurv", Sys.Date(), ".csv")
     },
     content = function(file) {
       write.csv(mergedTable()[[2]], file, row.names = FALSE)
     }
   )
   
   output$qkDL3 <- downloadHandler(
     filename = function() {
       paste0("pdprogbiomCont", Sys.Date(), ".csv")
     },
     content = function(file) {
       write.csv(mergedTable()[[3]], file, row.names = FALSE)
     }
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

