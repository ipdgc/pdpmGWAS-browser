library(shiny)
library(data.table)
library(R.utils)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(shinymaterial)


# UI
ui <- fluidPage(
  theme = "materialize.min.css",
  useShinyjs(),
  # IPDGC logo
  a(
    href = "https://pdgenetics.org/",
    target = "_blank",
    img(
      src = "ipdgc.png",
      style = "position:fixed;bottom:1%;right:1%;width:200px"
    )
  ),
  # Initial loading animation from: https://loading.io/css/
  includeCSS("www/loading.css"),
  # Initialization card
  div(
    material_card(
      div(
        style = "text-align:center;",
        div("Initializing..."),
        div(
          class = "lds-ellipsis",
          div(),
          div(),
          div(),
          div()
        )
      )
    ), #/material_card
    id = "initcard",
    style = "position:fixed;
             top:30%;
             left:50%;
             z-index:5;"
  ),
  # Application title
  titlePanel("PD Progression Meta-GWAS Browser"),
  h4('Please provide the inputs and hit "Create Table" to begin.'),
  # Sidebar----contains inputs
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # input for chromosome+basepair range, gene name, and rsID
      textInput(
        "variant",
        label = NULL,
        placeholder = "chr1:14000-15000 or WASH7P or rs201055865"
      ),
      # filter for p < 0.05
      checkboxInput(
        "p05",
        "P < 0.05",
        value = T
      ),
      # only appears when above is unchecked
      conditionalPanel(
        "input.p05 == false",
        p(
          "PLEASE NOTE: unfiltered data may take much longer to process.",
          style = "font-weight:bold"
        )
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
      div(
        style = "margin-left:10px;font-size:small;margin-top:14px;",
        p("* the modified Schwab and England Activities of Daily Living Scale"),
        p(
          tags$sup("†"),
          "the Unified Parkinson’s Disease Rating Scale OR the Movement Disorder Society revised UPDRS. All scores are scaled at the baseline (UPDRS1-3) or during the course."
        )
      )
    ), # /sidebar
    
    # Main panel----contains tables, the download buttons, and the About text
    mainPanel(
      # tabset
      tabsetPanel(
        # Tab----Data Tables (DT)
        tabPanel(title = "Tables",
                 # DT for base_ table
                 div(
                   id = "baseTbl",
                   h4("Baseline analysis by logistic regression model"),
                   withSpinner(DTOutput("baseResultTable")),
                   downloadButton("qkDL1", "Download (CSV)")
                 ),
                 # DT for surv_ table
                 div(
                   id = "survTbl",
                   h4("Survival analysis by Cox hazard model"),
                   withSpinner(DTOutput("survResultTable")),
                   downloadButton("qkDL2", "Download (CSV)")
                 ),
                 # DT for cont_ table
                 # begin hidden, show when relevant phenotype is chosen (see server)
                 hidden(
                   div(
                     id = "contTbl",
                     h4("The mean difference for continuous trait over time by linear mixed effect model"),
                     withSpinner(DTOutput("contResultTable")),
                     downloadButton("qkDL3", "Download (CSV)")
                   )
                 ),
                 # Key
                 p(
                   "Key:",
                   style = "margin-top:12px"
                 ),
                 tags$ul(
                   style = "column-count:3;
                            margin-top:14px;",
                   tags$li(tags$strong("REF-"), "Reference allele"),
                   tags$li(tags$strong("ALT-"), "Alternative/Minor/Effect allele"),
                   tags$li(tags$strong("MAF-"), "Minor/Effect allele frequency"),
                   tags$li(tags$strong("FUNC-"), "Functional group of loci (e.g. exonic, intronic)"),
                   tags$li(tags$strong("BETA-"), "Effect/Beta or natural log of Odds Ratio of the effect allele"),
                   tags$li(tags$strong("SE-"), "Standard error"),
                   tags$li(tags$strong("N-"), "Number of participants"),
                   tags$li(tags$strong("NSTUDY-"), "Number of studies"),
                   tags$li(tags$strong("Isq-"), "I square"),
                   tags$li(tags$strong("NearGENE-"), "Genes nearby"),
                   tags$li(tags$strong("CADD_phred-"), "Percentile highest scores"),
                   tags$li(tags$strong("SIFT_pred-"), "D: Deleterious; T: tolerated"),
                   tags$li(tags$strong("Polyphen2_HDIV_pred-"), "D: Probably damaging; P: Possibly damaging; B: Benign")
                 )
        ), # /tab DT
        
        # Tab----Download large tables
        tabPanel(title = "Download all results",
                 div(
                   "Download allele reference (131.5 MB)",
                   style = "font-weight:bold;
                            margin-top:12px;
                            margin-bottom:6px;"
                 ),
                 downloadButton("DLref", "Download"),
                 p(
                   "Allele reference key:",
                   style = "font-weight:bold;
                            margin-top:14px"
                 ),
                 tags$ul(
                   style = "column-count:2;
                     margin-top:14px",
                   tags$li(tags$strong("REF-"), "Reference allele"),
                   tags$li(tags$strong("ALT-"), "Alternative/Minor/Effect allele"),
                   tags$li(tags$strong("MAF-"), "Minor/Effect allele frequency"),
                   tags$li(tags$strong("FUNC-"), "Functional group of loci (e.g. exonic, intronic)"),
                   tags$li(tags$strong("NearGENE-"), "Genes nearby")
                 ),
                 tags$hr(),
                 # initial analysis method radio button starts with base + surv---additional choices are updated by an observeEvent in server
                 radioButtons("analysisDL",
                              "Analysis Method:",
                              choiceNames = c("Baseline analysis",
                                              "Survival Analysis"),
                              choiceValues = c("base_",
                                               "surv_")),
                 checkboxInput("p05DL", "P < 0.05", value = T),
                 conditionalPanel(
                   "input.p05DL == false",
                   p(
                     "PLEASE NOTE: unfiltered data is significantly larger (up to 141.1 MB).",
                     style = "font-weight:bold"
                   )
                 ),
                 radioButtons("phenotypeDL",
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
                 downloadButton("DLtable", "Download"),
                 p(
                   "Phenotype key:",
                   style = "font-weight:bold;
                            margin-top:14px"
                 ),
                 tags$ul(style = "column-count:2;margin-top:14px",
                         tags$li(tags$strong("BETA-"), "Effect/Beta or natural log of Odds Ratio of the effect allele"),
                         tags$li(tags$strong("SE-"), "Standard error"),
                         tags$li(tags$strong("N-"), "Number of participants"),
                         tags$li(tags$strong("NSTUDY-"), "Number of studies"),
                         tags$li(tags$strong("Isq-"), "I square")
                 )
        ),
        # Tab----About
        tabPanel(
          title = "About",
          div(
            style = "font-weight:bold;
                     font-size:130%;
                    margin-bottom:12px",
            "For more information on this data, please see",
            a(
              "Iwaki et al. 2019 (under review)",
              href = "https://www.biorxiv.org/content/10.1101/585836v2",
              target = "_blank"
            ),
            "or contact:"
          ),
          div(span("Hirotaka Iwaki", style = "font-weight:bold; font-size:110%"),
              div(a(href="mailto:PDprogressionGWAS@gmail.com", "PDprogressionGWAS@gmail.com")),
              div("Statistical Genetics Section"),
              div("Laboratory of Neurogenetics"),
              div("National Institute on Aging"),
              div("National Institutes of Health"),
              div("Bethesda, Maryland 20892"),
              div("United States"))
        ) #/tab----About
      ) #/tabset
    ) #/Mainpanel
  ) #/sidebarLayout
) #fluidPage



server <- function(input, output, session) {
  # ====initial data load for the search function====
  # gene.List--contains genes found in the GWAS and their chromosome locations
  # rsID.List--contains rsIDs of all SNPs in the GWAS and their chromosome locations
  gene.List <- readRDS("refData/gene.List.rds")
  rsID.List <- readRDS("refData/rsID.DT.rds")
  # resList--list of files (i.e. tables) in "resData" folder
  resList <- list.files("resData/")
  pre <- c("base_", "surv_", "cont_")
  
  # hide initialization card when load is complete
  hide("initcard")
  
  #====Updating analysis method tables====
  # CONDITION: phenotype radio button changes in sidebar
  # RESULT: hide unused analysis method tables
  observeEvent(input$phenotype, { 
    if (input$phenotype
        %in%
        c("CONST",
          "DEPR",
          "HY3",
          "INS",
          "RBD",
          "SEDL70",
          "DEMENTIA",
          "DYSKINESIAS",
          "HYPOSMIA",
          "MOTORFLUX",
          "RL",
          "SLEEP")
    ) {
      # hide cont
      hide("contTbl")
      show("baseTbl")
      show("survTbl")
    } else {
      # hide base+surv
      hide("survTbl")
      hide("baseTbl")
      show("contTbl")
    }
  })#/observeEvent
  #=================================
  
  #====Updating analysis method input radio buttons for DL====
  # CONDITION: phenotype radio button changes in DL tab
  # RESULT: change analysis method to those that are available for that phenotype
  observeEvent(input$phenotypeDL, { 
    updateRadioButtons(session, "analysisDL",
                       choiceNames = if (input$phenotypeDL %in% c("CONST",
                                                                  "DEPR",
                                                                  "HY3",
                                                                  "INS",
                                                                  "RBD",
                                                                  "SEDL70",
                                                                  "DEMENTIA",
                                                                  "DYSKINESIAS",
                                                                  "HYPOSMIA",
                                                                  "MOTORFLUX",
                                                                  "RL",
                                                                  "SLEEP")) {
                         c("Baseline analysis",
                           "Survival Analysis")
                       } else {
                         c("Mean difference for continous trait")
                       },
                       choiceValues = if (input$phenotypeDL %in% c("CONST",
                                                                   "DEPR",
                                                                   "HY3",
                                                                   "INS",
                                                                   "RBD",
                                                                   "SEDL70",
                                                                   "DEMENTIA",
                                                                   "DYSKINESIAS",
                                                                   "HYPOSMIA",
                                                                   "MOTORFLUX",
                                                                   "RL",
                                                                   "SLEEP")) {
                         c("base_",
                           "surv_")
                       } else {
                         c("cont_")
                       })#/updateRadioButtons
  })#/observeEvent
  #=================================
  
  
  #====Function for producing DT====
  # CONDITION: clicking "create table"
  # RESULT: produce 2 data tables that can be inputs for renderDT()
  #
  # Honestly...this code is a mess. There is a lot of room for optimization here.
  # HOWEVER it is fast enough and was not the rate-limiting factor in the
  # previous round of optimizations.
  
  mergedTable <- eventReactive(input$start, {
    #====STAGE 0: DEFINE RELEVANT VARIABLES + IMPORTANT FUNCTIONS=====================
    # define lists
    finalTable <- list()
    resTableMid <- list()
    # variantInput--search terms in the search bar
    # midResList--names of the result files that correspond to the phenotype chosen with the radio button
    #             e.g. if phenotype == "constipation", then midResList == "base_CONST.txt.gz & surv_CONST.txt.gz"
    variantInput <- input$variant
    midResList <- grep(as.character(input$phenotype), resList, value = T)
    
    #====freadResTable====
    # Function for reading the relevant result files
    # Produces a nested list of data tables
    
    freadResTable <- function () {
      # determine p < 0.05 table (i.e. smallTable) or all table
      folder <- ifelse(input$p05, "smallTable/", "resData/")
      nest <- list()
      # reminder: "pre" variable is "base_", "surv_", and "cont_"
      for (i in 1:3) {
        nest[[i]] <-  if (T %in% grepl(pre[i], midResList)) {
          fread(paste0(folder, grep(pre[i], midResList, value = T)))
        } else {
          NULL
        }
      }
      return(nest)
    }
    #=======================
    
    
    #====finalTableMerge====
    # function for merging the DT table and allele information table (refTable)
    # produces a nested list of merged data tables ready for renderDT()
    finalTableMerge <- function () {
      nest <- list()
      for (i in 1:3) {
        nest[[i]] <- if (T %in% grepl(pre[[i]], midResList)) {merge(refTableMid, resTableMid[[i]], by = "SNP")[,c(1:7,13:18,8:12)]} else {data.table()}
      }
      return(nest)
    }
    #=======================
    
    #=================================================================================
    
    
    #====STAGE 1: CATCH EMPTY AND WHITESPACE INPUTS===================================
    if (input$variant == "" | grepl("^\\s+$", input$variant)) {
      showNotification("Please enter a chromosome and base-pair range, gene name, or rsID.",
                       duration = 10,
                       type = "error")
      stop("Empty input detected. Please enter a chromosome and base-pair range, gene name, or rsID.")
    }
    if (grepl("\\s", input$variant)) {
      showNotification("Please remove all whitespace in the input.",
                       duration = 10,
                       type = "error")
      stop("Whitespace detected in input. Please remove all whitespace in the input")
    }
    #=================================================================================
    
    #====STAGE 2: DETECT INPUT FORMAT + FILTER ALLELE INFORMATION TABLE (refTable)====
    
    # 1. Detecting rsID
    # previously able to search more vague rsID inputs, but the scope was reduced to
    # specific rsIDs for optimization purposes.
    #     e.g. Previouly an input of "rs38" would result in any rsIDs that began with
    #          "rs38". Now it only specifically outputs rsID of "rs38".
    if (grepl("^rs\\d+", variantInput, ignore.case = T)) {
      # find chromosome number of the SNP with rsID
      mid.rsID.List <- rsID.List[grepl(paste0(variantInput, "$"), rsID.List$RSID)]$CHR
      if (length(mid.rsID.List) == 0) {
        showNotification("rsID not found. Please check your input for error or input a different rsID.",
                         duration = 10,
                         type = "error")
        stop("rsID not found. Please check your input again or input a different rsID.")
      } else {
        # read relevant refTable with chromosome number
        refTable <- readRDS(paste0("refData/", "chr", mid.rsID.List, ".rds"))
      }
      refTableMid <- refTable[grepl(variantInput, refTable$RSID)]
      
      # 2. Detecting chr:bp
    } else if (grepl("\\d{1,2}:\\d+", variantInput)) {
      # extract chromosome number
      chr <- gsub(".*(\\d+):.*", "\\1", variantInput)
      # read relevant refTable with chromosome number
      refTableMid <- readRDS(paste0("refData/", "chr", chr, ".rds"))
      # if range
      if (grepl("-", variantInput)) {
        start <- as.numeric(gsub(".*\\d{1,2}:(\\d+)-\\d+", "\\1", variantInput))
        end <- as.numeric(gsub(".*\\d{1,2}:\\d+-(\\d+)", "\\1", variantInput))
        refTableMid <- refTableMid[inrange(refTableMid$START, start, end)]
        # if single loci
      } else {
        refTableMid <- refTableMid[refTableMid$START == as.numeric(gsub(".*\\d{1,2}:(\\d+)", "\\1", variantInput))]
      }
      
      # 3. Detecting gene ID
      # this was also more lax, but was restricted for optimization purposes
    } else {
      # find chromosome associated with given input either at the end of the element ("$")
      # or with semicolon (for variants with multiple associated gene ID)
      mid.gene.List <- gene.List[grepl(paste0(variantInput, "$"), gene.List$NearGENE) | grepl(paste0(variantInput, ";"), gene.List$NearGENE)]
      chr.gene.List <- unique(as.data.table(mid.gene.List$CHR))
      rm(mid.gene.List)
      if (nrow(chr.gene.List) > 1) {
        chr <- list()
        for (i in 1:nrow(chr.gene.List)){
          chr[[i]] <- readRDS(paste0("refData/", "chr", chr.gene.List[i,1], ".rds"))
        }
        refTable <- unlist(chr)
      } else if (nrow(chr.gene.List) == 0) {
        showNotification(paste0('No gene with the ID "', variantInput, '" found. Please check your input for error or input a different gene ID.'),
                         duration = 10,
                         type = "error")
        stop(paste0('No gene with the ID "', variantInput, '" found. Please check your input for error or input a different gene ID.'))
      } else {
        refTable <- readRDS(paste0("refData/", "chr", chr.gene.List[1,1], ".rds"))
      }
      refTableMid <- refTable[grepl(paste0(variantInput, "$"), refTable$NearGENE) | grepl(paste0(variantInput, ";"), refTable$NearGENE)]
    }
    #=================================================================================
    
    #====STAGE 3: READ RESULT TABLES AND MERGE WITH REFTABLE==========================
    resTableMid <- freadResTable()
    finalTable <- finalTableMerge()
    
    # if there are no results, throw error
    if (length(finalTable[[1]]$CHR) == 0 && length(finalTable[[2]]$CHR) == 0 && length(finalTable[[3]]$CHR)) {
      showNotification("The given input was not found in the chosen phenotype. Please change your input or phenotype and try again.",
                       type = "error",
                       duration = 10)
    }
    return(finalTable)
    #=================================================================================
  })#/observeEvent
  
  #====Outputting tables as renderDT====
  output$baseResultTable <- renderDT(
    mergedTable()[[1]],
    options = list(lengthChange = FALSE)
    # "lengthChange" argument allows end-user to determine the number of rows shown at
    # once. Disabled to optimize performance.
  )
  
  output$survResultTable <- renderDT(
    mergedTable()[[2]],
    options = list(lengthChange = FALSE)
  )
  
  output$contResultTable <- renderDT(
    mergedTable()[[3]],
    options = list(lengthChange = FALSE)
  )
  
  #========downloadHandlers========
  #====downloading search results====
  output$qkDL1 <- downloadHandler(
    filename = function() {
      paste0("pdprogbiomBase", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(mergedTable()[[1]], file)
    }
  )
  
  output$qkDL2 <- downloadHandler(
    filename = function() {
      paste0("pdprogbiomSurv", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(mergedTable()[[2]], file)
    }
  )
  
  output$qkDL3 <- downloadHandler(
    filename = function() {
      paste0("pdprogbiomCont", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(mergedTable()[[3]], file)
    }
  )
  #====downloading whole Tables====
  output$DLref <- downloadHandler(
    filename = function() {
      "reference.txt.gz"
    },
    content = function(file) {
      file.copy("smallTable/reference.txt.gz", file)
    })
  output$DLtable <- downloadHandler(
    filename = function() {
      grep(paste0(input$analysisDL, input$phenotypeDL,".txt.gz"), resList, value = T)
    },
    content = function(file) {
      prefile <- paste0(if (input$p05DL == T) {
        "smallTable/"
      } else {
        "resData/"},
      input$analysisDL,
      input$phenotypeDL,
      ".txt.gz")
      file.copy(prefile, file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)