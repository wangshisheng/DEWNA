library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(readxl)
library(gdata)
library(ggplot2)
library(ggsci)
library(DT)
library(data.table)
library(tidyverse)
library(ggExtra)
library(cowplot)
options(warn=-1)
#source("/srv/shiny-server/wkomics/main/global.R")
colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10),pal_nejm("default")(8),pal_lancet("lanonc")(9),
                      pal_jama("default")(7),pal_jco("default")(10),pal_ucscgb("default")(26),pal_d3("category10")(10),
                      pal_locuszoom("default")(7),pal_igv("default")(51),
                      pal_uchicago("default")(9),pal_startrek("uniform")(7),
                      pal_tron("legacy")(7),pal_futurama("planetexpress")(12),pal_rickandmorty("schwifty")(12),
                      pal_simpsons("springfield")(16),pal_gsea("default")(12)))
#
ui<-renderUI(
  fluidPage(
    title="DEWNA",
    shinyjs::useShinyjs(),
    #tags$script('$(document).on("shiny:sessioninitialized",function(){$.get("https://api.ipify.org", function(response) {Shiny.setInputValue("getIP", response);});})'),
    fluidRow(
      div(
        HTML(
          "<div style='text-align:center;margin-top:5px;margin-right:0px'>
            <a href='#' target=''><img src='DEWNAti.png' height='50px'>
            </a>
            </div>"
        )
      )
    ),
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="busystyle.css"),
        tags$script(type="text/javascript", src = "busy.js"),
        tags$style(type="text/css", "
                           #loadmessage {
                     position: fixed;
                     top: 0px;
                     left: 0px;
                     width: 100%;
                     height:100%;
                     padding: 250px 0px 5px 0px;
                     text-align: center;
                     font-weight: bold;
                     font-size: 100px;
                     color: #000000;
                     background-color: #D6D9E4;
                     opacity:0.6;
                     z-index: 105;
                     }
                     "),
        tags$script('
                            var dimension = [0, 0];
                    $(document).on("shiny:connected", function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    $(window).resize(function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    '),
        tags$style(type="text/css", "
                   #tooltip {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip2 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip3 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip4 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltipx5 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   ")
      )
    ),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(h2(strong("Calculating......")),img(src="rmd_loader.gif"),id="loadmessage")),
    tabsetPanel(
      id="maintab",
      tabPanel(
        "Welcome",
        uiOutput("welcomeui"),
        textOutput("jinriusernum"),
        icon = icon("home")
      ),
      tabPanel(
        "Import Data",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 1: Upload Original Data",
              tags$span(
                id = 'span1',
                `data-toggle` = "tooltip",
                title = '
                In this part, users can upload their own proteomics expression data and type in sample information. The example data can be found when users click "Load example data" below. Detailed descriptions are provided in the supplementary notes.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            #selectInput(
            #  "datatypex",
            #  h4("Data type:"),
            #  choices = c("Normal"=1,"MaxQuant"=2,"Proteome Discoverer"=3,"Spectronaut"=4),
            #  selected = 1
            #),
            #tags$hr(style="border-color: grey;"),
            h4("1. Expression data:"),
            radioButtons(
              "loaddatatype",
              label = NULL,
              choices = list("I. Upload" = 1,"II. Load example data"=2),
              selected = 1,
              inline = TRUE
            ),
            tags$hr(style="border-color: grey80;"),
            conditionalPanel(
              condition = "input.loaddatatype==1",
              fileInput('file1', h5('1.1. Import data：'),accept=c('.csv','.txt')),
              checkboxInput('header', 'First row as column names ?', TRUE),
              checkboxInput('firstcol', 'First column as row names ?', TRUE),
              #conditionalPanel(
              #  condition = "input.datatypex==1",
              #  fileInput('file1', h5('1.1.1 Import your data：'),
              #            accept=c('.csv')),
              #  checkboxInput('header', 'First row as column names ?', TRUE),
              #  checkboxInput('firstcol', 'First column as row names ?', FALSE)
              #),
              #conditionalPanel(
              #  condition = "input.datatypex==2",
              #  fileInput('file2', h5('1.1.2 Import your data：'),
              #            accept=c('.txt'))
              #),
              #conditionalPanel(
              #  condition = "input.datatypex==3",
              #  fileInput('file3', h5('1.1.3 Import your data：'),
              #            accept=c('.xlsx'))
              #),
              #conditionalPanel(
              #  condition = "input.datatypex==4",
              #  fileInput('file4', h5('1.1.4 Import your data：'),
              #            accept=c('.csv'))
              #),
              tags$hr(style="border-color: grey80;"),
              h4("2. Samples information:"),
              textInput("grnums",h5("2.1. Group and replicate number:"),value = ""),
              bsTooltip("grnums",'Type in the group number and replicate number here. Please note, the group number and replicate number are linked with ";", and the replicate number of each group is linked with "-". For example, if you have two groups, each group has three replicates, then you should type in "2;3-3" here. Similarly, if you have 3 groups with 5 replicates in every groups, you should type in "3;5-5-5".',
                        placement = "right",options = list(container = "body")),
              textInput("grnames",h5("2.2. Group names:"),value = ""),
              bsTooltip("grnames",'Type in the group names of your samples. Please note, the group names are linked with ";". For example, there are two groups, you can type in "Control;Experiment".',
                        placement = "right",options = list(container = "body"))
            ),
            conditionalPanel(
              condition = "input.loaddatatype==2",
              #conditionalPanel(
              #  condition = "input.datatypex==1",
              #  downloadButton("loaddatadownload1","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              #),
              #conditionalPanel(
              #  condition = "input.datatypex==2",
              #  downloadButton("loaddatadownload2","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              #),
              #conditionalPanel(
              #  condition = "input.datatypex==3",
              #  downloadButton("loaddatadownload3","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              #),
              #conditionalPanel(
              #  condition = "input.datatypex==4",
              #  downloadButton("loaddatadownload4","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              #),
              downloadButton("loaddatadownload1","Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED"),
              #tags$hr(style="border-color: grey;"),
              #downloadButton("loaddatadownload2","Download example sample group data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              h4("Samples information:"),
              textInput("examgrnums",h5("2.1. Group and replicate number:"),value = "8;4-4-4-4-4-4-4-4"),#7;5-6-5-5-5-5-5
              textInput("examgrnames",h5("2.2. Group names:"),value = "T00;T03;T06;T09;T12;T18;T24;T36")
              #conditionalPanel(
              #  condition = "input.datatypex!=4",
              #  textInput("examgrnums",h5("2.1 Group and replicate number:"),value = "7;5-6-5-5-5-5-5"),
              #  textInput("examgrnames",h5("2.2 Group names:"),value = "T0;T1;T2;T3;T4;T5;T6")
              #),
              #conditionalPanel(
              #  condition = "input.datatypex==4",
              #  textInput("examgrnums2",h5("2.1 Group and replicate number:"),value = "2;10-10"),
              #  textInput("examgrnames2",h5("2.2 Group names:"),value = "Cyc;Noco")
              #)
            ),
            tags$hr(style="border-color: grey80;"),
            h4("3. Height and width for figures:"),
            numericInput("preheight",h5("3.1. Height for figure:"),value = 1000),
            numericInput("prewidth",h5("3.2. Width for figure:"),value = 900)
          ),
          mainPanel(
            width = 9,
            hr(),
            h4("1. Expression data："),
            hr(),
            dataTableOutput("peaksdata")#,
            #h4("2. Samples information data："),
            #dataTableOutput("samplesdata")
          )
        ),
        icon = icon("upload")
      ),
      tabPanel(
        "Data Preprocessing",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 2: Data Pre-processing",
              tags$span(
                id = 'span2',
                `data-toggle` = "tooltip2",
                title = '
                Here "Data Pre-processing" means this tool will process data normalization (Median normalization by default), coefficient of variation calculation, missing value statsx (KNN method by default) for the uploaded expression data.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            textInput("natype",h5("1. Missing value type:"),value = "NA"),
            bsTooltip("natype",'The type of missing value in the expression data.',
                      placement = "right",options = list(container = "body")),
            #textInput("natypecol",h5("2. Colors for plot by row:"),value = "#A11315;#8C86FF"),
            #hr(),
            checkboxInput('fenzukaolvif', '2. Count NA by each group or not?', FALSE),
            bsTooltip("fenzukaolvif",'If true, DEWNA will count the NA number in every group, otherwise, it will count the NA number across all groups.',
                      placement = "right",options = list(container = "body")),
            #conditionalPanel(
            #  condition = 'input.fenzukaolvif==true',
            #  checkboxInput("keepzeroif","2.1. Keep NA as zero (0) or not?",FALSE),
            #  bsTooltip("keepzeroif",'If true, that means when all of the values of one protein/peptide are NA in one group, DEWNA will keep these NA as 0.',
            #            placement = "right",options = list(container = "body"))
            #),
            numericInput('naratio', h5('3. NA ratio:'), 0.5,max = 1,min = 0,step = 0.1),
            bsTooltip("naratio",'The threshold of NA ratio. One protein/peptide with NA ratio above this threshold will be removed.',
                      placement = "right",options = list(container = "body")),
            checkboxInput('mediannormif', '4. Median normalization or not?', TRUE),
            bsTooltip("mediannormif",'If true, the values in expression matrix will be devided by its column median value to make the samples to have the same median. (Please note, DEWNA was not designed to perform sophisticated normalization analysis. Any normalized datasets with NA can be accepted for analysis).',
                      placement = "right",options = list(container = "body")),
            checkboxInput('logif', '5. Log or not?', TRUE),
            bsTooltip("logif",'If true, the values in expression matrix will be log-transformed with base 2.',
                      placement = "right",options = list(container = "body")),
            checkboxInput('cvyuzhiif', '6. CV or not?', FALSE),
            bsTooltip("cvyuzhiif",'Whether to set the threshold of coefficient of variation (CV).',
                      placement = "right",options = list(container = "body")),
            conditionalPanel(
              condition = "input.cvyuzhiif==true",
              numericInput('cvyuzhi', h5('6.1. CV threshold (raw scale):'), 0.5,max = 1000,min = 0,step = 0.1),
              bsTooltip("cvyuzhi",'The threshold of coefficient of variation (CV). One protein/peptide with CV above this threshold will be removed. "raw scale" here means the values without log-transformation are used to calculate the CV.',
                        placement = "right",options = list(container = "body"))
            ),
            tags$hr(style="border-color: grey;"),
            actionButton("mcsbtn_nafenbu","Start",icon("paper-plane"),width="100px",#Calculate
                         style="color: #fff; background-color: #00A087FF; border-color: #00A087FF")##CD853F
          ),
          mainPanel(
            width = 9,
            hidden(
              div(
                id="mcsbtn_nafenbu_hid",
                h4("2.1. Imputation table:"),
                downloadButton("knnimputeresdl","Download"),
                dataTableOutput("knnimputeres"),
                hr(),
                h4("2.2. NA plot by column:"),
                downloadButton("naplotbycolumndl","Download"),
                plotOutput("naplotbycolumn",height = "900px"),
                hr(),
                h4("2.3. NA plot by row:"),
                downloadButton("naplotbyrowdl","Download"),
                plotOutput("naplotbyrow")#,
                #downloadButton("nadatadfdl","Download"),
                #dataTableOutput("nadatadf")
              )
            )
          )
        ),
        icon = icon("binoculars")
      ),
      tabPanel(
        "Results",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 3: Results",
              tags$span(
                id = 'span4',
                `data-toggle` = "tooltip4",
                title = '
                In this part, users can obtain all results produced from DEWNA.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )#Detailed descriptions are provided in the "Help" part.
            ),
            checkboxInput("autoclusterif","1. Whether type in the cluster number?",value = FALSE),
            bsTooltip("autoclusterif",'If true, users could type in the number of clusters they want. By default, the number of clusters is the ceiling of the square root of the protein number, for example, if there are 8000 proteins, the cluster number is 90.',
                      placement = "right",options = list(container = "body")),
            conditionalPanel(
              condition = "input.autoclusterif==true",
              numericInput("clusternum",h5("1.1. Clustering number:"),value = 50)
            ),
            textInput("groupcol",h5("2. Color for times/groups:"),value = "#FEE8C8;#FDD49E;#FDBB84;#FC8D59;#EF6548;#D7301F;#B30000;#7F0000"),
            textInput("heatmapcol",h5("3. Color for heatmap:"),value = "blue;white;red"),
            checkboxInput("targetMif","4. Type in targeted cluster or not?",value = TRUE),
            bsTooltip("targetMif",'If true, users could type in the cluster index of their interest, and this tool would find hubs from these clusters. Otherwise, this tool will analyze all clusters, which will take more time.',
                      placement = "right",options = list(container = "body")),
            conditionalPanel(
              condition = "input.targetMif==true",
              textInput("targetModule",h5("4.1. Targeted cluster index:"),value = "2;5;21;24;27;35;38;46;57")
            ),
            textInput("wuzhongkegg",h5("5. Species id from KEGG database:"),value = "hsa"),
            selectInput("keggpathselect",h5("6. KEGG pathway classification:"),choices = c("Metabolism","Genetic Information Processing","Environmental Information Processing",
                                                            "Cellular Processes","Organismal Systems","Human Diseases"),#,"Drug Development"
                        selected=c("Metabolism","Genetic Information Processing","Environmental Information Processing",
                                   "Cellular Processes","Organismal Systems","Human Diseases"),multiple = T),
            #numericInput("fcthreshold",h5("3. Threshold for fold change:"),value = 1.5),
            tags$hr(style="border-color: grey;"),
            actionButton("mcsbtn_entropyres","Start",icon("paper-plane"),width="100px",
                         style="color: #fff; background-color: #00A087FF; border-color: #00A087FF")
          ),
          mainPanel(
            width = 9,
            div(
              id="resultsxuanzeid",style="font-size:16px;",
              radioButtons(
                "resultsxuanze",
                label = h4(""),
                #choices = list("3.1. Clustering" = 1,"3.2. Cluster activity matrix"=2,
                #               "3.3. Network and hubs"=3,"3.4. Hubs pathways"=4,
                #               "3.5. Pathway activity analysis"=5),
                choices = list("3.1. Cluster activity analysis" = 1,
                               "3.2. Pathway activity analysis"=2),
                selected = 1,
                inline = TRUE
              )
            ),
            tags$hr(style="border-color: grey;"),
            hidden(
              div(
                id="resultsxuanze_btn",
                conditionalPanel(
                  condition = "input.resultsxuanze==1",
                  div(
                    id="resxuanzeclusterid",style="font-size:16px;",
                    radioButtons(
                      "resxuanzecluster",
                      label = NULL,#h4("")
                      choices = list("3.1.1. Clustering" = 1,"3.1.2. Cluster activity matrix"=2,
                                     "3.1.3. Network and hubs"=3,"3.1.4. Hubs pathways"=4),
                      selected = 1,
                      inline = TRUE
                    )
                  ),
                  hr(),
                  conditionalPanel(
                    condition = "input.resxuanzecluster==1",
                    h4("3.1.1.1. Clustering table:"),
                    downloadButton("clusteringdfdl","Download"),
                    dataTableOutput("clusteringdf")#,
                    #hr(),
                    #h4("3.1.2. Clustering plot:"),
                    #downloadButton("clusteringplotdl","Download"),
                    #plotOutput("clusteringplot",height = "900")
                  ),
                  conditionalPanel(
                    condition = "input.resxuanzecluster==2",
                    h4("3.1.2.1. Heatmap of cluster activity value matirx:"),
                    downloadButton("MEheatmapdl","Download"),
                    plotOutput("MEheatmap",height = "900"),
                    hr(),
                    h4("3.1.2.2. Cluster activity value table:"),
                    downloadButton("moduleresdfdl","Download"),
                    dataTableOutput("moduleresdf"),
                    hr(),
                    h4("3.1.2.3. Entropy weighted expression table:"),
                    downloadButton("entropyweightdfdl","Download"),
                    dataTableOutput("entropyweightdf")
                  ),
                  conditionalPanel(
                    condition = "input.resxuanzecluster==3",
                    h4("3.1.3.1. Network of hubs:"),
                    downloadButton("hubnetworkpdl","Download"),
                    plotOutput("hubnetworkp",height = "900"),
                    hr(),
                    h4("3.1.3.2. Entropy weighted expression table of Hubs:"),
                    downloadButton("hubnetworkdfdl","Download"),
                    dataTableOutput("hubnetworkdf")
                  ),
                  conditionalPanel(
                    condition = "input.resxuanzecluster==4",
                    h4("3.1.4.1. Hubs pathway barplot based on KEGG database:"),
                    downloadButton("hubpathwaypdl","Download"),
                    plotOutput("hubpathwayp",height = "900"),
                    hr(),
                    h4("3.1.4.2. Hubs pathway table based on KEGG database:"),
                    downloadButton("hubpathwaydfdl","Download"),
                    dataTableOutput("hubpathwaydf")
                  )
                ),
                #conditionalPanel(
                #  condition = "input.resultsxuanze==2",
                #  h4("3.2.1. Heatmap of cluster activity value matirx:"),
                #  downloadButton("MEheatmapdl","Download"),
                #  plotOutput("MEheatmap",height = "900"),
                #  hr(),
                #  h4("3.2.2. Cluster activity value table:"),
                #  downloadButton("moduleresdfdl","Download"),
                #  dataTableOutput("moduleresdf"),
                #  hr(),
                #  h4("3.2.3. Entropy weighted expression table:"),
                #  downloadButton("entropyweightdfdl","Download"),
                #  dataTableOutput("entropyweightdf")
                #),
                #conditionalPanel(
                #  condition = "input.resultsxuanze==3",
                #  h4("3.3.1. Network of hubs:"),
                #  downloadButton("hubnetworkpdl","Download"),
                #  plotOutput("hubnetworkp",height = "900"),
                #  hr(),
                #  h4("3.3.2. Entropy weighted expression table of Hubs:"),
                #  downloadButton("hubnetworkdfdl","Download"),
                #  dataTableOutput("hubnetworkdf")
                #),
                #conditionalPanel(
                #  condition = "input.resultsxuanze==4",
                #  h4("3.4.1. Hubs pathway barplot based on KEGG database:"),
                #  downloadButton("hubpathwaypdl","Download"),
                #  plotOutput("hubpathwayp",height = "900"),
                #  hr(),
                #  h4("3.4.2. Hubs pathway table based on KEGG database:"),
                #  downloadButton("hubpathwaydfdl","Download"),
                #  dataTableOutput("hubpathwaydf")
                #),
                conditionalPanel(
                  condition = "input.resultsxuanze==2",
                  h4("3.2.1. Heatmap of KEGG pathway activity value matirx:"),
                  downloadButton("keggMEheatmapdl","Download"),
                  plotOutput("keggMEheatmap",height = "900"),
                  hr(),
                  h4("3.2.2. Bar plots of significantly enriched KEGG pathways:"),
                  fluidRow(
                    column(
                      6,
                      numericInput("ReporterScoreyz",h5("Threshold of Reporter Score:"),value = 2)
                    ),
                    column(
                      2,
                      div(style="margin-top:40px;",downloadButton("kegggrsabardl","Download"))
                    )
                  ),
                  plotOutput("kegggrsabar",height = "900"),
                  hr(),
                  h4("3.2.3. KEGG pathway activity value and reporter score table:"),
                  downloadButton("keggmoduleresdfdl","Download"),
                  dataTableOutput("keggmoduleresdf"),
                  hr(),
                  h4("3.2.4. Network of significantly enriched KEGG pathways:"),
                  fluidRow(
                    column(
                      6,
                      textInput("map_idx",h5("Targeted KEGG pathway ids:"),value = "hsa04110;hsa03030;hsa03410;hsa03430;hsa04630;hsa04140;hsa04024",width=550),
                      bsTooltip("map_idx",'The interested KEGG pathway ids can be copied from the "3.5.3. KEGG pathway activity value and reporter score table" above. Every id should be pasted with semicolon together.',
                                placement = "right",options = list(container = "body"))
                    ),
                    column(
                      2,
                      div(style="margin-top:40px;",downloadButton("kegggrsanetworkdl","Download"))
                    )
                  ),
                  #textInput("map_idx",h5("Targeted KEGG pathway ids:"),value = "mmu00600;mmu00590;mmu00190;mmu00280;mmu00020",width=600),
                  #downloadButton("kegggrsanetworkdl","Download"),
                  plotOutput("kegggrsanetwork",height = "900"),
                  hr(),
                  h4("3.2.5. KEGG pathway and protein statistical results table:"),
                  downloadButton("kegggrsaprodfdl","Download"),
                  dataTableOutput("kegggrsaprodf")
                )
              )
            )
          )
        ),
        icon = icon("table")
      )
    )
  )
)
#
server<-shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=100*1024^2)
  usertimenum<-as.numeric(Sys.time())
  #ui
  output$welcomeui<-renderUI({
    screenwidth<-input$dimension[1]
    if(is.null(screenwidth)){
      return(NULL)
    }else{
      if(screenwidth<=1024){
        imgwidth<-200
      }
      else if(screenwidth>1024 & screenwidth<=1440){
        imgwidth<-300
      }
      else{
        imgwidth<-400
      }
    }

    fluidRow(
      #div(style="text-align:center",h1("~~Welcome~~")),
      div(
        id="mainbody",
        column(3),
        column(
          6,
          div(style="text-align:left;margin-top:20px;font-size:150%;color:#E64B35FF",
              HTML("<em>Welcome to DEWNA</em>")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px",
              HTML("<b><i>DEWNA</i></b> (<u>D</u>ynamic <u>E</u>ntropy <u>W</u>eight <u>N</u>etwork <u>A</u>nalysis) is a sophisticated and user-friendly stand-alone software designed to facilitate the dynamic analysis of proteins and their associated functions in response to various treatments. This tool integrates two powerful methodologies: the entropy weight method and multiscale embedded gene co-expression network analysis, providing a comprehensive framework for processing time-course proteome expression data. The entropy weight method assigns weights to indices based on value dispersion, evaluating the perturbation of clusters or pathways by examining protein expression profiles. Higher variability in protein expression indicates a significant response to treatment, leading to greater weight assignment and providing nuanced insights into how treatments affect proteins and pathways over time. Protein co-expression networks reveal the multi-scale organization of protein clusters, identifying key proteins (hubs) and uncovering new potential therapeutic targets by analyzing time-course data. DEWNA integrates these methodologies to enhance dynamic proteomic data analysis by allowing users to input time-course proteome expression data, apply entropy weight calculations, and construct co-expression networks, resulting in detailed analysis and visualization of entropy profiles, network diagrams, and temporal activity trends. The software's user-friendly design includes both a web-based platform and a stand-alone version, ensuring broad accessibility and usability. DEWNA is particularly valuable in studying dynamic biological processes such as the response to chemotherapy drugs like cisplatin, understanding drug mechanisms, identifying biomarkers, and discovering therapeutic targets, making it an invaluable resource for researchers studying complex biological systems and responses to treatments. The source codes and detailed manual can be accessed at our <a href='https://github.com/wangshisheng/DEWNA' target='_blank'>GitHub</a>. (<b>Please Note:</b> If this online version does not work, which means you cannot open the software link, it is probably because our server is down and we will fix it very soon. Or, please try to install this tool and run it locally.)")),
          div(style="text-align:center;margin-top: 50px",
              a(href='#',
                img(src='DEWNAhome.png',height=imgwidth))),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;margin-top:20px;font-size:120%",
              HTML("DEWNA is developed by <a href='https://shiny.rstudio.com/' target='_blank'>R shiny (Version 1.3.2)</a>, and is free and open to all users with no login requirement. It can be readily accessed by all popular web browsers including Google Chrome, Mozilla Firefox, Safari and Internet Explorer 10 (or later), and so on. We would highly appreciate that if you could send your feedback about any bug or feature request to Shisheng Wang at <u>wsslearning@omicsolution.com</u>.")),
          div(style="text-align:center;margin-top:20px;font-size:140%;color:darkgreen",
              HTML("<br />"),
              HTML("~~ Enjoy yourself in DEWNA ~~")),
          tags$hr(style="border-color: grey60;"),
          div(style="text-align:center;margin-top: 20px;margin-bottom: 20px;font-size:100%",
              HTML(" &copy; 2024 <a href='http://english.cd120.com/' target='_blank'>Hao Yang's Group</a>. All Rights Reserved."))
        ),
        column(3)
      )
    )
  })
  examplepeakdatas<-reactive({
    library(writexl)
    #if(input$datatypex==1){
    #  dataread<-read.csv("Exampledata1.csv",stringsAsFactors = F,check.names = F)
    #}
    #else if(input$datatypex==2){
    #  dataread<-read.csv("Exampledata2.txt",stringsAsFactors = F,check.names = F,sep="\t")
    #}
    #else if(input$datatypex==3){
    #  dataread<-read_excel("Exampledata3.xlsx")
    #}
    #else{
    #  dataread<-read.csv("Exampledata4.csv",stringsAsFactors = F,check.names = F,na.strings = "Filtered")
    #}
    dataread<-read.csv("Exampledata1.csv",stringsAsFactors = F,check.names = F,row.names = 1)
    dataread
  })
  #examplesampledatas<-reactive({
  #  if(input$datatypex==1 | input$datatypex==2 | input$datatypex==3){
  #    dataread<-read.csv("grinfo.csv",header = T,stringsAsFactors = F,check.names = F)
  #  }else{
  #    dataread<-read.csv("grinfo2.csv",header = T,stringsAsFactors = F,check.names = F)
  #  }
  #  colnames(dataread)<-c("Samples","Groups")
  #  dataread
  #})
  output$loaddatadownload1<-downloadHandler(
    filename = function(){paste("Example_ExpressionData_Normal_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplepeakdatas(),file,row.names = T)
    }
  )
  #output$loaddatadownload2<-downloadHandler(
  #  filename = function(){paste("Example_ExpressionData_MaxQuant_",usertimenum,".txt",sep="")},
  #  content = function(file){
  #    write.table(examplepeakdatas(),file,row.names = FALSE,sep="\t")
  #  }
  #)
  #output$loaddatadownload3<-downloadHandler(
  #  filename = function(){paste("Example_ExpressionData_PD_",usertimenum,".xlsx",sep="")},
  #  content = function(file){
  #    write_xlsx(examplepeakdatas(),file)
  #  }
  #)
  #output$loaddatadownload4<-downloadHandler(
  #  filename = function(){paste("Example_ExpressionData_Spectronaut_",usertimenum,".csv",sep="")},
  #  content = function(file){
  #    write.csv(examplepeakdatas(),file,row.names = FALSE)
  #  }
  #)
  #output$loaddatadownload2<-downloadHandler(
  #  filename = function(){paste("Example_SampleData_",usertimenum,".csv",sep="")},
  #  content = function(file){
  #    write.csv(examplesampledatas(),file,row.names = FALSE)
  #  }
  #)
  peaksdataout<-reactive({
    files1 <- input$file1
    if (is.null(files1)){
      dataread<-data.frame(Description="DEWNA detects that you did not upload your data. Please upload the expression data, or load the example data to check first.")
    }else{
      if(sum(input$firstcol)==1){
        rownametf<-1
      }else{
        rownametf<-NULL
      }
      #aax<<-files1$datapath
      if(length(grep("\\.csv$",files1$datapath))>0){
        sepchar<-","
      }else{
        sepchar<-"\t"
      }
      dataread<-read.csv(files1$datapath,header=input$header,sep=sepchar,
                         row.names = rownametf,stringsAsFactors = F)
    }
    #if(input$datatypex==1){
    #  files1 <- input$file1
    #  if (is.null(files1)){
    #    dataread<-data.frame(Description="DEWNA detects that you did not upload your data. Please upload the expression data, or load the example data to check first.")
    #    list(yuanshidf=dataread)
    #  }else{
    #    if(sum(input$firstcol)==1){
    #      rownametf<-1
    #    }else{
    #      rownametf<-NULL
    #    }
    #    dataread<-read.csv(files1$datapath,header=input$header,
    #                       row.names = rownametf,stringsAsFactors = F)
    #    dataread1<-dataread[,-c(1,2)]
    #    dataread2<-dataread[,c(1,2)]
    #    #rowpaste<-apply(dataread2,1,function(x){
    #    #  paste0(x,collapse = "_")
    #    #})
    #    rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
    #    dataread1x<-dataread1[!duplicated(rowpaste),]
    #    rownames(dataread1x)<-rowpaste[!duplicated(rowpaste)]
    #    list(yuanshidf=dataread,yuanshidata=dataread1x,objectinfo=dataread2)
    #  }
    #}
    #else if(input$datatypex==2){
    #  files2 <- input$file2
    #  if (is.null(files2)){
    #    dataread<-data.frame(Description="DEWNA detects that you did not upload your data. Please upload the proteinGroups.txt file from MaxQuant, or load the example data to check first.")
    #    list(yuanshidf=dataread)
    #  }else{
    #    dataread<-read.csv(files2$datapath,stringsAsFactors = F,check.names = F,sep="\t")
    #    datamaxqpro1<-dataread[-which(dataread$Reverse=="+" | dataread$`Potential contaminant`=="+"),]
    #    proidsstr<-strsplit(datamaxqpro1$`Protein IDs`,";")
    #    proids<- as.character(lapply(proidsstr,function(x) x[[1]][1]))
    #    datamaxqpro1$`Protein IDs`<-proids
    #    prolfdnames1<-grep("LFQ intensity",colnames(datamaxqpro1),value = TRUE)
    #    datamaxqpro2<-datamaxqpro1[,c("Protein IDs","Razor + unique peptides",prolfdnames1)]
    #    prolfdnames<-gsub("LFQ intensity ","",prolfdnames1)
    #    colnames(datamaxqpro2)<-c("Protein IDs","PeptidesNum",prolfdnames)
    #    datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
    #    datamaxqpro4<-datamaxqpro3[,-c(1,2)]
    #    dataread2<-datamaxqpro3[,c(1,2)]
    #    #rowpaste<-apply(dataread2,1,function(x){
    #    #  paste0(x,collapse = "_")
    #    #})
    #    rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
    #    rownames(datamaxqpro4)<-rowpaste#datamaxqpro3[,1]
    #    pro0index<-apply(datamaxqpro4,1,function(x) sum(x)==0)
    #    datamaxqpro5<-datamaxqpro4[!pro0index,]
    #    datamaxqpro5[datamaxqpro5==0]<-0
    #    list(yuanshidf=dataread,yuanshidata=datamaxqpro5,objectinfo=dataread2)
    #  }
    #}
    #else if(input$datatypex==3){
    #  files3 <- input$file3
    #  if (is.null(files3)){
    #    dataread<-data.frame(Description="DEWNA detects that you did not upload your data. Please upload that Proteins file (.xlsx) exported from Proteome Discoverer, or load the example data to check first.")
    #    list(yuanshidf=dataread)
    #  }else{
    #    dataread<-read_excel(files3$datapath)
    #    datamaxqpro1<-dataread
    #    prolfdnames1<-grep("Abundances \\(Scaled\\): ",colnames(datamaxqpro1),value = TRUE)
    #    datamaxqpro2<-datamaxqpro1[,c("Accession","# Peptides",prolfdnames1)]
    #    prolfdnames<-gsub("Abundances \\(Scaled\\): ","",prolfdnames1)
    #    colnames(datamaxqpro2)<-c("Protein IDs","PeptidesNum",prolfdnames)
    #    datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
    #    datamaxqpro4<-as.data.frame(datamaxqpro3[,-c(1,2)])
    #    dataread2<-datamaxqpro3[,c(1,2)]
    #    #rowpaste<-apply(dataread2,1,function(x){
    #    #  paste0(x,collapse = "_")
    #    #})
    #    rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
    #    rownames(datamaxqpro4)<-rowpaste#datamaxqpro3[,1]
    #    #pro0index<-apply(datamaxqpro4,1,function(x) sum(x)==0)
    #    datamaxqpro5<-datamaxqpro4#[!pro0index,]
    #    list(yuanshidf=dataread,yuanshidata=datamaxqpro5,objectinfo=dataread2)
    #  }
    #}
    #else{
    #  files4 <- input$file4
    #  if (is.null(files4)){
    #    dataread<-data.frame(Description="DEWNA detects that you did not upload your data. Please upload the Protein Quant file (.csv) exported from Spectronaut, or load the example data to check first.")
    #    list(yuanshidf=dataread)
    #  }else{
    #    dataread<-read.csv(files4$datapath,stringsAsFactors = F,check.names = F,na.strings = "Filtered")
    #    prolfdnames1<-grep("NrOfPrecursorsUsedForQuantification",colnames(dataread),value = TRUE)
    #    datamaxqpro1<-dataread[,prolfdnames1]
    #    PeptidesNum<-apply(datamaxqpro1,1,sum,na.rm=TRUE)
    #    proidsstr<-strsplit(dataread$PG.ProteinGroups,";")
    #    proids<- as.character(lapply(proidsstr,function(x) x[[1]][1]))
    #    prolfdnames1<-grep("PG\\.Quantity",colnames(dataread),value = TRUE)
    #    datamaxqpro2<-dataread[,c("PG.ProteinGroups",prolfdnames1)]
    #    prolfdnames<-gsub("\\[\\d+\\] |\\.raw\\.PG\\.Quantity","",prolfdnames1)
    #    colnames(datamaxqpro2)<-c("Protein IDs",prolfdnames)
    #    datamaxqpro2$`Protein IDs`<-proids
    #    datamaxqpro2$PeptidesNum<-PeptidesNum
    #    datamaxqpro2<-datamaxqpro2[,c(1,ncol(datamaxqpro2),2:(ncol(datamaxqpro2)-1))]
    #    datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
    #    datamaxqpro4<-datamaxqpro3[,-c(1,2)]
    #    dataread2<-datamaxqpro3[,c(1,2)]
    #    #rowpaste<-apply(dataread2,1,function(x){
    #    #  paste0(x,collapse = "_")
    #    #})
    #    rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
    #    rownames(datamaxqpro4)<-rowpaste#datamaxqpro3[,1]
    #    #pro0index<-apply(datamaxqpro4,1,function(x) sum(x)==0)
    #    datamaxqpro5<-datamaxqpro4#[!pro0index,]
    #    list(yuanshidf=dataread,yuanshidata=datamaxqpro5,objectinfo=dataread2)
    #  }
    #}
  })
  output$peaksdata<-renderDataTable({
    if(input$loaddatatype==1){
      peaksdatax<<-peaksdataout()
      datatable(peaksdatax, options = list(pageLength = 10))
    }else{
      datatable(examplepeakdatas(), options = list(pageLength = 10))
    }
  })
  #####
  nadataout<-reactive({
    if(input$loaddatatype==1){
      nadatax<-peaksdataout()#$yuanshidata
    }else{
      dataread<-examplepeakdatas()
      #if(input$datatypex==1){
      #  dataread1<-dataread[,-c(1,2)]
      #  dataread2<-dataread[,c(1,2)]
      #  #rowpaste<-apply(dataread2,1,function(x){
      #  #  paste0(x,collapse = "_")
      #  #})
      #  rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
      #  dataread1x<-dataread1[!duplicated(rowpaste),]
      #  rownames(dataread1x)<-rowpaste[!duplicated(rowpaste)]
      #}
      #else if(input$datatypex==2){
      #  datamaxqpro1<-dataread[-which(dataread$Reverse=="+" | dataread$`Potential contaminant`=="+"),]
      #  proidsstr<-strsplit(datamaxqpro1$`Protein IDs`,";")
      #  proids<- as.character(lapply(proidsstr,function(x) x[[1]][1]))
      #  datamaxqpro1$`Protein IDs`<-proids
      #  prolfdnames1<-grep("LFQ intensity",colnames(datamaxqpro1),value = TRUE)
      #  datamaxqpro2<-datamaxqpro1[,c("Protein IDs","Razor + unique peptides",prolfdnames1)]
      #  prolfdnames<-gsub("LFQ intensity ","",prolfdnames1)
      #  colnames(datamaxqpro2)<-c("Protein IDs","PeptidesNum",prolfdnames)
      #  datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
      #  datamaxqpro4<-datamaxqpro3[,-c(1,2)]
      #  dataread2<-datamaxqpro3[,c(1,2)]
      #  #rowpaste<-apply(dataread2,1,function(x){
      #  #  paste0(x,collapse = "_")
      #  #})
      #  rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
      #  rownames(datamaxqpro4)<-rowpaste#datamaxqpro3[,1]
      #  pro0index<-apply(datamaxqpro4,1,function(x) sum(x)==0)
      #  dataread1x<-datamaxqpro4[!pro0index,]
      #  dataread1x[dataread1x==0]<-NA
      #  dataread1x
      #}
      #else if(input$datatypex==3){
      #  datamaxqpro1<-dataread
      #  prolfdnames1<-grep("Abundances \\(Scaled\\): ",colnames(datamaxqpro1),value = TRUE)
      #  datamaxqpro2<-datamaxqpro1[,c("Accession","# Peptides",prolfdnames1)]
      #  prolfdnames<-gsub("Abundances \\(Scaled\\): ","",prolfdnames1)
      #  colnames(datamaxqpro2)<-c("Protein IDs","PeptidesNum",prolfdnames)
      #  datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
      #  datamaxqpro4<-as.data.frame(datamaxqpro3[,-c(1,2)])
      #  dataread2<-datamaxqpro3[,c(1,2)]
      #  #rowpaste<-apply(dataread2,1,function(x){
      #  #  paste0(x,collapse = "_")
      #  #})
      #  rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
      #  rownames(datamaxqpro4)<-rowpaste
      #  dataread1x<-datamaxqpro4
      #}
      #else{
      #  prolfdnames1<-grep("NrOfPrecursorsUsedForQuantification",colnames(dataread),value = TRUE)
      #  datamaxqpro1<-dataread[,prolfdnames1]
      #  datamaxqpro1[] <- lapply(datamaxqpro1, function(x) as.numeric(as.character(x)))
      #  PeptidesNum<-apply(datamaxqpro1,1,sum,na.rm=TRUE)
      #  proidsstr<-strsplit(dataread$PG.ProteinGroups,";")
      #  proids<- as.character(lapply(proidsstr,function(x) x[[1]][1]))
      #  prolfdnames1<-grep("PG\\.Quantity",colnames(dataread),value = TRUE)
      #  datamaxqpro2<-dataread[,c("PG.ProteinGroups",prolfdnames1)]
      #  prolfdnames<-gsub("\\[\\d+\\] |\\.raw\\.PG\\.Quantity","",prolfdnames1)
      #  colnames(datamaxqpro2)<-c("Protein IDs",prolfdnames)
      #  datamaxqpro2$`Protein IDs`<-proids
      #  datamaxqpro2$PeptidesNum<-PeptidesNum
      #  datamaxqpro2<-datamaxqpro2[,c(1,ncol(datamaxqpro2),2:(ncol(datamaxqpro2)-1))]
      #  datamaxqpro3<-datamaxqpro2[!duplicated(datamaxqpro2$`Protein IDs`),]
      #  datamaxqpro4<-datamaxqpro3[,-c(1,2)]
      #  dataread2<-datamaxqpro3[,c(1,2)]
      #  #rowpaste<-apply(dataread2,1,function(x){
      #  #  paste0(x,collapse = "_")
      #  #})
      #  rowpaste<-paste0(dataread2[[1]],"_",dataread2[[2]])
      #  rownames(datamaxqpro4)<-rowpaste
      #  dataread1x<-datamaxqpro4
      #}
      nadatax<-dataread#dataread1x
    }
    nadatax[nadatax==input$natype]<-NA
    nadatax[] <- lapply(nadatax, function(x) as.numeric(as.character(x)))
    nadatax
  })
  filtereddatadfout<-reactive({
    if(input$loaddatatype==1){
      grnames1<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      grnum2<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
      grnames<-rep(grnames1,times=grnum2)
    }else{
      grnames1<-strsplit(input$examgrnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
      grnum2<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
      grnames<-rep(grnames1,times=grnum2)
    }
    datadf<-nadataout()
    naratiox<-input$naratio
    if(input$fenzukaolvif){
      nastatsdf<-NULL
      nastatsdf0<-NULL
      for(i in 1:length(grnames1)){
        dataindex<-datadf[,grnames==grnames1[i]]
        narowsumix<-apply(dataindex,1,function(x){sum(is.na(x))})==ncol(dataindex)
        narowsumi<-apply(dataindex,1,function(x){sum(is.na(x))})/ncol(dataindex)
        dataindex1<-data.frame(nabili=narowsumi<=naratiox)
        nastatsdf<-cbind(nastatsdf,as.matrix(dataindex1))
        nastatsdf0<-cbind(nastatsdf0,as.matrix(dataindex))
      }
      nafenzuindex<-apply(nastatsdf,1,function(x){
        if(all(x)){
          return(TRUE)
        }else{
          return(FALSE)
        }
      })
      datadfchuli<-datadf[nafenzuindex,]
      datadfchuli<-as.data.frame(datadfchuli)
    }else{
      narowsum<-apply(datadf,1,function(x){sum(is.na(x))})/ncol(datadf)
      datadfchuli<-datadf[narowsum<=input$naratio,]
    }
    datadfchulix<-datadfchuli
    if(input$mediannormif){
      medianval<-apply(datadfchuli,2,function(x) {median(x,na.rm = TRUE)})
      datadfchuli<-sweep(datadfchuli,2,medianval,FUN = "/")
    }
    if(input$logif){
      datadfchuli<-log2(datadfchuli)
    }
    list(datadfchuli=datadfchuli,datadfchuli_nonorm=datadfchulix)
  })
  cvfilterdataout<-reactive({
    dfxx<-filtereddatadfout()$datadfchuli
    dfx1<-filtereddatadfout()$datadfchuli_nonorm
    if(input$mediannormif){
      medianval<-apply(dfx1,2,function(x) {median(x,na.rm = TRUE)})
      dfx<-sweep(dfx1,2,medianval,FUN = "/")
    }else{
      dfx<-dfx1
    }
    if(input$loaddatatype==1){
      grnames1<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      grnum2<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
      grnames<-rep(grnames1,times=grnum2)
    }else{
      grnames1<-strsplit(input$examgrnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
      grnum2<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
      grnames<-rep(grnames1,times=grnum2)
    }
    cvdf<-cvifdf<-NULL
    for(i in 1:length(grnames1)){
      datai<-dfx[,grnames==grnames1[i]]
      cvi<-apply(datai,1,function(x){
        if(all(as.numeric(x)==0)|is.na(all(as.numeric(x)==0))){
          0
        }else{
          raster::cv(as.numeric(x),na.rm = TRUE,aszero=TRUE)
        }
      })
      cvdf<-cbind(cvdf,cvi)
      cvifdf<-cbind(cvifdf,cvi>input$cvyuzhi*100)
    }
    cvifdf1<-apply(cvifdf,1,any)
    dfxx1<-dfxx[!cvifdf1,]
    dfxx00<-apply(dfxx1, 1, sum,na.rm = TRUE)
    dfxx2<-dfxx1[dfxx00!=0,]
    round(dfxx2,digits=5)
  })
  knnimputeresout<-reactive({
    library(impute)
    if(input$cvyuzhiif){
      df1<-cvfilterdataout()
    }else{
      df1<-filtereddatadfout()$datadfchuli
    }
    #df1<<-df1
    data_zero1<-impute.knn(as.matrix(df1),k = 10, rowmax = 1, colmax = 1)
    df<-data_zero1$data
    df
  })
  plot_missing_xiu<-function (data, title = NULL){
    feature <- num_missing <- pct_missing <- group <- NULL
    is_data_table <- is.data.table(data)
    data_class <- class(data)
    if (!is_data_table) {
      data <- data.table(data)
    }
    missing_value <- data.table(feature = names(data), num_missing = sapply(data,function(x) {sum(is.na(x))}))
    missing_value[, `:=`(feature, factor(feature, levels = feature[order(-rank(num_missing))]))]
    missing_value[, `:=`(pct_missing, num_missing/nrow(data))]
    missing_value[pct_missing < 0.2, `:=`(group, "Good")]
    missing_value[pct_missing >= 0.2 & pct_missing < 0.6, `:=`(group,"OK")]
    missing_value[pct_missing >= 0.6 & pct_missing < 0.8, `:=`(group,"Bad")]
    missing_value[pct_missing >= 0.8, `:=`(group, "Remove")]
    if (!is_data_table) {
      class(missing_value) <- data_class
    }
    output <- ggplot(missing_value, aes_string(x = "feature",y = "num_missing", fill = "group")) +
      geom_bar(stat = "identity",colour = "black", alpha = 0.7) +
      geom_text(aes(label = paste0(round(100 *pct_missing, 2), "%")), hjust = -0.15, size = 4) +
      scale_fill_manual("Group", values = c(Good = "#1FB04C",OK = "#9AD94F", Bad = "#FDA249", Remove = "#D71316"),
                        breaks = c("Good", "OK", "Bad", "Remove")) + coord_flip() +
      xlab("Features") + ylab("Number of missing rows") +
      ylim(c(0,max(missing_value$num_missing)+10))+
      ggtitle(title)
    output
  }
  preheightx<-reactive({
    input$preheight
  })
  prewidthx<-reactive({
    input$prewidth
  })
  observeEvent(
    input$mcsbtn_nafenbu,{
      shinyjs::show(id = "mcsbtn_nafenbu_hid", anim = FALSE)
      #output$nadatadf<-renderDataTable({
      #  datatable(nadataout(), options = list(pageLength = 20))
      #})
      #output$nadatadfdl<-downloadHandler(
      #  filename = function(){paste("NAdata_",usertimenum,".csv",sep="")},
      #  content = function(file){
      #    write.csv(nadataout(),file)
      #  }
      #)
      output$naplotbycolumn<-renderPlot({
        dfx<<-nadataout()
        plot_missing_xiu(dfx)+theme_bw()+
          theme(legend.position = c("bottom"),axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      },height = preheightx)
      naplotbycolumnout<-reactive({
        dfx<-nadataout()
        plot_missing_xiu(dfx)+theme_bw()+
          theme(legend.position = c("bottom"),axis.text=element_text(size=14),
                axis.title=element_text(size=16),legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
      })
      output$naplotbycolumndl<-downloadHandler(
        filename = function(){paste("NAplot_bycolumn_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = prewidthx()/100,height = preheightx()/100)
          print(naplotbycolumnout())
          dev.off()
        }
      )
      output$naplotbyrow<-renderPlot({
        library(Amelia)
        natypecolx<-strsplit("grey80;#3C5488FF",";|,")[[1]]#input$natypecol
        dfx<-nadataout()
        tdatamaxqproNA<-as.data.frame(t(dfx))
        colnames(tdatamaxqproNA)<-paste0("Row ",1:ncol(tdatamaxqproNA))
        missmap(tdatamaxqproNA,y.labels=rev(colnames(dfx)),x.cex = 0.5, col = natypecolx)
      },height = preheightx)
      naplotbyrowout<-reactive({
        natypecolx<-strsplit("grey80;#3C5488FF",";|,")[[1]]
        dfx<-nadataout()
        tdatamaxqproNA<-as.data.frame(t(dfx))
        colnames(tdatamaxqproNA)<-paste0("Row ",1:ncol(tdatamaxqproNA))
        missmap(tdatamaxqproNA,y.labels=rev(colnames(dfx)),x.cex = 0.5, col = natypecolx)
      })
      output$naplotbyrowdl<-downloadHandler(
        filename = function(){paste("NAplot_byrow_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = prewidthx()/100,height = preheightx()/100)
          print(naplotbyrowout())
          dev.off()
        }
      )
      ##
      output$knnimputeres<-renderDataTable({
        datatable(knnimputeresout(), options = list(pageLength = 20))
      })
      output$knnimputeresdl<-downloadHandler(
        filename = function(){paste("Data_KNNimputed_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(knnimputeresout(),file)
        }
      )
    }
  )
  ##############
  observeEvent(
    input$mcsbtn_entropyres,{
      shinyjs::show(id = "resultsxuanze_btn", anim = FALSE)
      #######Entropy
      clusteringdfout<-reactive({
        if(input$logif){
          protxt1<<-2^knnimputeresout()
        }else{
          protxt1<<-knnimputeresout()
        }
        mdatareadn<-t(scale(t(protxt1)))
        #mdatareadn<<-knnimputeresout()
        if(input$loaddatatype==1){
          sgnames<-strsplit(input$grnames,";")[[1]]
          samplegroupsx<-strsplit(input$grnums,";")[[1]]
        }else{
          sgnames<<-strsplit(input$examgrnames,";")[[1]]
          samplegroupsx<<-strsplit(input$examgrnums,";")[[1]]
        }
        NumCond<-as.numeric(samplegroupsx[1])
        NumRepsx1<-as.numeric(strsplit(samplegroupsx[2],"-")[[1]])
        NumRepsx<-c(0,cumsum(NumRepsx1))
        tdat<-NULL
        for (i in 1:NumCond){
          tdat<-cbind(tdat,rowMeans(mdatareadn[,(NumRepsx[i]+1):(NumRepsx[i+1]),drop=FALSE],na.rm=T))
        }
        colnames(tdat)<-paste0("Mean of ",sgnames)
        set.seed(123456)
        #kk<-ceiling(sqrt(nrow(tdat)))
        #tdat<<-tdat
        if(isolate(input$autoclusterif)){
          clusternumx<<-isolate(input$clusternum)
        }else{
          clusternumx<<-ceiling(sqrt(nrow(protxt1)))
        }
        cliris<-kmeans(tdat,centers=clusternumx)
        list(kmeansres=cliris,meandata=tdat)
      })
      clusteringdfout2<-reactive({
        clusteringdfoutx<<-clusteringdfout()
        kmeansresx<-clusteringdfoutx$kmeansres
        meandatax<-as.data.frame(clusteringdfoutx$meandata)
        meandatax$Names<-rownames(meandatax)
        meandatax$Cluster<-kmeansresx$cluster
        meandatax<-meandatax[,c(ncol(meandatax)-1,ncol(meandatax),1:(ncol(meandatax)-2))]
        rownames(meandatax)<-1:nrow(meandatax)
        meandatax
      })
      ##
      MEheatmapout<-reactive({
        if(input$logif){
          protxt1<<-2^knnimputeresout()
        }else{
          protxt1<<-knnimputeresout()
        }
        cliris<<-clusteringdfout()$kmeansres
        if(isolate(input$autoclusterif)){
          kk<<-isolate(input$clusternum)
        }else{
          kk<<-ceiling(sqrt(nrow(protxt1)))
        }
        #kk<<-isolate(input$clusternum)
        entropydf<-NULL
        for(i in 1:kk){
          protxti<-protxt1[rownames(protxt1)%in%names(cliris$cluster)[cliris$cluster==i],]
          #STEP 1: Establish the gene expressions matrix
          protxt2<-t(protxti)
          #STEP 2: Normalize the matrix
          protxt3_1<-sweep(protxt2,2,apply(protxt2,2,function(x){min(x)}),FUN = "-")
          protxt3<-sweep(protxt3_1,2,apply(protxt2,2,function(x){max(x)-min(x)}),FUN = "/")
          #STEP 3: Calculate the proportion of proteins
          protxt4<-sweep(protxt3,2,apply(protxt3,2,function(x){sum(x)}),FUN = "/")
          #STEP 4: Calculate the entropy of proteins
          protxt5<- -(1/log(nrow(protxt4)))*apply(protxt4,2,function(x){sum(x*log(x),na.rm = T)})
          #STEP 5: Calculate information utility value
          protxt6<-1-protxt5
          #STEP 6: Calculate the weight of proteins
          protxt7<-protxt6/sum(protxt6)
          #STEP 7: Calculate the activity of a pathway based on the expressions and weights of all proteins within it
          protxt8<-data.frame(EE=apply(sweep(protxt2,2,protxt7,FUN = "*"),1,sum,na.rm=T))
          #
          if(i==1){
            entropydf<-protxt8
          }else{
            entropydf<-cbind(entropydf,protxt8)
          }
        }
        dim(entropydf)
        entropydf1<-t(entropydf)
        rownames(entropydf1)<-paste0("Cluster ",1:kk)
        entropydf1
      })
      EWdfout<-reactive({
        if(input$logif){
          protxt1<<-2^knnimputeresout()
        }else{
          protxt1<<-knnimputeresout()
        }
        #STEP 1: Establish the gene expressions matrix
        protxt2<-t(protxt1)
        #STEP 2: Normalize the matrix
        protxt3_1<-sweep(protxt2,2,apply(protxt2,2,function(x){min(x)}),FUN = "-")
        protxt3<-sweep(protxt3_1,2,apply(protxt2,2,function(x){max(x)-min(x)}),FUN = "/")
        #STEP 3: Calculate the proportion of proteins
        protxt4<-sweep(protxt3,2,apply(protxt3,2,function(x){sum(x)}),FUN = "/")
        #STEP 4: Calculate the entropy of proteins
        protxt5<- -(1/log(nrow(protxt4)))*apply(protxt4,2,function(x){sum(x*log(x),na.rm = T)})
        #STEP 5: Calculate information utility value
        protxt6<-1-protxt5
        #STEP 6: Calculate the weight of proteins
        protxt7<-protxt6/sum(protxt6)
        #STEP 7: Calculate the activity of a pathway based on the expressions and weights of all proteins within it
        protxt8<-t(sweep(protxt2,2,protxt7,FUN = "*"))*10^5
        protxt8
      })
      hubnetworkpdataout<-reactive({
        library(MEGENA)
        if(isolate(input$targetMif)){
          mubiaomodule<<-as.numeric(strsplit(isolate(input$targetModule),";")[[1]])
        }else{
          if(isolate(input$autoclusterif)){
            mubiaomodule<<-1:isolate(input$clusternum)
          }else{
            mubiaomodule<<-1:ceiling(sqrt(nrow(protxt1)))
          }
        }
        if(input$logif){
          protxt1<<-2^knnimputeresout()
        }else{
          protxt1<<-knnimputeresout()
        }
        cliris<<-clusteringdfout()$kmeansres
        #EWdfoutx<<-EWdfout()
        mdatareadn<-t(scale(t(protxt1)))
        MEGENA.output.list<-MEGENA.hubs.list<-list()
        withProgress(message = 'Hubs finding...', style = "notification", detail = "", value = 0,{
          for(i in 1:length(mubiaomodule)){
            datExprxi<-mdatareadn[rownames(mdatareadn)%in%names(cliris$cluster)[cliris$cluster==mubiaomodule[i]],]
            ijwi <- calculate.correlation(datExprxi,doPerm=10)
            eli <- calculate.PFN(ijwi[,1:3])
            gi <- graph.data.frame(eli,directed = FALSE)
            set.seed(123456)
            MEGENA.outputi <- do.MEGENA(g = gi,remove.unsig = FALSE,doPar = FALSE,n.perm = 100,
                                        mod.pval = 0.1,hub.pval = 0.1,min.size = 5,max.size = 2500)
            output.summaryi <- MEGENA.ModuleSummary(MEGENA.outputi,
                                                    mod.pvalue = 0.1,hub.pvalue = 0.1,
                                                    min.size = 5,max.size = 2500,
                                                    annot.table = NULL,id.col = NULL,symbol.col = NULL,
                                                    output.sig = TRUE)
            MEGENA.output.list[[i]]<-output.summaryi
            modulehubsi<-output.summaryi$module.table$module.hub
            modulehubsi1<-modulehubsi[modulehubsi!="()"]
            modulehubsi2<-unique(unlist(lapply(modulehubsi1,function(x){
              x1<-strsplit(x,",")[[1]]
              x2<-gsub("\\(\\d+\\)","",x1)
              x2
            })))
            MEGENA.hubs.list[[i]]<-modulehubsi2
            incProgress(1/length(mubiaomodule), detail = paste0("Cluster ",mubiaomodule[i]," done!"))
          }
        })
        names(MEGENA.output.list)<-names(MEGENA.hubs.list)<-paste0("M",mubiaomodule)
        list(MEGENA.output.list=MEGENA.output.list,MEGENA.hubs.list=MEGENA.hubs.list)
      })
      hubnetworkdfout<-reactive({
        if(input$logif){
          protxt1<<-2^knnimputeresout()
        }else{
          protxt1<<-knnimputeresout()
        }
        EWdfoutx<<-EWdfout()
        MEGENA.listx<<-hubnetworkpdataout()
        MEGENA.hubs.listx<-unique(unlist(MEGENA.listx$MEGENA.hubs.list))
        #datExprxi<-protxt1[rownames(protxt1)%in%unique(MEGENA.hubs.listx),]
        datExprxi<-EWdfoutx[rownames(EWdfoutx)%in%unique(MEGENA.hubs.listx),]
        datExprxi
      })
      hubpathwaypdfout1<-reactive({
        library(clusterProfiler)
        hubnetworkdfoutx<<-hubnetworkdfout()
        withProgress(message = 'Hubs function entichment...', style = "notification", detail = "", value = 0,{
          incProgress(1/2, detail = "")
          kegg_results_ALL<-enrichKEGG(gene= unique(rownames(hubnetworkdfoutx)),organism=isolate(input$wuzhongkegg),
                                       keyType = 'uniprot',pvalueCutoff = 1,
                                       qvalueCutoff=1,minGSSize = 1,maxGSSize=50000)
        })
        kegg_results_ALL
      })
      hubpathwaypdfout<-reactive({
        hubpathwaypdfout1x<<-hubpathwaypdfout1()
        kegg_results_ALL_df<-hubpathwaypdfout1x@result
        kegg_results_ALL_df$Description<-unlist(lapply(kegg_results_ALL_df$Description,function(x){
          strsplit(x," - ")[[1]][1]
        }))
        topicuploadgodf1<-kegg_results_ALL_df#[-grep(" disease$|Metabolic pathways",kegg_results_ALL_df$Description),]
        topicuploadgodf1$GeneRatio<-unlist(lapply(topicuploadgodf1$GeneRatio,function(x){
          x1<-strsplit(x,"\\/")[[1]]
          round(as.numeric(x1[1])/as.numeric(x1[2]),5)
        }))
        topicuploadgodf1<-topicuploadgodf1[order(topicuploadgodf1$GeneRatio,decreasing = T),c(1,2,9,3:8)]
        topicuploadgodf1
      })
      keggMEheatmapout<-reactive({
        if(input$logif){
          protxt1<<-2^knnimputeresout()
        }else{
          protxt1<<-knnimputeresout()
        }
        withProgress(message = 'Calculating KEGG pathway activity value matirx...', style = "notification", detail = "", value = 0,{
          incProgress(1/2, detail = "")
          keggdatabase<<-gson_KEGG(isolate(input$wuzhongkegg),KEGG_Type = "KEGG", keyType = "uniprot")
          keggdatabase1<-keggdatabase@gsid2gene
          keggdatabase2<-keggdatabase@gsid2name
          keggdatabase2$name<-unlist(lapply(keggdatabase2$name,function(x){
            strsplit(x," - [A-Z]")[[1]][1]
          }))
          load(file = "keggallpathwayclassdf.rdata")
          keggpathselectx<<-isolate(input$keggpathselect)
          keggallpathwayclassdf1<-keggallpathwayclassdf[keggallpathwayclassdf$Group1%in%keggpathselectx,]
          keggallpathwayclassdf1$PathIDs<-paste0(isolate(input$wuzhongkegg),keggallpathwayclassdf1$PathIDs)
          keggdatabase1x<-keggdatabase1[keggdatabase1$gsid%in%keggallpathwayclassdf1$PathIDs,]
          keggdatabase3<-keggdatabase1x[keggdatabase1x$gene%in%rownames(protxt1),]
          keggdatabase4<-table(keggdatabase3$gsid)[table(keggdatabase3$gsid)>10]
          entropydf<-NULL
          for(i in 1:length(names(keggdatabase4))){
            protxti<-protxt1[rownames(protxt1)%in%keggdatabase3$gene[keggdatabase3$gsid==names(keggdatabase4)[i]],]
            #STEP 1: Establish the gene expressions matrix
            protxt2<-t(protxti)
            #STEP 2: Normalize the matrix
            protxt3_1<-sweep(protxt2,2,apply(protxt2,2,function(x){min(x)}),FUN = "-")
            protxt3<-sweep(protxt3_1,2,apply(protxt2,2,function(x){max(x)-min(x)}),FUN = "/")
            #STEP 3: Calculate the proportion of proteins
            protxt4<-sweep(protxt3,2,apply(protxt3,2,function(x){sum(x)}),FUN = "/")
            #STEP 4: Calculate the entropy of proteins
            protxt5<- -(1/log(nrow(protxt4)))*apply(protxt4,2,function(x){sum(x*log(x),na.rm = T)})
            #STEP 5: Calculate information utility value
            protxt6<-1-protxt5
            #STEP 6: Calculate the weight of proteins
            protxt7<-protxt6/sum(protxt6)
            #STEP 7: Calculate the activity of a pathway based on the expressions and weights of all proteins within it
            protxt8<-data.frame(EE=apply(sweep(protxt2,2,protxt7,FUN = "*"),1,sum,na.rm=T))
            #
            if(i==1){
              entropydf<-protxt8
            }else{
              entropydf<-cbind(entropydf,protxt8)
            }
          }
          entropydf1<-as.data.frame(t(entropydf))
          keggdatabasenames<-keggdatabase2[unlist(lapply(names(keggdatabase4),function(x){
            which(keggdatabase2$gsid==x)
          })),]
          #rownames(entropydf1)<-keggdatabasenames$name
          entropydf1$Description<-keggdatabasenames$name
          entropydf1$ID<-keggdatabasenames$gsid
          entropydf2<-entropydf1[,c(ncol(entropydf1),(ncol(entropydf1)-1),1:(ncol(entropydf1)-2))]
        })
        entropydf2
      })
      kegghubsgrsaout<-reactive({
        library(ReporterScore)
        if(input$logif){
          protxt1<<-2^knnimputeresout()
        }else{
          protxt1<<-knnimputeresout()
        }
        withProgress(message = 'GRSA performing...', style = "notification", detail = "", value = 0,{
          incProgress(1/2, detail = "")
          if(input$loaddatatype==1){
            sgnames<-strsplit(input$grnames,";")[[1]]
            samplegroupsx<-strsplit(input$grnums,";")[[1]]
          }else{
            sgnames<<-strsplit(input$examgrnames,";")[[1]]
            samplegroupsx<<-strsplit(input$examgrnums,";")[[1]]
          }
          NumRepsx1<-as.numeric(strsplit(samplegroupsx[2],"-")[[1]])
          #hubnetworkdfoutx<<-hubnetworkdfout()
          EWdfoutx<<-EWdfout()
          metadata<-data.frame(Group=rep(sgnames,NumRepsx1))
          metadata$Group<-factor(metadata$Group,levels = sgnames)
          rownames(metadata)<-colnames(EWdfoutx)
          keggdatabase<<-gson_KEGG(isolate(input$wuzhongkegg),KEGG_Type = "KEGG", keyType = "uniprot")
          keggdatabase1<-keggdatabase@gsid2gene
          keggdatabase2<-keggdatabase@gsid2name
          keggdatabase2$name<-unlist(lapply(keggdatabase2$name,function(x){
            strsplit(x," - [A-Z]")[[1]][1]
          }))
          load(file = "keggallpathwayclassdf.rdata")
          keggpathselectx<<-isolate(input$keggpathselect)
          keggallpathwayclassdf1<-keggallpathwayclassdf[keggallpathwayclassdf$Group1%in%keggpathselectx,]
          keggallpathwayclassdf1$PathIDs<-paste0(isolate(input$wuzhongkegg),keggallpathwayclassdf1$PathIDs)
          keggdatabase1x<-keggdatabase1[keggdatabase1$gsid%in%keggallpathwayclassdf1$PathIDs,]
          keggdatabase3<-keggdatabase1x[keggdatabase1x$gene%in%rownames(protxt1),]
          keggdatabase4<-table(keggdatabase3$gsid)[table(keggdatabase3$gsid)>10]
          custom_modulelistx<<-custom_modulelist(keggdatabase1[keggdatabase1$gsid%in%names(keggdatabase4),],
                                                keggdatabase2[keggdatabase2$gsid%in%names(keggdatabase4),])
          reporterhubs_res<-GRSA(EWdfoutx, "Group", metadata,mode = "directed",p.adjust.method1="BH",
                                 method = "spearman", perm = 999,modulelist = custom_modulelistx)
        })
        reporterhubs_res
      })
      keggMEheatmapdfout<-reactive({
        MEheatmapoutx<<-keggMEheatmapout()
        kegghubsgrsaoutx<<-kegghubsgrsaout()
        MEheatmapoutx1<-base::merge(MEheatmapoutx,kegghubsgrsaoutx$reporter_s[,c("ID","ReporterScore")],
                                    by="ID",sort=F)
        MEheatmapoutx1<-MEheatmapoutx1[order(MEheatmapoutx1$ReporterScore,decreasing = T),c(1,2,ncol(MEheatmapoutx1),3:(ncol(MEheatmapoutx1)-1))]
        MEheatmapoutx1
      })
      ###############################################
      output$clusteringdf<-renderDataTable({
        datatable(clusteringdfout2(), options = list(pageLength = 20))
      })
      output$clusteringdfdl<-downloadHandler(
        filename = function(){paste("Cluster.Table_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(clusteringdfout2(),file,row.names = F)
        }
      )
      clusteringumapout<-reactive({
        library(uwot)
        clusteringdfoutx<<-clusteringdfout()
        kmeansresx<-clusteringdfoutx$kmeansres
        meandatax<-as.data.frame(clusteringdfoutx$meandata)
        meandatax$Names<-rownames(meandatax)
        meandatax$Cluster<-kmeansresx$cluster
        meandatax<-meandatax[,c(ncol(meandatax)-1,ncol(meandatax),1:(ncol(meandatax)-2))]
        rownames(meandatax)<-1:nrow(meandatax)
        meandatax1<-meandatax[,-1]
        rownames(meandatax1)<-meandatax[,1]
        withProgress(message = 'Clustering...', style = "notification", detail = "", value = 0,{
          incProgress(1/2, detail = "")
          set.seed(123)
          umapres1<-uwot::umap(meandatax1[-1],n_neighbors=15,n_components=2)
          umapres2<-as.data.frame(umapres1)
          umapres2$Clusters<-factor(meandatax1[,1],levels = 1:length(unique(meandatax1[,1])))
        })
        umapres2
      })
      output$clusteringplot<-renderPlot({
        #library(factoextra)
        #kmeansresx<-clusteringdfoutx$kmeansres
        #meandatax<-clusteringdfoutx$meandata
        #fviz_cluster(kmeansresx, data = meandatax,
        #             palette = c(colpalettes[1:isolate(input$clusternum)]),
        #             geom = "point",shape=21,show.clust.cent=F,
        #             ellipse.type = "convex",
        #             ggtheme = theme_bw()
        #)+
        #  theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())
        umapres2x<<-clusteringumapout()
        ggplot(umapres2x,aes(x=V1,y=V2,col=Clusters))+
          geom_point(alpha=0.9,shape=19)+
          scale_color_manual(values=colpalettes[1:length(unique(umapres2x$Clusters))])+
          xlab("Umap 1")+ylab("Umap 2")+
          theme_bw()+
          theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())
      })
      clusteringplotout<-reactive({
        #library(factoextra)
        #kmeansresx<-clusteringdfoutx$kmeansres
        #meandatax<-clusteringdfoutx$meandata
        #fviz_cluster(kmeansresx, data = meandatax,
        #             palette = c(colpalettes[1:isolate(input$clusternum)]),
        #             geom = "point",shape=21,show.clust.cent=F,
        #             ellipse.type = "convex",
        #             ggtheme = theme_bw()
        #)+
        #  theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())
        umapres2x<<-clusteringumapout()
        ggplot(umapres2x,aes(x=V1,y=V2,col=Clusters))+
          geom_point(alpha=0.9,shape=19)+
          scale_color_manual(values=colpalettes[1:length(unique(umapres2x$Clusters))])+
          xlab("Umap 1")+ylab("Umap 2")+
          theme_bw()+
          theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())
      })
      output$clusteringplotdl<-downloadHandler(
        filename = function(){paste("Clustering.Plot_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = prewidthx()/100,height = preheightx()/100)
          print(clusteringplotout())
          dev.off()
        }
      )
      ##
      output$MEheatmap<-renderPlot({
        library(pheatmap)
        library(RColorBrewer)
        MEheatmapoutx<<-MEheatmapout()
        if(input$loaddatatype==1){
          sgnames<-strsplit(input$grnames,";")[[1]]
          samplegroupsx<-strsplit(input$grnums,";")[[1]]
        }else{
          sgnames<<-strsplit(input$examgrnames,";")[[1]]
          samplegroupsx<<-strsplit(input$examgrnums,";")[[1]]
        }
        NumCond<-as.numeric(samplegroupsx[1])
        NumRepsx1<-as.numeric(strsplit(samplegroupsx[2],"-")[[1]])
        timecolor<<-strsplit(isolate(input$groupcol),";")[[1]]
        heatmapcol<<-strsplit(isolate(input$heatmapcol),";")[[1]]
        annotation_col_hca = data.frame(
          Times = factor(rep(sgnames,NumRepsx1))
        )
        names(timecolor)<-sgnames
        ann_colors = list(
          Times=timecolor
        )
        rownames(annotation_col_hca) = colnames(MEheatmapoutx)
        pheatmap(MEheatmapoutx, scale = "row", cluster_rows = TRUE,cluster_cols = FALSE,
                 color = colorRampPalette(c(heatmapcol))(100),
                 annotation_col = annotation_col_hca,border_color=NA,annotation_colors = ann_colors,
                 clustering_distance_rows = "correlation",clustering_method = "ward.D2")
      })
      MEheatmapplotout<-reactive({
        library(pheatmap)
        library(RColorBrewer)
        MEheatmapoutx<<-MEheatmapout()
        if(input$loaddatatype==1){
          sgnames<-strsplit(input$grnames,";")[[1]]
          samplegroupsx<-strsplit(input$grnums,";")[[1]]
        }else{
          sgnames<<-strsplit(input$examgrnames,";")[[1]]
          samplegroupsx<<-strsplit(input$examgrnums,";")[[1]]
        }
        NumCond<-as.numeric(samplegroupsx[1])
        NumRepsx1<-as.numeric(strsplit(samplegroupsx[2],"-")[[1]])
        timecolor<<-strsplit(isolate(input$groupcol),";")[[1]]
        heatmapcol<<-strsplit(isolate(input$heatmapcol),";")[[1]]
        annotation_col_hca = data.frame(
          Times = factor(rep(sgnames,NumRepsx1))
        )
        names(timecolor)<-sgnames
        ann_colors = list(
          Times=timecolor
        )
        rownames(annotation_col_hca) = colnames(MEheatmapoutx)
        pheatmap(MEheatmapoutx, scale = "row", cluster_rows = TRUE,cluster_cols = FALSE,
                 color = colorRampPalette(c(heatmapcol))(100),
                 annotation_col = annotation_col_hca,border_color=NA,annotation_colors = ann_colors,
                 clustering_distance_rows = "correlation",clustering_method = "ward.D2")
      })
      output$MEheatmapdl<-downloadHandler(
        filename = function(){paste("ModuleEntropy.Heatmap_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = prewidthx()/100,height = preheightx()/100)
          print(MEheatmapplotout())
          dev.off()
        }
      )
      output$moduleresdf<-renderDataTable({
        datatable(MEheatmapout(), options = list(pageLength = 10))
      })
      output$moduleresdfdl<-downloadHandler(
        filename = function(){paste("Cluster.Activity.Value.Table_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(MEheatmapout(),file,row.names = T)
        }
      )
      output$entropyweightdf<-renderDataTable({
        EWdfoutx<<-EWdfout()
        datatable(EWdfout(), options = list(pageLength = 10))
      })
      output$entropyweightdfdl<-downloadHandler(
        filename = function(){paste("Entropy.Weight.Table_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(EWdfout(),file,row.names = T)
        }
      )
      #
      hubnetworkplistout<-reactive({
        if(input$logif){
          protxt1<<-2^knnimputeresout()
        }else{
          protxt1<<-knnimputeresout()
        }
        MEGENA.listx<<-hubnetworkpdataout()
        MEGENA.output.listx<-MEGENA.listx$MEGENA.output.list
        MEGENA.proteins<-unique(unlist(lapply(MEGENA.output.listx,function(x){
          unlist(x$modules)
        })))
        MEGENA.hubs.listx<-unique(unlist(MEGENA.listx$MEGENA.hubs.list))
        MEGENA.nohubs<-setdiff(MEGENA.proteins,MEGENA.hubs.listx)
        mdatareadn<-t(scale(t(protxt1)))
        datExprxi<-mdatareadn[rownames(mdatareadn)%in%unique(c(MEGENA.proteins,MEGENA.hubs.listx)),]
        #ijwi <- calculate.correlation(datExprxi,doPerm=10)
        FDR.cutoff<-0.05
        cor.output <- MEGENA:::test.pairwiseCor(datExprxi, method = "pearson")
        vertex.names <- cor.output$row.names
        edgelist <- cor.output[[1]]
        edgelist <- cbind(edgelist, fdr.q.value = p.adjust(edgelist$p.value,"fdr"))
        edgelist <- edgelist[which(edgelist$fdr.q.value < FDR.cutoff),]
        edgelist <- edgelist[order(edgelist[, 3], decreasing = T),]
        ijwi <- data.frame(row = vertex.names[edgelist[[1]]],
                           col = vertex.names[edgelist[[2]]],
                           as.data.frame(edgelist[,3:ncol(edgelist)]))
        withProgress(message = 'Network constructing (a bit time-consuming) ...', style = "notification", detail = "", value = 0,{
          incProgress(1/2, detail = "")
          eli <- calculate.PFN(ijwi[,1:3])
        })
        list(eli=eli,MEGENA.hubs.listx=MEGENA.hubs.listx)
      })
      output$hubnetworkp<-renderPlot({
        eli<<-hubnetworkplistout()$eli
        MEGENA.hubs.listx<<-hubnetworkplistout()$MEGENA.hubs.listx
        gi <- graph.data.frame(eli,directed = FALSE)
        gdegreei<-igraph::degree(gi)+1
        qgraph::qgraph(eli,directed=F,layout = "spring",#
                       groups = list(Hubs=which(unique(c(eli$row,eli$col))%in%MEGENA.hubs.listx),
                                     Neighbors=c(1:length(unique(c(eli$row,eli$col))))[-which(unique(c(eli$row,eli$col))%in%MEGENA.hubs.listx)]),
                       color = c("#E64B35FF","#3B4992FF"),label.color="black",vsize=log(gdegreei),
                       labels=F,label.cex=2,borders=F,edge.width=0.5,edge.color="grey80")
      })
      hubnetworkplotout<-reactive({
        eli<<-hubnetworkplistout()$eli
        MEGENA.hubs.listx<-hubnetworkplistout()$MEGENA.hubs.listx
        gi <- graph.data.frame(eli,directed = FALSE)
        gdegreei<-igraph::degree(gi)+1
        qgraph::qgraph(eli,directed=F,layout = "spring",#
                       groups = list(Hubs=which(unique(c(eli$row,eli$col))%in%MEGENA.hubs.listx),
                                     Neighbors=c(1:length(unique(c(eli$row,eli$col))))[-which(unique(c(eli$row,eli$col))%in%MEGENA.hubs.listx)]),
                       color = c("#E64B35FF","#3B4992FF"),label.color="black",vsize=log(gdegreei),
                       labels=F,label.cex=2,borders=F,edge.width=0.5,edge.color="grey80")
      })
      output$hubnetworkpdl<-downloadHandler(
        filename = function(){paste("Hubs.Network.Plot_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = prewidthx()/100,height = preheightx()/100)
          print(hubnetworkplotout())
          dev.off()
        }
      )
      output$hubnetworkdf<-renderDataTable({
        datatable(hubnetworkdfout(), options = list(pageLength = 10))
      })
      output$hubnetworkdfdl<-downloadHandler(
        filename = function(){paste("Hubs.Expression.Table_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(hubnetworkdfout(),file,row.names = T)
        }
      )
      output$hubpathwayp<-renderPlot({
        hubpathwaypdfoutx<<-hubpathwaypdfout()
        topicuploadgodf2<-hubpathwaypdfoutx[1:20,]
        topicuploadgodf2$index<-1:nrow(topicuploadgodf2)
        ggplot() +
          ggforce::geom_link(data = topicuploadgodf2, aes(x = 0, y = reorder(Description,GeneRatio),
                                                          xend =GeneRatio,yend=Description,
                                                          size = after_stat(index),
                                                          alpha = after_stat(index),
                                                          color = pvalue),show.legend = F) +
          geom_point(data = topicuploadgodf2, aes(x = GeneRatio, y = Description, color = pvalue), size = 6, shape = 21, fill = "white")+
          #facet_grid(Ontology~., scales="free")+
          scale_color_gradient(low="red", high="blue")+#scale_size(range = c(1, 3))+
          xlab("GeneRatio")+
          ylab("KEGG pathways")+
          theme_test()+
          theme(axis.title = element_text(size = 18),
                axis.text = element_text(size = 15))
      })
      hubpathwaypout<-reactive({
        hubpathwaypdfoutx<<-hubpathwaypdfout()
        topicuploadgodf2<-hubpathwaypdfoutx[1:20,]
        topicuploadgodf2$index<-1:nrow(topicuploadgodf2)
        ggplot() +
          ggforce::geom_link(data = topicuploadgodf2, aes(x = 0, y = reorder(Description,GeneRatio),
                                                          xend =GeneRatio,yend=Description,
                                                          size = after_stat(index),
                                                          alpha = after_stat(index),
                                                          color = pvalue),show.legend = F) +
          geom_point(data = topicuploadgodf2, aes(x = GeneRatio, y = Description, color = pvalue), size = 6, shape = 21, fill = "white")+
          #facet_grid(Ontology~., scales="free")+
          scale_color_gradient(low="red", high="blue")+#scale_size(range = c(1, 3))+
          xlab("GeneRatio")+
          ylab("KEGG pathways")+
          theme_test()+
          theme(axis.title = element_text(size = 18),
                axis.text = element_text(size = 15))
      })
      output$hubpathwaypdl<-downloadHandler(
        filename = function(){paste("Hubs.Pathway.Plot_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = prewidthx()/100,height = preheightx()/100)
          print(hubpathwaypout())
          dev.off()
        }
      )
      output$hubpathwaydf<-renderDataTable({
        datareadn<<-hubpathwaypdfout()
        datareadn[[1]] <- paste0("<a href='http://www.genome.jp/dbget-bin/www_bget?pathway+",datareadn[[1]],"' target='_blank'>",datareadn[[1]],"</a>")
        datatable(datareadn,escape = FALSE,selection="single",class = "cell-border hover",
                  options = list(pageLength = 10,columnDefs = list(list(className = 'dt-center', targets = 0:2)),scrollX = TRUE))
      })
      output$hubpathwaydfdl<-downloadHandler(
        filename = function(){paste("Hubs.Pathway.Table_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(hubpathwaypdfout(),file,row.names = F)
        }
      )
      ##
      output$keggMEheatmap<-renderPlot({
        library(pheatmap)
        library(RColorBrewer)
        keggMEheatmapoutx<<-keggMEheatmapout()
        keggMEheatmapoutx1<-keggMEheatmapoutx[,-c(1,2)]
        rownames(keggMEheatmapoutx1)<-keggMEheatmapoutx[,2]
        if(input$loaddatatype==1){
          sgnames<-strsplit(input$grnames,";")[[1]]
          samplegroupsx<-strsplit(input$grnums,";")[[1]]
        }else{
          sgnames<<-strsplit(input$examgrnames,";")[[1]]
          samplegroupsx<<-strsplit(input$examgrnums,";")[[1]]
        }
        NumCond<-as.numeric(samplegroupsx[1])
        NumRepsx1<-as.numeric(strsplit(samplegroupsx[2],"-")[[1]])
        timecolor<<-strsplit(isolate(input$groupcol),";")[[1]]
        heatmapcol<<-strsplit(isolate(input$heatmapcol),";")[[1]]
        annotation_col_hca = data.frame(
          Times = factor(rep(sgnames,NumRepsx1))
        )
        names(timecolor)<-sgnames
        ann_colors = list(
          Times=timecolor
        )
        rownames(annotation_col_hca) = colnames(keggMEheatmapoutx1)
        pheatmap(keggMEheatmapoutx1, scale = "row", cluster_rows = TRUE,cluster_cols = FALSE,
                 color = colorRampPalette(c(heatmapcol))(100),
                 annotation_col = annotation_col_hca,border_color=NA,annotation_colors = ann_colors,
                 clustering_distance_rows = "correlation",clustering_method = "ward.D2")
      })
      keggMEheatmapplotout<-reactive({
        library(pheatmap)
        library(RColorBrewer)
        keggMEheatmapoutx<<-keggMEheatmapout()
        keggMEheatmapoutx1<-keggMEheatmapoutx[,-c(1,2)]
        rownames(keggMEheatmapoutx1)<-keggMEheatmapoutx[,2]
        if(input$loaddatatype==1){
          sgnames<-strsplit(input$grnames,";")[[1]]
          samplegroupsx<-strsplit(input$grnums,";")[[1]]
        }else{
          sgnames<<-strsplit(input$examgrnames,";")[[1]]
          samplegroupsx<<-strsplit(input$examgrnums,";")[[1]]
        }
        NumCond<-as.numeric(samplegroupsx[1])
        NumRepsx1<-as.numeric(strsplit(samplegroupsx[2],"-")[[1]])
        timecolor<<-strsplit(isolate(input$groupcol),";")[[1]]
        heatmapcol<<-strsplit(isolate(input$heatmapcol),";")[[1]]
        annotation_col_hca = data.frame(
          Times = factor(rep(sgnames,NumRepsx1))
        )
        names(timecolor)<-sgnames
        ann_colors = list(
          Times=timecolor
        )
        rownames(annotation_col_hca) = colnames(keggMEheatmapoutx1)
        pheatmap(keggMEheatmapoutx1, scale = "row", cluster_rows = TRUE,cluster_cols = FALSE,
                 color = colorRampPalette(c(heatmapcol))(100),
                 annotation_col = annotation_col_hca,border_color=NA,annotation_colors = ann_colors,
                 clustering_distance_rows = "correlation",clustering_method = "ward.D2")
      })
      output$keggMEheatmapdl<-downloadHandler(
        filename = function(){paste("KEGG.Pathway.Entropy.Heatmap_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = prewidthx()/100,height = preheightx()/100)
          print(keggMEheatmapplotout())
          dev.off()
        }
      )
      output$kegggrsabar<-renderPlot({
        kegghubsgrsaoutx<<-kegghubsgrsaout()
        plot_report_bar(kegghubsgrsaoutx,rs_threshold=input$ReporterScoreyz)
      })
      kegggrsabarout<-reactive({
        kegghubsgrsaoutx<-kegghubsgrsaout()
        plot_report_bar(kegghubsgrsaoutx,rs_threshold=input$ReporterScoreyz)
      })
      output$kegggrsabardl<-downloadHandler(
        filename = function(){paste("KEGG.Pathway.GRSA.barplots_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = prewidthx()/100+2,height = preheightx()/100+1)
          print(kegggrsabarout())
          dev.off()
        }
      )
      output$kegggrsanetwork<-renderPlot({
        kegghubsgrsaoutx<<-kegghubsgrsaout()
        kegghubsgrsaoutx1<-kegghubsgrsaoutx
        kegghubsgrsaoutx1$reporter_s$ID<-kegghubsgrsaoutx$reporter_s$Description
        kegghubsgrsaoutx1$modulelist$id<-kegghubsgrsaoutx$modulelist$Description
        map_idx<<-strsplit(input$map_idx,";")[[1]]
        map_idx1<-kegghubsgrsaoutx$modulelist[kegghubsgrsaoutx$modulelist$id%in%map_idx,]
        plot_KOs_network(kegghubsgrsaoutx1,map_id = map_idx1$Description,main = "", mark_module = TRUE,
                         legend_position=c(left_leg_x = -1.3,left_leg_y = 1,right_leg_x = 1.2,right_leg_y=1),
                         vertex_size_range=list(Pathway=c(12,15),KOs=c(5,5)),
                         legend_cex=1.5)
        #reporter_sx<-kegghubsgrsaoutx$reporter_s
        #plotnetworkx<-plot_KOs_network(kegghubsgrsaoutx,map_id = map_idx,
        #                               main = "", mark_module = TRUE,return_net = T)
        #tmp_v2 <- MetaNet::get_v(plotnetworkx)
        #tmp_v3 <- dplyr::left_join(tmp_v2, reporter_sx, by = c(name = "ID"))
        #modules <- dplyr::group_by(tmp_v3, module) %>%
        #  dplyr::summarise(RS = mean(ReporterScore,na.rm = TRUE)) %>%
        #  as.data.frame()
        #kos_color<-c(Depleted = "seagreen", Enriched = "orange", None = "grey",
        #             Significant ="red2", Pathway = "#80b1d3")
        #modules$color <- ifelse(modules$RS > 1.64, kos_color["Enriched"],
        #                        ifelse(modules$RS < (-1.64), kos_color["Depleted"],
        #                               kos_color["None"]))
        #modules_col <- setNames(modules$color, modules$module)
        ##plotnetworkx1<-MetaNet:::get_v(plotnetworkx)
        #map_idx1<-tmp_v3$name[1:length(map_idx)]
        #module_labelx<-reporter_sx$Description[unlist(lapply(map_idx1,function(x){
        #  which(reporter_sx$ID==x)
        #}))]
        #module_coors<-MetaNet:::get_module_coors(plotnetworkx,module_label_just=c(0,1))
        #module_coors$module<-module_labelx
        #module_label_cex<-1.2
        #module_label_color<-modules_col#"black"
        #n_module <- nrow(module_coors)
        #for (i in seq_len(n_module)) {
        #  text(x = module_coors[i, "X"], y = module_coors[i,"Y"],
        #       labels = module_coors[i, "module"], cex = module_label_cex,
        #       col = module_label_color[i])
        #}
      })
      kegggrsanetworkout<-reactive({
        kegghubsgrsaoutx<<-kegghubsgrsaout()
        kegghubsgrsaoutx1<-kegghubsgrsaoutx
        kegghubsgrsaoutx1$reporter_s$ID<-kegghubsgrsaoutx$reporter_s$Description
        kegghubsgrsaoutx1$modulelist$id<-kegghubsgrsaoutx$modulelist$Description
        map_idx<<-strsplit(input$map_idx,";")[[1]]
        map_idx1<-kegghubsgrsaoutx$modulelist[kegghubsgrsaoutx$modulelist$id%in%map_idx,]
        plot_KOs_network(kegghubsgrsaoutx1,map_id = map_idx1$Description,main = "", mark_module = TRUE,
                         legend_position=c(left_leg_x = -1.3,left_leg_y = 1,right_leg_x = 1.2,right_leg_y=1),
                         vertex_size_range=list(Pathway=c(12,15),KOs=c(5,5)),
                         legend_cex=1.5)
        #reporter_sx<-kegghubsgrsaoutx$reporter_s
        #plotnetworkx<-plot_KOs_network(kegghubsgrsaoutx,map_id = map_idx,
        #                               main = "", mark_module = TRUE,return_net = T)
        #tmp_v2 <- MetaNet::get_v(plotnetworkx)
        #tmp_v3 <- dplyr::left_join(tmp_v2, reporter_sx, by = c(name = "ID"))
        #modules <- dplyr::group_by(tmp_v3, module) %>%
        #  dplyr::summarise(RS = mean(ReporterScore,na.rm = TRUE)) %>%
        #  as.data.frame()
        #kos_color<-c(Depleted = "seagreen", Enriched = "orange", None = "grey",
        #             Significant ="red2", Pathway = "#80b1d3")
        #modules$color <- ifelse(modules$RS > 1.64, kos_color["Enriched"],
        #                        ifelse(modules$RS < (-1.64), kos_color["Depleted"],
        #                               kos_color["None"]))
        #modules_col <- setNames(modules$color, modules$module)
        ##plotnetworkx1<-MetaNet:::get_v(plotnetworkx)
        #map_idx1<-tmp_v3$name[1:length(map_idx)]
        #module_labelx<-reporter_sx$Description[unlist(lapply(map_idx1,function(x){
        #  which(reporter_sx$ID==x)
        #}))]
        #module_coors<-MetaNet:::get_module_coors(plotnetworkx,module_label_just=c(0,1))
        #module_coors$module<-module_labelx
        #module_label_cex<-1.2
        #module_label_color<-modules_col#"black"
        #n_module <- nrow(module_coors)
        #for (i in seq_len(n_module)) {
        #  text(x = module_coors[i, "X"], y = module_coors[i,"Y"],
        #       labels = module_coors[i, "module"], cex = module_label_cex,
        #       col = module_label_color[i])
        #}
      })
      output$kegggrsanetworkdl<-downloadHandler(
        filename = function(){paste("KEGG.Pathway.GRSA.network_",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = prewidthx()/100+2,height = preheightx()/100+2)
          print(kegggrsanetworkout())
          dev.off()
        }
      )
      output$keggmoduleresdf<-renderDataTable({
        datatable(keggMEheatmapdfout(), options = list(pageLength = 10))
      })
      output$keggmoduleresdfdl<-downloadHandler(
        filename = function(){paste("KEGG.Pathway.Activity.Value.ReporterScore.Table_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(keggMEheatmapdfout(),file,row.names = F)
        }
      )
      kegggrsaprodfout<-reactive({
        kegghubsgrsaoutx<<-kegghubsgrsaout()
        kegghubsgrsaoutx1<-kegghubsgrsaoutx$modulelist
        kegghubsgrsaoutx2<-kegghubsgrsaoutx$ko_stat[,c("KO_id","cor","p.value","p.adjust","Z_score","type")]#,"Significantly"
        kegghubsgrsaoutx3<-tidyr::separate_rows(kegghubsgrsaoutx1,3,sep=",")[,c(1,4,3)]
        kegghubsgrsaoutx4<-base::merge(kegghubsgrsaoutx3,kegghubsgrsaoutx2,by.x="KOs",by.y="KO_id",sort=F)
        kegghubsgrsaoutx4
      })
      output$kegggrsaprodf<-renderDataTable({
        datatable(kegggrsaprodfout(), options = list(pageLength = 10))
      })
      output$kegggrsaprodfdl<-downloadHandler(
        filename = function(){paste("KEGG.Pathway.Protein.Statistics.Table_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(kegggrsaprodfout(),file,row.names = F)
        }
      )

    }
  )




})

shinyApp(ui = ui, server = server)
