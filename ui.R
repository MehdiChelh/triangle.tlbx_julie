
library(shiny)
library(shinydashboard)
library(ChainLadder)
library(ChainLadder)
library(copula)         # load the copula package
library(distr)          # distribution library
library(scatterplot3d)  # scatterplot3d - not always needed
library(matrixcalc)
#library(actuar)        # removed this as seemed to cause problem with ChainLadder - not sure why, as worked before!
library(shinythemes)
library(shinyLP)
library(shinyBS)
library(shinyjs)
library(scales)         # used for adding commas as separators for numbers
library(DT)             # for fancy datatables
library(ggplot2)        # for good graphs
library(markdown)       # for good graphs
library(plyr)
library(rhandsontable)
library(highcharter)
library(plotly)
library(shinyWidgets)
library(formattable)


Triangle_options<-c("UKMotor",
                    "MW2008",
                    "RAA", 
                    "MW2014",
                    "GenIns")



#Special CSS for loading page.
appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

header <- dashboardHeader(disable=TRUE)
# sidebar <- dashboardSidebar(disable=TRUE)


header_vMJ <- dashboardHeader(title = "R&D Shiny",disable=TRUE)  
sidebar_vMJ <- dashboardSidebar(  disable = TRUE, sidebarMenu(    #m#enuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),  #,href = "https://davidjhindley.com/shiny/claimsreserving/"
  menuItem("GIRA Ã l'origine...", icon = icon("send",lib='glyphicon'),         
           href = "https://davidjhindley.com/shiny/claimsreserving/")  ))
jsResetCode <- "shinyjs.resetapp = function() {history.go(0);}" # Define the js method that resets the page
jscloseWindowCode<-"shinyjs.closeWindow = function() { window.close(); }" # define the js method that closes window (doesn't seem to work in Chrome or Safari though)



####################################################################### Body         ###########################################################
body<-dashboardBody(fluidPage
                    (theme=shinytheme("lumen"),
                      useShinyjs(),
                      inlineCSS(appCSS),
                      
                      inlineCSS("table.dataTable tbody td.selected {
                                color: white !important;
                                background-color: #8B0000 !important;
                                }"),
                       
                      # Loading message
                      div(
                        id = "loading-content",
                        h2("Loading...")
                      ),
                      
                      tags$style(type="text/css", "body {padding-top: 50px;}"),
                      tags$style(HTML("
                                      
                                      
                                      .box.box-solid.box-primary>.box-header {
                                      color:#fff;
                                      background:#bdc3c7
                                      }
                                      
                                      .box.box-solid.box-primary{
                                      border-bottom-color:#bdc3c7;
                                      border-left-color:#bdc3c7;
                                      border-right-color:#bdc3c7;
                                      border-top-color:#bdc3c7;
                                      }
                                      ")),
                      tags$head(HTML('<link rel="icon", href="favicon-line-chart.ico"/>')),
                      div(style="padding: 1px 0px; width: '100%'",
                          titlePanel(title="", windowTitle="R&D Shiny")
                      ),
                      navbarPage("",position="fixed-top",collapsible = TRUE,
                                 
                                 ####################################################################### CL           ###################################################################                             
                                 #close bsModal
                                 tabPanel("Chain Ladder",icon=icon("menu-hamburger",lib="glyphicon"),
                                          fluidPage(
                                            fluidRow(
                                              column(2,
                                                     wellPanel(id="CL_inputpanel",    #main wellPanel for assumptions
                                                               wellPanel(
                                                                 
                                                                 source("DatatriangleinputCL.R"),
                                                                 
                                                                 selectInput("unitselect",
                                                                             "Unité d'affichage des montants:",
                                                                             c("Unités"=1,"Centaines"=100,"Milliers"=1000,"Millions"=1000000,"Milliards"=1000000000),selected=1)
                                                               ),
                                                               wellPanel(
                                                                 materialSwitch(inputId="Vision_globale_montants", label="Visuel des montants", status = "warning", right = TRUE),
                                                                 conditionalPanel(
                                                                   condition='input.Vision_globale_montants',
                                                                   selectInput('color_type','Choix de visualisation',
                                                                               c('Nivellation' = 'bar',
                                                                                 'Gradient de couleurs'='gradient'))),
                                                                 materialSwitch(inputId="TailFactor", label="Tail Factor", status = "warning", right = TRUE)
                                                                 
                                                                 
                                                               ),
                                                               wellPanel(
                                                                 
                                                                 h2('IBNR total'),
                                                                 fluidRow( 
                                                                   column(2,
                                                                          valueBoxOutput("Tot_IBNR")
                                                                   ))
                                                                 # hr(),
                                                                 # valueBoxOutput("T                                                             t_IBNR")
                                                               )
                                                               # fluidRow(actionButton("ResetCLinputs","Reset inputs",
                                                               #                       icon = icon("refresh")))
                                                     ) # close overall assumption wellPanel
                                              ),# close first three columns
                                              
                                              column(10,
                                                     tabBox(width=10,height='2100px',
                                                            tabPanel("Traitement des coefficients",
                                                                     h4("Triangle initial") ,
                                                                     uiOutput("ui"),#,height=600, width=12 
                                                                     hr(),
                                                                     h4("Link ratios et estimateurs") ,
                                                                     DT::dataTableOutput("Linkratios1") , #, height=600, width=12
                                                                     # h5("Link ratios et estimateurs"),
                                                                     # DT::dataTableOutput("Linkratios1"),
                                                                     # hr(),
                                                                     hr(),
                                                                     fluidRow(
                                                                       #column(5,
                                                                       # box(    title = "Charges et link ratios"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                       #         highchartOutput("devCoef"),width=5 ,status="primary"),
                                                                       # # ),
                                                                       column(12,
                                                                              box(    title ="Visualisation des exclusions - moyenne des LR retenus",status = "primary"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                                      plotlyOutput("devCoefChosen"),width=12))),
                                                                     # )
                                                                     #), #, height = 500
                                                                     hr(),
                                                                     fluidRow(
                                                                       #rHandsontableOutput("User_Entry"),
                                                                       column(12,
                                                                              box(    title = "Choix utilisateur"   ,status = "primary"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                                      DTOutput("User_Entry"),width=12   )),
                                                                       hr(),
                                                                       column(12,
                                                                              box(    title = "Paramètres de sauvegarde"   ,status = "primary"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                                      actionButton('buttons', 'Sauvegardez vos sélections avant de remonter vers vos suppressions',width='990px'),width=12   )),
                                                                       hr(),
                                                                       column(12,
                                                                              box(    title = "Personnalisation"   ,status = "primary"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                                      DT::dataTableOutput("LinkratiosCustom"),width=12   ))), #, height=600, width=12
                                                                     
                                                                     hr(),
                                                                     fluidRow(
                                                                       column(12,
                                                                              box(    title ="Coefficients retenus",status = "primary"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                                      DT::dataTableOutput("Retenus"),width=12  )))#height = 50 
                                                                     
                                                                     
                                                                     
                                                            ),
                                                            tabPanel("Résultats",
                                                                     fluidRow(
                                                                       column(12,
                                                                              h4("Triangle projeté"),
                                                                              DT::dataTableOutput("CL_triproj")),
                                                                       hr(),
                                                                       column(12,
                                                                              h4("En cas de projection de triangle de réglements..."),
                                                                              materialSwitch(inputId="d_d", label="Intégrer les provisions dossiers/dossiers", status = "info", right = TRUE)),
                                                                       
                                                                       hr(),
                                                                       column(12,
                                                                              h4("Résumé des résultats"),
                                                                              DT::dataTableOutput("CL_results1")))
                                                            )
                                                     ) #### end of tabsetPanel for results and graphs
                                                     
                                                     
                                              ) # end of column 9
                                              
                                              
                                            )# end of fluidRow
                                            
                                          )# end of fluidPage
                                 ),# end of TabPanel for main CL menu  
                                 
                                 ####################################################################### CL Bootstrap ###################################################################                             
                                 
                                 tabPanel("CL Bootstrap",icon=icon("random",lib="font-awesome"),
                                          fluidPage(
                                            fluidRow(
                                              column(2,
                                                     wellPanel(id="CLBoot_inputpanel",    #main wellPanel for assumptions
                                                               wellPanel(
                                                                 actionButton("Runboot", "Compiler le bootstrap")
                                                               ),
                                                               
                                                               
                                                               
                                                               wellPanel(
                                                                 materialSwitch(inputId="Switch_ChainLadder", label="Séparer du ChainLadder", status = "info", right = TRUE),
                                                                 conditionalPanel(
                                                                   condition='input.Switch_ChainLadder',
                                                                   selectInput("dataset_boot", label="Sélection de triangle",
                                                                               choices =Triangle_options)
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition='input.Switch_ChainLadder==false',
                                                                   materialSwitch(inputId="RecupCoef", label="Récupérer de coefficients", status = "info", right = TRUE)
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition='input.RecupCoef',
                                                                   materialSwitch(inputId="Diffusion_TF", label="Diffuser le tail factor", status = "info", right = TRUE)
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition='input.Switch_ChainLadder==false',
                                                                   materialSwitch(inputId="Recentrage", label="Recentrer sur le deterministe", status = "warning", right = TRUE),
                                                                   conditionalPanel(
                                                                     condition='input.Recentrage',
                                                                     awesomeRadio('methode',label='Méthode de recentrage',choices=c("Additive" = "additive",
                                                                                                                                     "Multiplicative"="multiplicative"),selected="additive")),
                                                                   conditionalPanel(
                                                                     condition='input.Recentrage',
                                                                     awesomeRadio("sans_TF","",choices=c("Avec Tail Factor"=0,"Sans Tail Factor"=1),selected=0)
                                                                   )
                                                                 )
                                                                 
                                                                 
                                                               ),
                                                               wellPanel(
                                                                 selectInput("unitselect_CLBoot","Unité d'affichage:",
                                                                             c("Unités"=1,"Centaines"=100,"Milliers"=1000,"Millions"=1000000,"Milliards"=1000000000),selected=1
                                                                 ),
                                                                 materialSwitch(inputId="Vision_globale_residus", label="Visuel des résidus", status = "info", right = TRUE),
                                                                 conditionalPanel(
                                                                   condition='input.Vision_globale_residus',
                                                                   selectInput('color_type_boot','Choix de visualisation',
                                                                               c('Nivellation' = 'bar_boot',
                                                                                 'Gradient de couleurs'='gradient_boot')))),
                                                               wellPanel(
                                                                 materialSwitch(inputId="Param", label="Paramétrique", status = "info", right = TRUE),
                                                                 conditionalPanel(
                                                                   condition='input.Param',
                                                                   awesomeRadio("bootprocessdist", "Distribution des résidus", 
                                                                                choices = list("ODP"=1,"Gamma"=2),selected = 2))
                                                               ),
                                                               wellPanel(
                                                                 numericInput("Boot_sims", "Nombres de tirage",
                                                                              min = 0, max = 1000000, value = 5000)
                                                               ),
                                                               wellPanel(
                                                                 awesomeRadio("seedoption","Option sur la graine de simulation",choices=c("Ne pas préciser"=0,"Préciser"=1),selected=0),
                                                                 conditionalPanel(
                                                                   condition = "input.seedoption == 1",
                                                                   numericInput("CLBootstrap_seed", "Simulation seed value",value=1328967780,
                                                                                min = 0)
                                                                 )
                                                               ),
                                                               
                                                               wellPanel(
                                                                 numericInput("Boot_percentile", "Additional percentile value(%)",
                                                                              min = 0, max = 100, value = 85)
                                                               )#,
                                                               
                                                               
                                                               #fluidRow(actionButton("InfoCLBoot1","Aide",icon = icon("info")),actionButton("ResetCLBootinputs","Réinitialisation",icon = icon("refresh")))
                                                     ) # close overall assumption wellPanel
                                              ),# close first three columns
                                              column(10,
                                                     tabBox(width=10,
                                                            tabPanel("Données & résidus",
                                                                     fluidRow(
                                                                       
                                                                       column(12,
                                                                              h5("Triangle"),
                                                                              DT::dataTableOutput("Datatri_CLBoot"),
                                                                              hr()),
                                                                       
                                                                       box(    title = "Sigmas de Mack"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                               DT::dataTableOutput("Mack"),width=12 ,status="primary"),
                                                                       hr(),
                                                                       box(    title = "Visualisation des sigmas de Mack"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                               highchartOutput("MackPlot"),width=12 ,status="primary"),
                                                                       conditionalPanel(
                                                                         condition='input.RecupCoef',
                                                                         hr(),
                                                                         box(    title = "Rappel des coefficients de ChainLadder retenus ..."    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                                 DT::dataTableOutput("RappelCoefCL"),width=12 ,status="primary")),
                                                                       
                                                                       
                                                                       hr(),
                                                                       box(    title = "Résidus simples"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                               uiOutput("ui_bootSimple"),width=12 ,status="primary"),
                                                                       hr(),
                                                                       box(    title = "Résidus normalisés"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                               uiOutput("ui_boot"),width=12 ,status="primary")
                                                                     )
                                                                     #DT::dataTableOutput("Bootstrap_residuals"),
                                                                     
                                                            ),
                                                            tabPanel("Résultats & Quantiles de bootstrap",
                                                                     fluidRow(
                                                                       column(12,
                                                                              hr(),
                                                                              h5("Résumé des résultats"),
                                                                              DT::dataTableOutput("Bootstrap_results"),
                                                                              hr(),
                                                                              h5("Quantiles"),
                                                                              DT::dataTableOutput("Bootstrap_percentiles")
                                                                       )
                                                                     )
                                                            ),
                                                            
                                                            tabPanel("Graphs",
                                                                     
                                                                     hr(),
                                                                     fluidRow(
                                                                       column(6, 
                                                                              h5('Amplitude des tranches de montants'),
                                                                              sliderInput('amp',label = h3(""),min=0,max=50000, value=2000, step=500))),
                                                                     
                                                                     hr(),
                                                                     fluidRow(
                                                                       column(12,
                                                                              
                                                                              box(    title ='Histogramme des simulations de charges',status = "primary"    ,solidHeader = TRUE     ,collapsible = TRUE     ,
                                                                                      plotlyOutput("Bootstrap_graphs1"),width=6 ),
                                                                              box(    title ='Fonction de répartition',status = "primary"    ,solidHeader = TRUE     ,collapsible = TRUE     , 
                                                                                      highchartOutput("Bootstrap_graphs2"),width=6))), #, height = 500
                                                                     hr(),
                                                                     fluidRow(
                                                                       column(6, h5("Résultats de simulation par cohorte"),
                                                                              plotOutput("Bootstrap_graphs3")),
                                                                       column(6, h5("Back test latest dev yr"),
                                                                              plotOutput("Bootstrap_graphs4"))) #, height = 500
                                                                     
                                                                     # tabBox(title="", id="CLBootgraphstab",width=10,
                                                                     #        tabPanel("Histogramme",
                                                                     #                 plotOutput(outputId="Bootstrap_graphs1")
                                                                     #        ),
                                                                     #        
                                                                     #        tabPanel("Fonction de répartition",
                                                                     #                 plotOutput(outputId="Bootstrap_graphs2")
                                                                     #        ),
                                                                     #        tabPanel("Résultats de simulation par cohort",
                                                                     #                 plotOutput(outputId="Bootstrap_graphs3")
                                                                     #        ),
                                                                     #        
                                                                     #        tabPanel("Back test latest dev yr",
                                                                     #                 plotOutput(outputId="Bootstrap_graphs4")
                                                                     #        )
                                                                     # ) # end of tabBox
                                                                     
                                                            ) # end of Graphs tabPanel 
                                                     ) # end of tabsetPanel for results and graphs
                                              ) # end of column 9
                                              
                                              
                                            ) # end of fluidRow
                                            
                                            
                                          ) # end of fluidPage
                                          
                                 ) # end of tabPanel for GLM assumptions and results
                                 
                                 
                                 
                                 
                                 
                                 
                      ) # overall navbarPage closes 
                      ) # fluidPage closes
                      ) # overall dashboard body, ending definition of UI


ui <- dashboardPage(header_vMJ, sidebar_vMJ, body)





