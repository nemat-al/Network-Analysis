if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
if (!require('datasets')) install.packages('datasets'); library('datasets') #for the data tables
fillPage(
    tags$style(".assential{padding:30px;}",
               "li a{color: #ffe57d;}",
               HTML("
                     .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #ffffff;
                    }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #000000;
                     }
                    .assential { height: 90vh; overflow-y: auto; }
                    p{ color:#ffe57d;}
                    
                   "
               )),
    div(class="assential",style="background-color: #563d7c;color: white;width: 100%; height: 100%",
        div( h1("Detecting Communities"), style = "text-align: center"),
        "Please upload the data file, then select the algorithm to be used for detecting communities or comparing algorithms",
        br(),br(),
        fileInput("file", "Choose a CSV File",
                  accept = c(
                      ".txt",
                      ".csv")
        ),
        fluidRow(
            column(3, checkboxInput("header", "Header", TRUE)),
            column(6, radioButtons(inputId = 'sep', label = 'Separator', 
                                   choices = c(Comma=',' ,Semicolon=';'
                                               ,Tab='\t', Space=''), inline = TRUE,
                                   selected = ','))
            
        ),
    ####################TABS#####################
    tabsetPanel(type = "tabs",
                #########################Centrality Measurments####################################
                tabPanel(style="color=black","Centrality Measurments",
                         br(),
                         actionButton("do_centrality", "find Measurments",style="margin-left:20px;"),
                         br(),br(),
                         fluidRow
                         (
                             column(4,div(
                                 p("Average Degree of Centrality: "),br(),br(),
                                 p("Average Betweenness: "),br(),br(),
                                 p("Average Closeness: "),br(),br(),
                                 p("Average Eigenvector: "),br(),br(),
                                 p("Page Rank: "),br(),br()
                             )
                             ),
                             column(3,
                                    div(
                                        textOutput("avg_deg"),br(),br(),
                                        textOutput("avg_bet"),br(),br(),
                                        textOutput("avg_clos"),br(),br(),
                                        textOutput("avg_evec"),br(),br(),
                                        textOutput("avg_pafe"),br(),br()
                                    )
                             ),
                             column(4,
                                    "Scroll Down to see the details",
                                    div(
                                        actionButton("degree_details", "Degree details"),br(),br(),
                                        actionButton("bet_details", "Betwenness details"),br(),br(),
                                        actionButton("clos_details", "Closeness details"),br(),br(),
                                        actionButton("eig_details", "Eigenvector details"),br(),br(),
                                        actionButton("page_details", "Page rank details"),br(),br()
                                    )
                                    
                             )
                         ),
                         dataTableOutput("newWindowContent")
                ),
                ############################PLOTING######################################
                tabPanel(style="color=black","Plot", 
                         br(),
                         actionButton("do_plot", "plot network",style="margin-left:20px;"),
                         plotOutput("plot_net")
                ),
                ###############################Modularity###########################
                tabPanel(style="color=black","Modularity", 
                         br(),
                         "Evaluation of the strength of the community structure given by the following Algo, modularity:",
                         br(),br(),
                         actionButton("do_Recomend", "find modularity",style="margin-left:20px;"),
                         br(),br(),
                         p("Infomap: "),textOutput("infomap_mod"),textOutput("state_info"),
                         p("Edge betwenness: "),textOutput("edge_mod"),textOutput("state_2"),
                         p("Fast greedy: "),textOutput("greed_mod"),textOutput("state_3"),
                         p("Label Progroption: "),textOutput("progrop_mod"),textOutput("state_4"),
                         p("Leading eigenvector: "),textOutput("eigen_mod"),textOutput("state_5"),
                         p("Louvian: "),textOutput("louvian_mod"),textOutput("state_6"),
                         p("Walktrap: "),textOutput("walktrap_mod"),textOutput("state_7"),
                         p("Optimal modularity: "),textOutput("modul_mod"),textOutput("state_8")
                ),
                ###############################Detecting############################
                tabPanel(style="color=black","Detecting",
                         br(),
                         box(selectInput(inputId = "algorithm", label = "The available algorithms: ", choices = list('Infomap', 'Edge-Betweenness'
                                                                                                                     ,'fast greedy','Label Propagation'
                                                                                                                     ,'Leading Eigenvector','MultiLevel(louvian)'
                                                                                                                     ,'walktrap','optimal modularity')
                         ) ),
                         actionButton("do_detect", "find communities",style="margin-left:20px;"),
                         mainPanel(
                             "Number of communities: ",textOutput("len_comm")
                             , dataTableOutput('comm_details')
                         )
                ),
                ###############################Comparing#############################
                tabPanel("Comparing", 
                         br(),
                         fluidRow(
                             column(4,
                                    box(selectInput(inputId = "first_algo", label = "First algorithm: ", choices = list('Infomap', 'Edge-Betweenness'
                                                                                                                        ,'fast greedy','Label Propagation'
                                                                                                                        ,'Leading Eigenvector','MultiLevel(louvian)'
                                                                                                                        ,'walktrap','optimal modularity')
                                    ),
                                    "Number of communities: ",textOutput("len_comm_first"),dataTableOutput('size_comm_first')
                                    )),
                            
                             
                             column(4,box(selectInput(inputId = "second_algo", label = "Second algorithm: ", choices = list('Infomap', 'Edge-Betweenness'
                                                                                                                            ,'fast greedy','Label Propagation'
                                                                                                                            ,'Leading Eigenvector','MultiLevel(louvian)'
                                                                                                                            ,'walktrap','optimal modularity')
                             ),"Number of communities: ",textOutput("len_comm_second")
                             ,dataTableOutput('size_comm_second')
                             )),
                             column(3,
                                    
                                    radioButtons(inputId = 'comp_method', label = 'Comparing Methods', 
                                                 choices = c(variation_of_information="vi",
                                                             normalized_mutual_information="nmi"
                                                             ,split_join="split.join", rand="rand", adjusted_rand = "adjusted.rand"), inline = TRUE,
                                                 selected = 'vi'),
                                    actionButton("do_compare", "Compare the two Algo",style="margin-left:20px;"),
                                    br(),br(),"Distance= ",textOutput("distance")
                             )
                         )
                         ),
                ##########################Influencer#############################
                tabPanel(style="color=black","Influencer", 
                         br(),
                         actionButton("do_Influence_pr", "Influence Page Rank",style="margin-left:20px;"),
                         br(),
                         "The most influencer member has the id ",textOutput("the_influencer"),
                         br(),
                         "It's page rank ",textOutput("the_page_rank"),
                         br(),
                         dataTableOutput("Influencer_PR")
                )
    )#tabs
    
    
    )#div class esential
    )#fill page