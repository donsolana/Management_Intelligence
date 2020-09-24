#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library("shiny")
library("projmanr")
library("lpSolve")
library("readr")
library("shinythemes")
########UI#######
vchoices <- 1:ncol(mtcars)
names(vchoices) <- names(mtcars)
ui <- tagList(
                navbarPage(
                 theme =  shinytheme(theme = "slate"),
                 title = "Management Intelligence",
                 ######## Critical Path analysis######
                 tabPanel("Critical Path Analysis",
                          sidebarLayout(
                              sidebarPanel(
                                  title = "Critical Path",
                                  numericInput("ID", "Activity ID", 1),
                                  dateInput("start_date", "Enter start date"),
                                  fileInput("Task Data","Input Task Data",
                                            accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                              ),
                              mainPanel(
                                  
                                  plotOutput("network"),
                                  hr(),
                                  helpText("Critical path method (CPM) offers managers a means of planning, scheduling and controlling large and complex projects")
                           )
                         )
                 ),
                 tabPanel("Gantt Chart",
                          sidebarLayout(
                              sidebarPanel(
                                  title = "Gantt Chart",
                                  numericInput("ID", "Activity ID", 1),
                                  dateInput("start_date", "Enter start date"),
                                  fileInput("Task Data","Input Task Data",
                                            accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))
                              ),
                              mainPanel(
                                  plotOutput("gantt"),
                                  hr(),
                                  helpText("The Gantt Chart was invented by Henry Gantt, it provides a visual aid for project scheduling. 
                                           Load a file containing taskdata"),
                                  hr(),
                                  verbatimTextOutput("crit_path"),
                                  verbatimTextOutput("total_duration"),
                                  verbatimTextOutput("end_date")
                                  
                            )
                          )
                 ),
            #linear programming panel
                 tabPanel(title = "Linear programming",
                          sidebarLayout(
                              sidebarPanel(
                                  fileInput("lp_mat", "Input .csv file"
                                            ,accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                                  hr(),
                                  sliderInput("obj_row", "Kindly, choose the row number that represents decision variables:", 
                                              min = 1, max = 100, value = 1),
                                  hr(),
                                  sliderInput("rhs_col",
                                              "and the column number for the  right hand side: ", 
                                              min = 1, max = 100, value = 1),
                                  hr(),
                                  selectInput("const.dir", "Maximize or Minimize?", c("Max" = "max",
                                                                                   "Min" = "min"))
                                  
                              ), 
                              mainPanel( 
                                  tabsetPanel(
                                  tabPanel(title = " Matrix", 
                                               tableOutput("lp_mat"),
                                               hr(),
                                               helpText(" Linear Programming (LP) is a modelling technique that focuses on enabling managers to make decisions relating to planning and resource allocation.")),
                                  tabPanel(title = "Solution", verbatimTextOutput("lp_solution"))
                                      
                                      )
                                  
                            )
                          )
                 ),
                 
                 ##Assignment model panel
                 tabPanel(title =  "Assignment Model",
                          sidebarLayout(
                              sidebarPanel(fileInput("assign_mat","Input cost Matrix",
                                                     accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                                           hr(),
                                           selectInput("assign_dir", "Selcetion model direction", 
                                                       choices = c("Max"= "max",   "Min"= "min"))
                              ),
                              mainPanel(
                                  tabsetPanel(
                                        tabPanel(title = "Cost Matrix", tableOutput("cost_mat"),
                                                 hr(), 
                                                 helpText("The assignment model is a special type of linear programming model the assignment models aims to find the best selection of agents to complete a set of tasks. An agent can be a machine or human; we can say the assignment model helps select the right individuals for a Project, the right machine for a job, the best marketer for a region and so on. (Render et al. 2017). This type of optimization problem is referred to as Combinatorial, as it deals with finding the optimal selection out of a possible list of alternatives.")),
                                        tabPanel(title = "Assignment Table",tableOutput("assign"))
                              )
                             )
                            )
                 ),
                 tabPanel(title =  "Monte Carlo simulation",
                          sidebarLayout(
                          sidebarPanel(
                          fileInput("mont_dat", "Input a csv file with a single column", accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                          sliderInput("exp_time", "How many times should the experiment be Ran", min = 1, max = 10000, value = 99),
                          numericInput("sample_size", "Select Sample Size", value = 99),
                          numericInput("new_value", "Input a value and you'll get the likelyhood of the oucome!!", value = 20 )
                        ),
                              mainPanel(h4(textOutput("Monte_carlo")),
                                        hr(), 
                                        helpText("A simulation can be described as a replication of a real-world system or process. Simulations are often used in business and science, as a way of experimenting ideas that will otherwise be too complex or expensive to experiment in real life")
                                        )
             )
           )
         )    
    
)  


server <- function(input, output) { 
    ###########################################network, CPM, Gantt###############################
    crit_path<- critical_path(taskdata2)
    critical <- crit_path$critical_path
    total_duration <- crit_path$total_duration
    end_date <- crit_path$end_date
    
    paste("The critical path is", critical, "and the total duration and end date of your project are respectfully", total_duration, end_date )
    
    #plot the network diagram
    output$network <- renderPlot({network_diagram(taskdata2)})
    
    #plot the Gantt chart
    output$gantt <- renderPlot({gantt(taskdata2)})
    output$crit_path <- renderText({crit_path<- critical_path(taskdata2)
                                    critical <- crit_path$critical_path
                                    paste("The activities on your critical path are", critical)
                                })
  
    
    output$total_duration <- renderText({crit_path<- critical_path(taskdata2)
                                         total_duration <- crit_path$total_duration
                                         paste("The total duration of your project is",total_duration)
    })
    output$end_date <- renderText({crit_path<- critical_path(taskdata2)
                                   end_date <- crit_path$end_date
                                   paste("The end_date of your project",end_date)
    })
    #################Assignment Model##################################
    #create reactive cost matrix
    
     assign_mat<- reactive({assign_dat<-input$assign_mat
     assign_dat <-read_csv(assign_dat$datapath)
     as.matrix(assign_dat)})
    
    #render cost matrix
    output$cost_mat <- renderTable({assign_mat()})
    #display assignment matrix
    output$assign <- renderTable({optimal.assign <- lp.assign(assign_mat(), input$assign_dir)
    optimal.assign$solution})
    ##############Linear Programming#######
    ###create reactive values
    lp_mat <- reactive({lp_dat <- input$lp_mat
      lp_mat <- read_csv(lp_dat$datapath)
      as.matrix(lp_mat)})

    objective.in <- reactive({lp_mat()[1, -3]})
    const.mat <- reactive({lp_mat()[-1, -3]})
    const.rhs <- reactive({lp_mat()[-1, 3]})
    #render lp data table
   output$lp_mat <- renderTable({lp_mat()})
    #render lp solution
   output$lp_solution <- renderPrint({optimal <- lp(input$const.dir, objective.in(), const.mat(), c("<",">="), const.rhs())
   optimal_obj_coe <- optimal$solution
   optimal_solution <- optimal$objval
   paste("The optimal solution for the unknown values is", 
         optimal_obj_coe, 
         "and the", 
         input$const.didr,
         "value is", 
         optimal_solution 
   )})
   #########monte carlo#####
   #read in CSV
   mont_dat <- reactive({mont_dat <- input$mont_dat
                        read_csv(mont_dat$datapath)
               })
   ##Run montecarlo experiment by sample user data with a samplesize "sample_size" and runtime of "exp_time"
   
   mont_exp <- reactive({ replicate(input$exp_time,  
                        { r<- sample(mont_dat(), input$sample_size, replace = TRUE) 
                          sum(r)
                        })
                                    
   }) 
   #render Monte carlo results by first finding expected value using mean and sd 
   output$Monte_carlo <-renderText({exp_value<- mean(mont_exp())
                                  Std_deviation <- sd(mont_exp())
                                prob <- pnorm(input$new_value, exp_value, Std_deviation) 
                                paste("The expected value of your experiment is", 
                                      exp_value,
                                      "with a standard deviation of",
                                       Std_deviation, 
                                       "and the cummulative probability of the new event is", 
                                       prob, sep = " ")}) 
   
}





# Run the application 
shinyApp(ui = ui, server = server)