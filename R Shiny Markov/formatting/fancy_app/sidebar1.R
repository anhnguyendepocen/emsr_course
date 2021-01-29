library(shinydashboard)

# Sidebar ------------------
sidebar1 = dashboardSidebar(width = 400,
                            
                          sidebarMenu(
                              
                          # Input CSV of Treat Utililities
                           menuItem(text = "Upload data",
                           h4("Input CSV of Treatment Utilities"),
                           checkboxInput("use_CSV",label = "Overwrite Treatment Utilities with CSV"),
                           fileInput("file1", "Choose CSV File", accept = ".csv"),
                           checkboxInput("header", "Header", TRUE)
                           ),
                           
                           # Treatment Cost
                           menuItem(text = "Parameter Inputs",
                                    startExpanded = T,
                                    numericInput(inputId = "SI_c_Trt",      # id of input, used in server
                                        label = "Treatment Cost",  # label next to numeric input
                                        value = 200,               # initial value
                                        min = 0,                   # minimum value allowed
                                        max = 400),
                                    sliderInput(inputId = "SI_n_age_init",  # id of input, used in server
                                                label = "Initial Age",      # label next to numeric input
                                                value = 25,                 # initial value
                                                min = 10,                   # minimum value allowed
                                                max = 80),
                                    numericInput(inputId = "lambda",
                                                 label = "WTP Threshold",
                                                 value = 20000)
                           ),
                           
                           # Number of Simulations
                           menuItem(text = "Input Number of PSA runs",
                                    startExpanded = T,
                                    numericInput(inputId = "SI_n_sim",      # id of input, used in server
                                        label = "Select PSA runs",        # label next to numeric input
                                        value = 1000,              # initial value
                                        min = 0,                   # minimum value allowed
                                        max = 400)
                           
                           ),
                           
                           br(),
                           br(),
                           
                           # action button
                           fluidRow(
                             column(width = 12,
                             actionButton(inputId = "run_model",     # id of action button, used in server
                                          label   = "Run model",
                                          width = "300px") # customised button using CSS  
                           ))
                           
                           
                        
                           
                            ) # close sideBarmenu
                           
)  # close sidebar panel
