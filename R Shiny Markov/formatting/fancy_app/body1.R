body1 = dashboardBody(# open dashboardbody
  
  fluidRow(
    tabBox(title = "",
           width = "1200px",
           height = "1000px",
    
    # as above but results table
    tabPanel(title = "Results Table",
             tableOutput(outputId = "SO_icer_table")
            ), # # end box, fluidrow and tab item
    
    # as above but CE plane
    tabPanel(title = "Cost-effectiveness Plane",
             plotOutput(outputId = "SO_CE_plane"),
             downloadButton(outputId = 'cep', label = "Download Plot"),
             downloadButton(outputId = "downloadData", label =  "Download Data")
      ), # end box, fluidrow and tab item
    
    tabPanel(title = "CEAC",
             plotOutput(outputId = "SO_ceac"),
             downloadButton(outputId = 'ceac', 
                            label = "Download Plot")#,
             #downloadButton(outputId = "downloadData", label =  "Download Data")
    ), # end box, fluidrow and tab item
    
    
    # this is a tab which shows a single table
    # referenced as contents in the server
    tabPanel(title = "Uploaded data",
             "Uploaded data (if any) can be seen below:",
             br(),br(),
             tableOutput("contents")
    ), # end box, fluidrow and tab item
      
      # in this tab we use an i-frame to show another website.
      # here I show the classic Rick Astley song ' Never Gonna Give You Up'
      # however you could include promotional material
      # or a video explaining how to use the tool
    tabPanel(title = "Further Information",
             br(),
             tags$blockquote("Rickrolling, alternatively rick-rolling, is a prank and an Internet meme involving an unexpected appearance of the music video for the 1987 Rick Astley song 
             Never Gonna Give You Up. The meme is a type of bait and switch using a disguised hyperlink that leads to the music video. 
             When victims click on a seemingly unrelated link, the site with the music video loads instead of what was expected, 
             and in doing so they are said to have been rickrolled. 
             The meme has also extended to using the song's lyrics in unexpected places.",
            tags$a(href ="https://en.wikipedia.org/wiki/Rickrolling","Wikipedia")),
             
             br(),br(),
        
        tags$iframe(
         seamless = "seamless",
         src = "https://www.youtube.com/embed/dQw4w9WgXcQ",
         height = 300,
         width = 500), # end iframe
        
        br(),br(),br(),br(),br(),br(),
        
        "You can find all materials from this short course at our",
        tags$a(href="https://github.com/RobertASmith/shiny_healthy_economics","Github repository")
        
        
        
        #HTML('<img src="Logo_Final.jpg",
        #     height="300px"
        #     style="float:center"/>')
        
    ) # end box, fluidrow and tab item
    
    ) # close tabItems
     
      
    ), # close fluidRow
    
  #dashboardFooter(left = tags$div("hello",
  #                                tags$a(href="https://www.darkpeakanalytics.com/","Dark Peak Analytics"),
  #                                "me"),
  #                right = tags$img(src = "Logo_Final.jpg", width = "100px", height = "100px"))
  
  tags$footer(tags$img(src = "Logo_Final.jpg", width = "150px", height = "150px"),
              tags$br(),
              "This user-interface was created by Robert Smith & Paul Schneider of",
              tags$a(href="https://www.darkpeakanalytics.com/", "Dark Peak Analytics."),
              tags$br(),
              "For more information contact",
              tags$b("darkpeakanalytics@gmail.com"),
              style = "position:absolute;
                       bottom:50px;
                       height:200px;
                       padding: 0px;
                       background-color: white;
                       "
              ) #to discuss, please contact Robert Smith or Paul Schneider at darkpeakanalytics@gmail.com",

  ) # close dashboardBody

