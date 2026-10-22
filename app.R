library(shiny)
library(bslib)
library(DT)
library(shinyalert)
library(tidyverse)

#source("helpers.R")


# Load, clean the data----
student_data <- read_csv("data.csv", locale = locale(encoding = "ISO-8859-1"))

og_questions<-colnames(student_data)

# Rename columns I'm keeping
studata<-student_data|>
  rename(
    num_pets = 7,
    dist = all_of(9),
    minutes = 10,
    how_old = 11,
    us_born = 12,
    name_ltrs = 13,
    since_movie = all_of(14),
    num_stuffies = 15,
    non_scsu = 16,
    stu_yr = 17,
    rand_num = 18,
    semester = 19,
    acad_yr = 20,
    dog = 21,
    cat = 22, 
    reptile = 23,
    fish = 24, 
    bird = 25,
    other = 26,
    none = 27)|>
  
  # Remove unneeded columns
  select(-c(1:6,8,28))|>
  
  #Change numerical values to 'Yes' or 'No', as appropriate.
  
  mutate(dog = if_else(dog == 1,"Yes","No"), 
         cat = if_else(cat == 1,"Yes","No"),
         reptile = if_else(reptile == 1,"Yes","No"),
         fish = if_else(fish == 1,"Yes","No"),
         bird = if_else(bird == 1,"Yes","No"),
         other = if_else(other == 1,"Yes","No"),
         none = if_else(none == 1,"No pets","Has pet(s)")) |>
  
  #Finally, before I start with the summaries, I'll create factors for my         qualitative variables and assign the levels, as appropriate.
  
  
  mutate(us_bornF = factor(us_born,levels=c("Yes","No")),
         since_movieF = factor(since_movie,levels=c("Less than 7 days","7 - 14 days","15 - 30 days","31 - 180 days","More than 180 days or Never")),
         non_scsuF = factor(non_scsu,levels=c("Yes","No")),
         stu_yrF = factor(stu_yr,levels=c("Freshman", "Sophomore", "Junior", "Senior","PSEO/Other")),
         semesterF = factor(semester,levels=c("Fall","Spring")),
         acad_yrF = factor(acad_yr,levels = c("2024/2025", "2025/2026","2026/2027")),
         dogF = factor(dog,levels=c("Yes","No")),
         catF = factor(cat,levels=c("Yes","No")),
         reptileF = factor(reptile,levels=c("Yes","No")),
         fishF = factor(fish,levels=c("Yes","No")),
         birdF = factor(bird,levels=c("Yes","No")),
         otherF = factor(other,levels=c("Yes","No")),
         noneF = factor(none,levels=c("Has pet(s)","No pets"))
         
  )

# Create UI ----
# Create sidebar and widgets

ui<-page_fluid(
  
  card(
    #Begin user instructions
    card_header("Below, select the variables you'd like to analyze. See the About tab for more details.")
    ),
    
    layout_columns(
      col_widths = c(3,3,3,3),
    # Select first categorical variable
    card(selectInput(inputId = "cat1",
                label = "First categorical variable",
                choices = c("Native to US" = "us_bornF",
                            "Time since going to the cinema" = "since_movieF",
                            "Transfer status" = "non_scsuF",
                            "University class level" = "stu_yrF",
                            "Semester" = "semesterF",
                            "Academic Year" = "acad_yrF",
                            "Pet owner (any)" = "noneF",
                            "Dog owner" = "dogF",
                            "Cat owner" = "catF",
                            "Reptile owner" = "reptileF",
                            "Bird owner" = "birdF",
                            "Other animal" = "otherF"),
                selected = "noneF"),
    
    
    #Subset on levels of the first categorical variable
    uiOutput("levels1"),
    ),
                  
    # Select second categorical variable
    card(selectInput(inputId = "cat2",
                label = "Second categorical variable",
                choices = c("Native to US" = "us_bornF",
                            "Time since going to the cinema" = "since_movieF",
                            "Transfer status" = "non_scsuF",
                            "University class level" = "stu_yrF",
                            "Semester" = "semesterF",
                            "Academic Year" = "acad_yrF",
                            "Pet owner (any)" = "noneF",
                            "Dog owner" = "dogF",
                            "Cat owner" = "catF",
                            "Reptile owner" = "reptileF",
                            "Bird owner" = "birdF",
                            "Other animal" = "otherF"),
                selected = "stu_yrF"
    ),
    
    #Subset on levels of the second categorical variable
    uiOutput("levels2")
    ),
    
    #Select first numeric variable
    card(selectInput(inputId="x",
                label = "First numeric variable, x",
                choices = c("Number of pets" = "num_pets",
                            "Distance traveled to class" = "dist",
                            "Time on phone in minutes" = "minutes",
                            "Guess at professor's age" = "how_old",
                            "Number of letters in nickname" = "name_ltrs",
                            "Number of stuffed animals" = "num_stuffies",
                            "Randomly selected number" = "rand_num"),
                selected = "num_pets"
                ),
    
    #Allow for range of interest
    uiOutput("range1")
    ),
  
    #Select second numeric variable
    card(selectInput(inputId="y",
                label = "Second numeric variable, y",
                choices = c("Number of pets" = "num_pets",
                            "Distance traveled to class" = "dist",
                            "Time on phone in minutes" = "minutes",
                            "Guess at professor's age" = "how_old",
                            "Number of letters in nickname" = "name_ltrs",
                            "Number of stuffed animals" = "num_stuffies",
                            "Randomly selected number" = "rand_num"),
                selected = "num_stuffies"),
  
  # Allow for range again
  uiOutput("range2")
  ),
  # Create action button
  actionButton("get_data","Collect data")
  ),
  
  #Create tabs for main panel
  navset_card_underline(
    # About tab
    nav_panel("About",
              tags$figure(
                class = "centerFigure",
                tags$img(
                  src = "logo.png",
                  width = 600,
                  alt = "SCSU logo"
                )
              ),
              "text from README"),
    
    #Numeric Data Exploration
    nav_panel("Data Exploration",
        # plots tab
        navset_card_tab(
          nav_panel("Summaries of numerical data"),
          nav_panel("Summaries of categorical data"),
          nav_panel("Numerical and categorical data")
        )
        
    ),
  
    #Data tab
    nav_panel("Data",
            layout_columns(
              col_widths = c(3,3),
              downloadButton("get_filtered", "Download Filtered Data"),
              downloadButton("get_studata", "Download Full Dataset")
              ),
              DT::dataTableOutput("filtered_studata")
              )
  )
)

#Create server----
server <- function(input, output, session) {
  # Set up trigger to change plots
  trigger<-reactiveVal(0)
  
   #Dynamic radio buttons for first categorical variable
  output$levels1 <- renderUI({
    #Make sure we have input$cat1 (something to work with)
    req(input$cat1)
    
    #Get relevant categorical data
    cat1_data <- studata[[input$cat1]]
    
    #Get options
    cat1_lvls <- levels(cat1_data)
    
    #Make buttons
    radioButtons(inputId = "levels1",
                 label = "Values of interest for first variable",
                 choices = c(
                   "All" = "all",
                   cat1_lvls),
                 selected = "all"
                 )
     })
  
  #Dynamic radio buttons for second categorical variable
  output$levels2 <- renderUI({
    #Make sure we have input$cat1 (something to work with)
    req(input$cat2)
    
    #Get relevant categorical data
    cat2_data <- studata[[input$cat2]]
    
    #Get options
    cat2_lvls <- levels(cat2_data)
    
    #Make buttons
    radioButtons(inputId = "levels2",
                 label = "Values of interest for second variable",
                 choices = c(
                   "All" = "all",
                   cat2_lvls),
                 selected = "all"
    )
  })
  
  
  #Dynamic x-slider
  output$range1 <- renderUI({
    # Make sure we have input$x (something to work with)
    req(input$x)
   
    # Get relevant numeric data
    x_data <- studata[[input$x]]
    
    #Make slider
    sliderInput(inputId = "range1",
                label = "Range of interest for x-value",
                min = min(x_data),
                max = max(x_data),
                value=c(min(x_data),max(x_data))
    )
  })
  
  #Dynamic y-slider
  output$range2 <- renderUI({
    # Make sure we have input$y (something to work with)
    req(input$y)
    
    # Get relevant numeric data
    y_data <- studata[[input$y]]
    
    #Make slider again
    sliderInput(inputId = "range2",
                label = "Range of interest for y-value",
                min = min(y_data),
                max = max(y_data),
                value=c(min(y_data),max(y_data))
    )
  })
  # Alert user to repeated selections
  observeEvent(input$get_data, {
    if(input$cat1 == input$cat2 || input$x == input$y){
      shinyalert(title = "Warning!",
                 "You've repeated your variable selction(s). Please adjust your variable selections so neither your categorical nor numeric variables are repeated.")
    } else {trigger(trigger() + 1)}
  }) 

  #Filter data set based on selections
  filtered_studata<- eventReactive(trigger(),{
    #Check to make sure I have what I need
    req(input$x,input$y,input$range1,input$range2,input$cat1,input$cat2,input$levels1,input$levels2)
    
    # Filter
    studata|>
      select(all_of(c(input$cat1, input$cat2,input$x,input$y))) |>
      filter(
        .data[[input$x]]>=input$range1[1],.data[[input$x]]<=input$range1[2],
        .data[[input$y]]>=input$range2[1],.data[[input$y]]<=input$range2[2],
        (input$levels1 == "all" | .data[[input$cat1]] == input$levels1),
        (input$levels2 == "all" | .data[[input$cat2]] == input$levels2) 
      )
      
  })
  
output$get_filtered <- downloadHandler(
    filename = function() {
      paste('filtered_studata-', Sys.Date(), ".csv", sep = '')
  },
  content = function(con) {
    write.csv(filtered_studata(), con)
  }
)

output$get_studata <- downloadHandler(
  filename = function() {
    paste('studata-', Sys.Date(), ".csv", sep = '')
  },
  content = function(con) {
    write.csv(studata, con)
  }
)
  

  #Create output table for Data tab
  output$filtered_studata <- DT::renderDataTable({
    filtered_studata() 
  })
    
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
