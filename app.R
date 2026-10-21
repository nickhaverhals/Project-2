library(shiny)
library(bslib)
library(shinyalert)
library(tidyverse)

#source("helpers.R")


# Load the data
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

# Create sidebar and widgets

ui<-page_sidebar(
  
  sidebar = sidebar(
    #Need lots of room
    width = 500,
    
    #Begin user instructions
    h2("Select two numeric variables to analyze"),
    
    #Select first numeric variable
    selectInput(inputId="x",
                label = "First numeric variable, x",
                choices = c("How many pets do you have?" = "num_pets",
                            "How many minutes have you spent using your phone so far today?" = "minutes",
                            "How old do you think your stats professor is?" = "how_old",
                            "How many letters are in the name you prefer your friends and family call you?" = "name_ltrs",
                            "How many stuffed animals do you own?" = "num_stuffies",
                            "Randomly select a whole number between 1 and 10 (inclusive)." = "rand_num"),
                selected = "num_pets"
                ),
    
    #Allow for range of interest
    uiOutput("range1"),
  
    #Select second numeric variable
    selectInput(inputId="y",
                label = "Second numeric variable, y",
                choices = c("How many pets do you have?" = "num_pets",
                            "How many minutes have you spent using your phone so far today?" = "minutes",
                            "How old do you think your stats professor is?" = "how_old",
                            "How many letters are in the name you prefer your friends and family call you?" = "name_ltrs",
                            "How many stuffed animals do you own?" = "num_stuffies",
                            "Randomly select a whole number between 1 and 10 (inclusive)." = "rand_num"),
                selected = "num_stuffies"
    ),
    # Allow for range again
    uiOutput("range2")
  )
)

server <- function(input, output, session) {
  #Dyamnic x-slider
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
  
  # Other slider
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
  
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
