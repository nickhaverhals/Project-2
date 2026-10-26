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


# Create name vectors since I've discovered I need it later
num_labs<- c("Number of pets" = "num_pets",
             "Distance traveled to class" = "dist",
             "Time on phone in minutes" = "minutes",
             "Guess at professor's age" = "how_old",
             "Number of letters in nickname" = "name_ltrs",
             "Number of stuffed animals" = "num_stuffies",
             "Randomly selected number" = "rand_num")

cat_labs <- c("Native to US" = "us_bornF",
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
              "Other animal" = "otherF")

# Create UI ----
# Create sidebar and widgets

ui<-page_fluid(
  useBusyIndicators(),
  card(
    #Begin user instructions
    card_header("Below, select the variables you'd like to analyze. See the About tab for more details.")
    ),
    
    layout_columns(
      col_widths = c(3,3,3,3),
    # Select first categorical variable
    card(selectInput(inputId = "cat1",
                label = "First categorical variable",
                choices = cat_labs,
                selected = "noneF"),
    
    
    #Subset on levels of the first categorical variable
    uiOutput("levels1"),
    ),
                  
    # Select second categorical variable
    card(selectInput(inputId = "cat2",
                label = "Second categorical variable",
                choices = cat_labs,
                selected = "stu_yrF"
    ),
    
    #Subset on levels of the second categorical variable
    uiOutput("levels2")
    ),
    
    #Select first numeric variable
    card(selectInput(inputId="x",
                label = "First numeric variable, x",
                choices = num_labs,
                selected = "dist"
                ),
    
    #Allow for range of interest
    uiOutput("range1")
    ),
  
    #Select second numeric variable
    card(selectInput(inputId="y",
                label = "Second numeric variable, y",
                choices = num_labs,
                selected = "minutes"),
  
  # Allow for range again
  uiOutput("range2")
  ),
  # Create action button
  actionButton("get_data","Load selected data")
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
              navset_card_tab(
                nav_panel("Information about data",
                          markdown("This app allows you to investigate data I've collected from my students on the first day of class in the introductory Statistical Thinking course I've been teaching since Fall 2024. The questions I've asked are given below, along with, in brackets, the variable(s) used to record responses. In the tabs to the right, you will find instructions for generating tables and plots to summarize these data and specific information about variables associated with them.")),
                nav_panel("How to use app",
                          markdown("To begin, select the data you're interested above. Next, nagivate through the **Data Exploration** tab to find the sumarized data you're interested in.<br><br> *Important note*: If you choose to subest on any of the variables above, this filters **all** the data you see. For instance, if you choose to only gather information on students with pets, but have all university class levels selected, the tables and plots you see will only include students with pets (even though the tables and plots may not refer to pet ownership). <br><br>In the **Summaries of numerical data** subtab, you have the option to display tables with summary statistics, histograms and/or a scatterplot (fitted with a least sqaures line)  for the selected variables. Note that the line is fit with the to <u>all</u> the selected data, but the color of the dots are grouped by the first categorical variable.<br><br> In the **Summaries of categorical data** you will find one and two way tables, standard bar graphs and bar graphs breaking out the two variables in a variety of ways. <br><br> The **Combined Plots** provides box and violin plots where the numeric variables are broken out by the selected categorical variables. <br><br> Finally, in the **Data** tab, you will find buttons to download the full and filtered data sets, as well as a table will the selected data in raw form.")
                
              ),
              nav_panel("Information about variables",
                        markdown("<u>**Survey Questions and Associated Variables**</u><br><br>1. How many pets do you have? [''Number of pets'' = `num_pets`]<br>2. What kind of pet do you have? (Select all that apply.) [''Pet owner (any)'' = `dog`, `cat`, `reptile`, `fish`, `bird`,`other`, `none`]<br>3. How far (in miles) did you commute to get to class today? (For reference, if you live on campus, from Hill to Wick is 0.2 miles and from Shoemaker to Wick is 0.1 miles.) [''Distance traveled to class'' = `dist`]<br>4. How many minutes have you spent using your phone so far today? [''Time on phone in minutes'' = `minutes`]<br>5. How old do you think your stats professor is? [''Guess at professor's age'' = `how_old`]<br>6. Were you born in the United States? [''Native to US'' = `us_born`]<br>7. How many letters are in the name you prefer your friends and family call you? [''Number of letters in nickname'' = `name_ltrs`]<br>8. How many long has it been since you've seen a movie in a movie theater? [''Time since going to the cinema'' = `since_movie`]<br>9. How many stuffed animals do you own? [''Number of stuffed animals'' = `num_stuffies`]<br>10. Have you attended an institution besides SCSU after graduating high school (or a school equivalent to high school)? [''Transfer status'' = `non_scsu`]<br>11. What year of university are you currently in? [''University class level'' = `stu_yr`]<br>12. Randomly select a whole number between 1 and 10 (inclusive). [''Randomly selected number'' = `rand_num`]<br><br><u>**Additional Notes**</u><br><br>a. For question 9, I had a few outlandish answers: 10000000 and 98765432. The average of all other values was 7.5 stuffed animals so I replaced them with 7 and 8, respectively.<br><br>b. Question 2 allowed for multiple selections so I created new columns to record whether students recorded, ''Dog'', ''Cat'', ''Reptile'', ''Fish'', ''Bird'', ''Other'', or ''None'' for question 2. I also added a ''Primary pet'' column which recorded the first pet listed but I think I won't use it, as it's misleading. The first pet listed was determined by the order of selection boxes, not necessarily what the student would consider their ''primary'' pet. So, that column is biased toward ''Dog'' and, although that's the correct answer, it likely doesn't represent all students' views faithfully."))
    )
    ),
    
    #Numeric Data Exploration
    nav_panel("Data Exploration",
        # plots tab
        navset_card_tab(
          nav_panel("Summaries of numerical data",
                    checkboxInput(inputId = "show_num_sum", label = "Show summary statistics",value=FALSE),
                    fluidRow(
                      column(width = 3, tableOutput("num_sum_spot")),
                      column(width = 3, tableOutput("cat1_sum_spot")),
                      column(width = 3, tableOutput("cat2_sum_spot")),
                      column(width = 3, tableOutput("comb_sum_spot"))
                    ),
                    #Make histograms
                    h6("Click the box below to see histograms of selected data."),
                    checkboxInput(inputId = "show_hists", label = "Show histograms",value=FALSE),
                    conditionalPanel( "input.show_hists == true",
                    fluidRow(
                       column(width = 6, plotOutput("hist1_spot")),
                       column(width = 6, plotOutput("hist2_spot"))
                       )
                    ),
                    
                    # Make scatter plot
                    h6("Click the box below to see a scatter plot selected data."),
                    checkboxInput(inputId = "show_scat", label = "Show scatter plot",value=FALSE),
                    plotOutput("scatter_spot"),
                    uiOutput("reg_eq"),
                                        ),
          
          nav_panel("Summaries of categorical data",
                    # plots tab
                    h6("One and two way tables for selected variables"),
                                checkboxInput(inputId = "show_cat_sum", label = "Show category counts",value=FALSE),
                                fluidRow(
                                  column(width = 4, tableOutput("cat1_count_spot")),
                                  column(width = 4, tableOutput("cat2_count_spot")),
                                  column(width = 4, tableOutput("two_way_spot"))
                                ),
                    
                     checkboxInput(inputId = "show_bars", label = "Show bar graphs",value=FALSE),
                    conditionalPanel( "input.show_bars == true",
                          fluidRow(
                            column(width = 6, plotOutput("bar1_spot")),
                            column(width = 6, plotOutput("bar2_spot"))
                            ),
                                      
                          fluidRow(
                           column(width = 4, plotOutput("scale_spot")),
                           column(width = 4, plotOutput("facet_spot")),
                           column(width = 4, plotOutput("dodge_spot")))

                          
                   )
              ),
        #Plots with numerical and categorical
          nav_panel("Combined Plots",
        h6("Combined numerical and categorical plots"),
        
        checkboxInput(inputId = "show_boxes", label = "Show box plots",value=FALSE),
        conditionalPanel( "input.show_boxes == true",
                          fluidRow(
                            column(width = 6, plotOutput("box1_spot")),
                            column(width = 6, plotOutput("box2_spot"))
                          ),
                          
                          fluidRow(
                            column(width = 6, plotOutput("box3_spot")),
                            column(width = 6, plotOutput("box4_spot"))
                          )
        ),
        #Working here: UI----
        checkboxInput(inputId = "show_violins", label = "Show violin plots",value=FALSE),
        conditionalPanel( "input.show_violins == true",
                          fluidRow(
                            column(width = 6, plotOutput("violin1_spot")),
                            column(width = 6, plotOutput("violin2_spot"))
                          ),
                          
                          fluidRow(
                            column(width = 6, plotOutput("violin3_spot")),
                            column(width = 6, plotOutput("violin4_spot"))
                          )
        )
        
        #End new code
          )
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
    
    #Make checkboxes for level selection
    checkboxGroupInput(inputId = "levels1",
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
    
    #Make checkboxes for level selection
    checkboxGroupInput(inputId = "levels2",
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
    sliderInput(inputId = "slider_x",
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
    sliderInput(inputId = "slider_y",
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
    #Check to make sure button's been pressed
    req(trigger() > 0)
    #Check to make sure I have what I need
    req(input$x,input$y,input$slider_x,input$slider_y,input$cat1,input$cat2,input$levels1,input$levels2)
    
    # Filter
    studata|>
      select(all_of(c(input$cat1, input$cat2,input$x,input$y))) |>
      filter(
        .data[[input$x]]>=input$slider_x[1],.data[[input$x]]<=input$slider_x[2],
        .data[[input$y]]>=input$slider_y[1],.data[[input$y]]<=input$slider_y[2],
        ("all" %in% input$levels1 | .data[[input$cat1]] %in% input$levels1),
        ("all" %in% input$levels2 | .data[[input$cat2]] %in% input$levels2) 
      )
      
  })
  
  # Numerical summaries----
  #Numerical summary table all
  output$num_sum_spot<-renderTable({
    #Check for checkbox
    req(input$show_num_sum == TRUE)
    
    sum_results <- apply(X=filtered_studata() |> select(where(is.numeric)), 
                       MARGIN = 2,
                       FUN = function(x){
                         c(Mean = mean(x), SD = sd(x), Min = min(x), Q1 = quantile(x,0.25), Median = quantile(x,0.5), Q3 = quantile(x,0.75), Max = max(x))
                       }
    )
    as.data.frame(sum_results)
    },rownames = TRUE)
  
  #Numerical summary table cat1
  output$cat1_sum_spot<-renderTable({
    #Check for checkbox
    req(input$show_num_sum == TRUE)
    
    cat1_sum_results <- filtered_studata() |> 
      group_by(.data[[input$cat1]])|>
      summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
    as.data.frame(cat1_sum_results)
  },rownames = FALSE)
  
  #Numerical summary table cat2
  output$cat2_sum_spot<-renderTable({
    #Check for checkbox
    req(input$show_num_sum == TRUE)
    
    cat2_sum_results <- filtered_studata() |> 
      group_by(.data[[input$cat2]])|>
      summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
    as.data.frame(cat2_sum_results)
  },rownames = FALSE)
  
  #Numerical summary table combined
  output$comb_sum_spot<-renderTable({
    #Check for checkbox
    req(input$show_num_sum == TRUE)
    
    comb_sum_results <- filtered_studata() |> 
      group_by(.data[[input$cat1]],.data[[input$cat2]])|>
      summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
    as.data.frame(comb_sum_results)
  },rownames = FALSE)
  
  
  # Make first histogram
  output$hist1_spot<-renderPlot({
     #Check for checkbox
     req(input$show_hists == TRUE)
    
    
     
  ggplot(data = filtered_studata(), aes(x = .data[[input$x]])) + 
  geom_histogram(fill = "red3", color = "gray", linetype = "solid") + 
    labs(title = names(which(num_labs == input$x)), x = input$x)
  }) |>
    bindEvent(input$get_data)
  
  #Make second histogram
  output$hist2_spot<-renderPlot({
    #Check for checkbox
    req(input$show_hists == TRUE)
    
    ggplot(data = filtered_studata(), aes(x = .data[[input$y]])) + 
      geom_histogram(fill = "red3", color = "gray", linetype = "solid") + 
      labs(title = names(which(num_labs == input$y)), x = input$y)
  }) |>
    bindEvent(input$get_data)
  
    # Make scatter plot
  output$scatter_spot <- renderPlot({
    #Check for checkbox
    req(input$show_scat == TRUE)
    
    ggplot(data = filtered_studata(), aes(x = .data[[input$x]], y = .data[[input$y]], color = .data[[input$cat1]])) +
      geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", linetype = "solid") + labs(title = paste(names(which(num_labs == input$x)), " vs ", names(which(num_labs == input$y))), x = input$x , y = input$y, color = input$cat1)
  })
  
  output$reg_eq <- renderUI({
    req(input$show_scat==TRUE)
    fit<-lm(reformulate(input$x, response = input$y),filtered_studata())
    paste("Linear model: y = ", round(coef(fit)[2],2),'x + ', round(coef(fit)[1],2), " with R^2 = ", round(summary(fit)$r.squared,3))
  })|>
    bindEvent(input$get_data)
  
  #Categorical summaries----
  #One and two way tables
  #cat1 one way
  output$cat1_count_spot<-renderTable({
    #Check for checkbox
    req(input$show_cat_sum == TRUE)
    
    cat1_count_results <- filtered_studata() |> 
      group_by(.data[[input$cat1]])|>
      summarize(count=n())
      },rownames = FALSE)
  
  #cat2 one way
  output$cat2_count_spot<-renderTable({
    #Check for checkbox
    req(input$show_cat_sum == TRUE)
    
    cat2_count_results <- filtered_studata() |> 
      group_by(.data[[input$cat2]])|>
      summarize(count=n())
  },rownames = FALSE)
  
  #Two way
  output$two_way_spot<-renderTable({
    #Check for checkbox
    req(input$show_cat_sum == TRUE)
    
    two_way_results <- filtered_studata() |> 
      group_by(.data[[input$cat1]], .data[[input$cat2]])|>
      summarize(count=n()) |>
      pivot_wider(names_from = .data[[input$cat1]],values_from=count)
  },rownames = FALSE)
  
 
  output$bar1_spot<-renderPlot({
    #Check for checkbox
    req(input$show_bars == TRUE)
    
    ggplot(data=filtered_studata(),aes(x=.data[[input$cat1]]))+geom_bar(fill = "red3", color = "gray")+labs(
      title = names(which(cat_labs == input$cat1)), x = input$cat1)
  })
  
  output$bar2_spot<-renderPlot({
    #Check for checkbox
    req(input$show_bars == TRUE)
    
    ggplot(data=filtered_studata(),aes(x=.data[[input$cat2]]))+geom_bar(fill = "red3", color = "gray")+labs(
      title = names(which(cat_labs == input$cat2)), x = input$cat2)
  })
  
 
  #Make scale fill plot
  output$scale_spot<-renderPlot({
    #Check for checkbox
    req(input$show_bars == TRUE)
    
    ggplot(data=filtered_studata(),aes(x=.data[[input$cat1]], fill = .data[[input$cat2]]))+geom_bar()+labs(
      title = names(which(cat_labs == input$cat1))) + scale_fill_discrete(names(which(cat_labs == input$cat2)))
  })
  
  #Make scale fill plot
  output$facet_spot<-renderPlot({
    #Check for checkbox
    req(input$show_bars == TRUE)
    
    ggplot(data=filtered_studata(),aes(x=.data[[input$cat1]], fill = .data[[input$cat1]]))+geom_bar()+labs(
      title = names(which(cat_labs == input$cat1)), x = " ") + facet_wrap(~.data[[input$cat2]]) + scale_fill_discrete("") +
       theme(axis.text.x = element_blank())
  })
  
  
  output$dodge_spot<-renderPlot({
    #Check for checkbox
    req(input$show_bars == TRUE)
    
    ggplot(data=filtered_studata(),aes(x=.data[[input$cat1]], fill = .data[[input$cat2]]))+geom_bar(position = "dodge")+labs(
      title = names(which(cat_labs == input$cat1)), x = " ") + scale_fill_discrete(names(which(cat_labs == input$cat2)))
  })
  
  
  #Make box plot for cat1, x
  output$box1_spot <- renderPlot({
    #Check for checkbox
    req(input$show_boxes == TRUE)
    
    ggplot(data = filtered_studata(), aes(x = .data[[input$cat1]], y = .data[[input$x]])) + geom_boxplot() +
      labs(title = paste(names(which(cat_labs == input$cat1)), " by ", names(which(num_labs == input$x))), x = names(which(cat_labs == input$cat1)), y = names(which(num_labs == input$x)))
  })
  #Combined plots----
  #Make box plot for cat1, y
  output$box2_spot <- renderPlot({
    #Check for checkbox
    req(input$show_boxes == TRUE)
    
    ggplot(data = filtered_studata(), aes(x = .data[[input$cat1]], y = .data[[input$y]])) + geom_boxplot() +
      labs(title = paste(names(which(cat_labs == input$cat1)), " by ", names(which(num_labs == input$y))), x = names(which(cat_labs == input$cat1)), y = names(which(num_labs == input$y)))
  })|>
    bindEvent(input$get_data)
  
  #Make box plot for cat2, x
  output$box3_spot <- renderPlot({
    #Check for checkbox
    req(input$show_boxes == TRUE)
    
    ggplot(data = filtered_studata(), aes(x = .data[[input$cat2]], y = .data[[input$x]])) + geom_boxplot() +
      labs(title = paste(names(which(cat_labs == input$cat2)), " by ", names(which(num_labs == input$x))), x = names(which(cat_labs == input$cat2)), y = names(which(num_labs == input$x)))
  })|>
    bindEvent(input$get_data)
  
  #Make box plot for cat2, y
  output$box4_spot <- renderPlot({
    #Check for checkbox
    req(input$show_boxes == TRUE)
    
    ggplot(data = filtered_studata(), aes(x = .data[[input$cat2]], y = .data[[input$y]])) + geom_boxplot() +
      labs(title = paste(names(which(cat_labs == input$cat2)), " by ", names(which(num_labs == input$y))), x = names(which(cat_labs == input$cat2)), y = names(which(num_labs == input$y)))
  })|>
    bindEvent(input$get_data)
  
    #Make violin plot for cat1, x
  output$violin1_spot <- renderPlot({
    #Check for checkbox
    req(input$show_violins == TRUE)
    
    ggplot(data = filtered_studata(), aes(x = .data[[input$cat1]], y = .data[[input$x]])) + geom_violin(scale = "area", fill = "red3") +
      labs(title = paste(names(which(num_labs == input$x)), " by ", names(which(cat_labs == input$cat1))), x = names(which(cat_labs == input$cat1)), y = names(which(num_labs == input$x)))
  })|>
    bindEvent(input$get_data)
  
  
  #Make violin plot for cat1, y
  output$violin2_spot <- renderPlot({
    #Check for checkbox
    req(input$show_violins == TRUE)
    
    ggplot(data = filtered_studata(), aes(x = .data[[input$cat1]], y = .data[[input$y]])) + geom_violin(scale = "area", fill = "red3") +
      labs(title = paste(names(which(num_labs == input$y)), " by ", names(which(cat_labs == input$cat1))), x = names(which(cat_labs == input$cat1)), y = names(which(num_labs == input$y)))
  })|>
    bindEvent(input$get_data)
  
  #Working here: Server----
  #Make violin plot for cat2, x
  output$violin3_spot <- renderPlot({
    #Check for checkbox
    req(input$show_violins == TRUE)
    
    ggplot(data = filtered_studata(), aes(x = .data[[input$cat2]], y = .data[[input$x]])) + geom_violin(scale = "area", fill = "red3") +
      labs(title = paste(names(which(num_labs == input$x)), " by ", names(which(cat_labs == input$cat2))), x = names(which(cat_labs == input$cat2)), y = names(which(num_labs == input$x)))
  })|>
    bindEvent(input$get_data)
  
  
  #Make violin plot for cat2, y
  output$violin4_spot <- renderPlot({
    #Check for checkbox
    req(input$show_violins == TRUE)
    
    ggplot(data = filtered_studata(), aes(x = .data[[input$cat2]], y = .data[[input$y]])) + geom_violin(scale = "area", fill = "red3") +
      labs(title = paste(names(which(num_labs == input$y)), " by ", names(which(cat_labs == input$cat2))), x = names(which(cat_labs == input$cat2)), y = names(which(num_labs == input$y)))
  })|>
    bindEvent(input$get_data)
  #End new code
  
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
  })|>
    bindEvent(input$get_data)
  
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
