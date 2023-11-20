#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

#read Data
total <- readRDS("Total.RDS")
region <- readRDS("Region.RDS")
age <- readRDS("Age.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("PCR-4-All Study Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "selected_virus",
            label = "Select Virus for Regions and Age Plot:",
            choices = c("SARS", "Influenza A", "Influenza B", "RSV"),
            selected = "SARS"
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("GesamtPlot"), 
          plotOutput("RegionPlot"),
          plotOutput("AgePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Gesamt Plot
  plot_gesamt <- ggplot(data = total, aes(x = Week, y = Positivrate, colour = Virus)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(name = "Wöchentliche Positivenrate (%)", limits = c(0,10)) +
    scale_x_continuous(name = "Week", breaks = c(44,45,46,47,48,49,50,51,52), limits = c(44, 52)) +
    theme_classic()
  plot_gesamt <- plot_gesamt +
    geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Virus), linetype = 2, alpha =0.1)
  
  #Plots by region (4 total)
  whichVirus <- reactive({
    req(input$selected_virus)
  })
  #SARS
  plot_sars_r <- ggplot(data = region, aes(x = Week, y = SARS_pr, colour = Region)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(values = c("moccasin","navajowhite2","burlywood1","magenta","burlywood","bisque3","navajowhite4")) +
    scale_y_continuous(name = "Wöchentliche SARS Positivenrate (%)", limits = c(0,15)) +
    scale_x_continuous(name = "Week", breaks = c(44,45,46,47,48,49,50,51,52), limits = c(44, 52)) +
    theme_classic()
  plot_sars_r <- plot_sars_r +
    geom_ribbon(aes(ymin = SARS_l, ymax = SARS_u, fill = Region), linetype = 0, alpha =0.1) +
    scale_fill_manual(values = c("moccasin","navajowhite2","burlywood1","magenta","burlywood","bisque3","navajowhite4"))
  #Influenza A
  plot_influ_a_r <- ggplot(data = region, aes(x = Week, y = Influenza_A_pr, colour = Region)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(values = c("moccasin","navajowhite2","burlywood1","magenta","burlywood","bisque3","navajowhite4")) +
    scale_y_continuous(name = "Wöchentliche Influenza A Positivenrate (%)", limits = c(0,15)) +
    scale_x_continuous(name = "Week", breaks = c(44,45,46,47,48,49,50,51,52), limits = c(44, 52)) +
    theme_classic()
  plot_influ_a_r <- plot_influ_a_r +
    geom_ribbon(aes(ymin = Influenza_A_l, ymax = Influenza_A_u, fill = Region), linetype = 0, alpha =0.1) +
    scale_fill_manual(values = c("moccasin","navajowhite2","burlywood1","magenta","burlywood","bisque3","navajowhite4"))
  #Influenza B
  plot_influ_b_r <- ggplot(data = region, aes(x = Week, y = Influenza_B_pr, colour = Region)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(values = c("moccasin","navajowhite2","burlywood1","magenta","burlywood","bisque3","navajowhite4")) +
    scale_y_continuous(name = "Wöchentliche Influenza B Positivenrate (%)", limits = c(0,15)) +
    scale_x_continuous(name = "Week", breaks = c(44,45,46,47,48,49,50,51,52), limits = c(44, 52)) +
    theme_classic()
  plot_influ_b_r <- plot_influ_b_r +
    geom_ribbon(aes(ymin = Influenza_B_l, ymax = Influenza_B_u, fill = Region), linetype = 0, alpha =0.1) +
    scale_fill_manual(values = c("moccasin","navajowhite2","burlywood1","magenta","burlywood","bisque3","navajowhite4"))
  #RSV
  plot_rsv_r <- ggplot(data = region, aes(x = Week, y = RSV_pr, colour = Region)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(values = c("moccasin","navajowhite2","burlywood1","magenta","burlywood","bisque3","navajowhite4")) +
    scale_y_continuous(name = "Wöchentliche RSV Positivenrate (%)", limits = c(0,15)) +
    scale_x_continuous(name = "Week", breaks = c(44,45,46,47,48,49,50,51,52), limits = c(44, 52)) +
    theme_classic()
  plot_rsv_r <- plot_rsv_r +
    geom_ribbon(aes(ymin = RSV_l, ymax = RSV_u, fill = Region), linetype = 0, alpha =0.1) +
    scale_fill_manual(values = c("moccasin","navajowhite2","burlywood1","magenta","burlywood","bisque3","navajowhite4"))
  
  #Region
  region_graph <- reactive({
    if (whichVirus() == "SARS"){
      plot_sars_r
    }
    else if (whichVirus() == "Influenza A"){
      plot_influ_a_r
    }
    else if (whichVirus() == "Influenza B"){
      plot_influ_b_r
    }
    else if (whichVirus() == "RSV"){
      plot_rsv_r
    }
  })
  
  #Plots by age (4 total)
  #SARS
  plot_sars_a <- ggplot(data = age, aes(x = Week, y = SARS_pr, colour = Age_Group)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(values = c("moccasin", "navajowhite2", "burlywood1","burlywood", "bisque3", "navajowhite4", "magenta")) +
    scale_y_continuous(name = "Wöchentliche SARS Positivenrate (%)", limits = c(0,15)) +
    scale_x_continuous(name = "Week", breaks = c(44,45,46,47,48,49,50,51,52), limits = c(44, 52)) +
    theme_classic()
  plot_sars_a <- plot_sars_a +
    geom_ribbon(aes(ymin = SARS_l, ymax = SARS_u, fill = Age_Group), linetype = 0, alpha =0.1) +
    scale_fill_manual(values = c("moccasin", "navajowhite2", "burlywood1", "burlywood", "bisque3", "navajowhite4", "magenta"))
  plot_sars_a <- plot_sars_a +
    labs(color = "Age Group", fill = "Age Group")
  #Influenza A
  plot_influ_a_a <- ggplot(data = age, aes(x = Week, y = Influenza_A_pr, colour = Age_Group)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(values = c("moccasin", "navajowhite2", "burlywood1","burlywood", "bisque3", "navajowhite4", "magenta")) +
    scale_y_continuous(name = "Wöchentliche Influenza A Positivenrate (%)", limits = c(0,15)) +
    scale_x_continuous(name = "Week", breaks = c(44,45,46,47,48,49,50,51,52), limits = c(44, 52)) +
    theme_classic()
  plot_influ_a_a <- plot_influ_a_a +
    geom_ribbon(aes(ymin = Influenza_A_l, ymax = Influenza_A_u, fill = Age_Group), linetype = 0, alpha =0.1) +
    scale_fill_manual(values = c("moccasin", "navajowhite2", "burlywood1", "burlywood", "bisque3", "navajowhite4", "magenta"))
  plot_influ_a_a <- plot_influ_a_a +
    labs(color = "Age Group", fill = "Age Group")
  #Influenza B
  plot_influ_b_a <- ggplot(data = age, aes(x = Week, y = Influenza_B_pr, colour = Age_Group)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(values = c("moccasin", "navajowhite2", "burlywood1","burlywood", "bisque3", "navajowhite4", "magenta")) +
    scale_y_continuous(name = "Wöchentliche Influenza B Positivenrate (%)", limits = c(0,15)) +
    scale_x_continuous(name = "Week", breaks = c(44,45,46,47,48,49,50,51,52), limits = c(44, 52)) +
    theme_classic()
  plot_influ_b_a <- plot_influ_b_a +
    geom_ribbon(aes(ymin = Influenza_B_l, ymax = Influenza_B_u, fill = Age_Group), linetype = 0, alpha =0.1) +
    scale_fill_manual(values = c("moccasin", "navajowhite2", "burlywood1", "burlywood", "bisque3", "navajowhite4", "magenta"))
  plot_influ_b_a <- plot_influ_b_a +
    labs(color = "Age Group", fill = "Age Group")
  #RSV
  plot_rsv_a <- ggplot(data = age, aes(x = Week, y = RSV_pr, colour = Age_Group)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(values = c("moccasin", "navajowhite2", "burlywood1","burlywood", "bisque3", "navajowhite4", "magenta")) +
    scale_y_continuous(name = "Wöchentliche RSV Positivenrate (%)", limits = c(0,15)) +
    scale_x_continuous(name = "Week", breaks = c(44,45,46,47,48,49,50,51,52), limits = c(44, 52)) +
    theme_classic()
  plot_rsv_a <- plot_rsv_a +
    geom_ribbon(aes(ymin = RSV_l, ymax = RSV_u, fill = Age_Group), linetype = 0, alpha =0.1) +
    scale_fill_manual(values = c("moccasin", "navajowhite2", "burlywood1", "burlywood", "bisque3", "navajowhite4", "magenta"))
  plot_rsv_a <- plot_rsv_a +
    labs(color = "Age Group", fill = "Age Group")
  
  #Age
  age_graph <- reactive({
    if (whichVirus() == "SARS"){
      plot_sars_a
    }
    else if (whichVirus() == "Influenza A"){
      plot_influ_a_a
    }
    else if (whichVirus() == "Influenza B"){
      plot_influ_b_a
    }
    else if (whichVirus() == "RSV"){
      plot_rsv_a
    }
  })
  
  #Plotoutput
  output$GesamtPlot <- renderPlot({
    plot_gesamt
  })
  output$RegionPlot <- renderPlot({
    region_graph()
  })
  output$AgePlot <- renderPlot({
    age_graph()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
