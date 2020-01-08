# Introductory Analyses of College Majors Dataset
#
# This script is short data analysis for application of the Center for 
# Academic Innovation's Data Science Fellowship.
#
# Data: all-ages.csv, recent-grads.csv
# Source: https://github.com/fivethirtyeight/data/tree/master/college-majors
#
# Author: Ming-Chen Lu (mimngchlu@umich.edu)
# Updated: January 6, 202
#80: ---------------------------------------------------------------------------

# Libraries :-------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(shiny)

# Read in the data: ------------------------------------------------------------
all_ages = fread("https://github.com/fivethirtyeight/data/raw/master/college-majors/all-ages.csv")
recent_grad = fread("https://github.com/fivethirtyeight/data/raw/master/college-majors/recent-grads.csv")

# Part B: ----------------------------------------------------------------------
# Trends in colleage majors among recent graduates
# R shiny App: -----------------------------------------------------------------
# Define UI for dataset viewer app: --------------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Input: Select major categories to show in the plot
      checkboxGroupInput("ctg", "Major categories to show:",
                         unique(all_ages$Major_category), 
                         selected = unique(all_ages$Major_category)),
      
      # Create a line break
      br(),
      
      # Input: Proportion interval with step value
      sliderInput("prop", "Proportion:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.05)
    ),
    mainPanel(
      plotOutput("grad_barplot")  
    )
  )
)

# Define a server for shiny app: -----------------------------------------------
server <- function(input, output) {
  
  datainput = reactive({
    all = all_ages[, .(Major_code, Total)]
    recent_grad[, .(Major_code, Major, Major_category, Total)] %>%
      na.omit() %>%
      merge(all, by = "Major_code", suffixes = c(".grad", ".all")) %>%
      .[, prop_total := Total.grad / Total.all] %>%
      .[ prop_total >= input$prop & Major_category %in% input$ctg, 
         .(Major, Major_category, prop_total)] %>%
      .[order(-prop_total)]
  })
  
  output$grad_barplot = renderPlot({
    datainput() %>%
      arrange(prop_total) %>%
      mutate( Major = factor(Major, levels = unique(Major)) ) %>%
      as.data.frame() %>%
      ggplot( aes_string( x = "Major", y = "prop_total", fill = "Major_category")) +
      geom_bar( stat = "identity", width = .5) +
      coord_flip() +
      ylab('Percentage of recent graduates (ages < 28) in the majors')
    
  })
}

# Run the shiny app
shinyApp(ui, server, options = list(width = "100%", height = 650))