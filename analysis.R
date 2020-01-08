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

# Part A: ----------------------------------------------------------------------
# The distribution of unemployment rates among the different major categories

all_ages %>%
  # Order by mean of unemployment rate by major category
  group_by(Major_category) %>%
  mutate( mean_unemploy = mean(Unemployment_rate) ) %>%
  arrange(-mean_unemploy) %>%
  ungroup() %>%
  mutate( 
    Major_category = factor(Major_category, levels = unique(Major_category)) 
  ) %>%
  # Generate boxplot
  ggplot( aes(y = Unemployment_rate, x = Major_category) ) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  ylab("Unemployment Rate") +
  xlab("Major Category")

cap = paste0(
  "**Figure 1.** *Distribution of unemployment rates among the different major",
  "categories.*")

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

# Data Table: ------------------------------------------------------------------
# Extract key columns from all-ages.csv
all = all_ages[, .(Major_code, Total)]

# Merge and compute the proportion
recent = 
  recent_grad[, .(Major_code, Major, Major_category, Total)] %>%
  na.omit() %>%
  merge(all, by = "Major_code", suffixes = c("_grad", "_all")) %>%
  .[, Proportion := Total_grad / Total_all] %>%
  .[, .(Major_category, Major, Proportion, Total_grad, Total_all)] %>%
  .[order(-Proportion)]

# Bonus: -----------------------------------------------------------------------
# Mean differences of unemployment rate between all_ages and recent_grad
all_unemploy = all_ages %>% select(Major_code, Unemployment_rate)
diff_unemploy = 
  recent_grad %>%
  select(Major_code, Major_category, Unemployment_rate) %>%
  left_join(all_unemploy, by = "Major_code", suffix = c("_grad", "_all")) %>%
  mutate( diff = Unemployment_rate_grad - Unemployment_rate_all ) %>%
  group_by(Major_category) %>%
  summarize( mean_diff = mean(diff), 
             sd_diff = sd(diff),
             lwr = mean_diff - qnorm(.975) * sd_diff,
             upr = mean_diff + qnorm(.975) * sd_diff,
             diff_unemploy = sprintf('%3.3f (%3.3f, %3.3f)', 
                                     mean_diff, lwr, upr)) %>%
  filter(Major_category != "Interdisciplinary") %>%
  arrange(mean_diff)

# Plot
diff_unemploy %>% 
  ungroup() %>%
  arrange(-mean_diff) %>%
  mutate(
    Major_category = factor(Major_category, levels = unique(Major_category))
  ) %>%
  group_by(Major_category) %>%
  ggplot( aes( y = Major_category, x = mean_diff) ) +
  geom_point() +
  geom_errorbarh( aes(xmin = lwr, xmax = upr) ) +
  theme_bw() +
  ylab("Major Category") +
  xlab("Mean difference of unemployment rate between recent graduates and all ages")

cap = paste0(
  "**Figure 2.** *Mean differences of unemployment rate between recent ",
  "graduates and all ages.* The figure shows that recent graduates of ",        "Industrial Arts & Consumer Services major category tend to find jobs ",      "easier compared to all ages, while Social Science major category is the ",
  "opposite."
)

