library(shiny)
library(shinyjs)
library(shinyBS)
library(data.table)
library(echarts4r)
library(billboarder)
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(formattable)
library(DT)
library(summarytools)
library(recipes)
library(h2o)
library(waiter)
library(shinycssloaders)
library(esquisse)
library(lime)
h2o.no_progress()  # turn off progress bars
h2o.init()

##########################################################################################################


label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}

#######################################################################################################

df<- fread("./training.csv")
coln=colnames(df)


#######################################################################################################

explore_distribution_numeric <- function(x, vname = "numeric") {
  x <- x[!is.na(x)]
  nuniq <- length(unique(x))
  if (nuniq < 30) {
    nbins <- 10
  } else if (nuniq >= 30 && nuniq < 300) {
    nbins <- 20
  } else {
    nbins <- 30
  }
  d <- data.frame(var = x)
  g <- ggplot(d, aes(x)) +
    theme_bw() +
    geom_histogram(aes(y = ..density..), bins = nbins, colour = "black", fill = "#56B4E9") +
    geom_density(size = 1) +
    ggtitle(paste0("Distribution - ", vname)) +
    xlab(NULL) +
    ylab(NULL) +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
      axis.text = element_text(size = 15),
      panel.border = element_blank()
    )
  g
}

explore_countperc_categorical <- function(x, max_values = 30) {
  df <- data.frame(Value = x, stringsAsFactors = FALSE)
  df <- df %>%
    group_by(Value) %>%
    summarise(Frequency = n()) %>%
    mutate(Proportion = Frequency / sum(Frequency)) %>%
    ungroup() %>%
    mutate(Value = ifelse(is.na(Value), "MISSING", Value)) %>%
    arrange(desc(Frequency))
  if (nrow(df) > max_values) {
    frequency_o <- sum(df[max_values:nrow(df), 2])
    proportion_o <- sum(df[max_values:nrow(df), 3])
    df <- bind_rows(df[1:(max_values - 1), ],
                    data.frame(Value = "OTHERS", Frequency = frequency_o,
                               Proportion = proportion_o,
                               stringsAsFactors = FALSE))
  }
  df
}


explore_distribution_categorical <- function(x, vname = "categorical") {
  d <- explore_countperc_categorical(x)
  g <- ggplot(d, aes(factor(Value), Proportion)) +
    theme_bw() +
    geom_col(width = 0.3, colour = "black", fill = "#56B4E9") +
    geom_text(aes(y = Proportion + .02,
                  label = sprintf("%.2f%%", Proportion * 100)), size = 3) +
    scale_y_continuous(
      limits = c(0, 1.1),
      breaks = seq(0, 1.1, by = 0.1),
      labels = scales::percent
    ) +
    ggtitle(paste0("Distribution - ", vname)) +
    xlab(NULL) +
    ylab(NULL) +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
      axis.text = element_text(size = 15),
      panel.border = element_blank()
    )
  list( distplot = g)
}



#####################################################################################################







