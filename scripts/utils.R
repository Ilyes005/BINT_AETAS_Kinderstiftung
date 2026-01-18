##Libraries
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(scales)
library(grid)
library(forcats)
library(ggplot2)
library(ggbeeswarm)
library(ggalluvial)
library(gghalves)
library(ggalluvial)
library(ggtext)
library(patchwork)
library(Hmisc)
##importing the data
selected.data <- readRDS("data/processed/selected.data.rds")
cleaned_data <- readRDS("data/processed/cleaned_data.rds")
clean_data <- readRDS("data/processed/clean_data.rds")
d <- readRDS("data/processed/d.rds")

##color palette
cyan <- "#2295B8"
Reddish.purple <- "#CC79A7"
Neutral.Gray <- "#999999"
teal <- "#009E73"
deep.blue <- "#0072B2"
light.cyan.mist <- "#88CCEE"
slate <- "#708090"
forest.green <- "#225522"
light_raspberry  <- "#F4C9DA"
medium_raspberry <- "#D46FA0"
deep_raspberry   <- "#B53679"
lightblue  <- "#DEEBF7"
steelblue  <- "#6BAED6"
navy       <- "#08519C"
