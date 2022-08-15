library(plotly)
library(dplyr)
source("utils.R")


#load data 
df_trees = read.csv("tree-data.csv")

# define date interval
date = as.Date(c("01/01/13", "01/01/21"), format="%m/%d/%y")

# modify data-frame 
df_trees = treat_dataframe(df_trees, date)

# create scatter map
fig_scatter = generate_scattermap(df_trees,date)

# create age living histogram
fig_histogram = generate_histogram(df_trees,date)

# create cumulative curve
fig_cum = generate_cumulative(df_trees, date)

# Printing scatter area
fig_histogram
fig_scatter
fig_cum
