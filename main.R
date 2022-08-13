library(plotly)
library(dplyr)
source("utils.R")


#load data 
df_trees = read.csv("tree-data.csv")

# define date interval
date = as.Date(c("01/01/18", "01/01/20"), format="%m/%d/%y")

#remove duplicates
df_trees = df_trees[duplicated(df_trees[,"lon"]),]

# transform date in strings and remove Xs from column dates 
colnames(df_trees) = gsub("X", "", colnames(df_trees), fixed=T)
start_date = gsub("-",".", as.character(date[1]), fixed=T)
end_date = gsub("-",".", as.character(date[2]), fixed=T)

# remove trees after the input end date
dfg = data.frame(colnames(df_trees)) 
end_date_index = which(dfg[,1]==end_date)
df_trees = df_trees[,1:end_date_index]

# filter non existing trees
last_index= tail(which(df_trees[ ,grepl(end_date, names(df_trees))] == 1), 1)
df_trees = df_trees[1: last_index, ]

# create scatter map
fig_scatter = generate_scattermap(df_trees,date)

# create age living histogram
fig_histogram = generate_histrogram(df_tress,date)

# create cumulative curve
fig_cum = generate_cumulative(df_trees, date)






# Printing scatter area
fig_histogram
fig_scatter
fig_cum
