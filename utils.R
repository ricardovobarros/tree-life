generate_scattermap = function(df_trees, date){
  
  # remove trees that weren't planted yet 
  df = remove_nonexisting_trees(df_trees, date)
  
  # create a column with the colors red and blue 
  df_2 = create_colored_trees(df, date)
  
  
  # create map
  fig <- df_2
  fig <- fig %>%
    plot_ly(
      lat = ~lat,
      lon = ~lon,
      marker = list(color = ~colorx, size=4),
      type = 'scattermapbox',
      hovertext = paste0("id:", df_2[,1])
    ) 
  fig <- fig %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =7.5,
        center = list(lon = 111.99, lat = -3.03))) 
  fig
  
  return(fig)
}

generate_histogram =function(df_tress,date){
  
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

# sum months of leaving of the trees
df_trees$life_time = rowSums(df_trees[,4:end_date_index], na.rm=TRUE)

# create data-frames with alive and dead trees 
df_trees_dead = df_trees[is.na(df_trees[,end_date]) ,]
df_trees_alive = df_trees[!is.na(df_trees[,end_date]) ,]

# compute number of bis of histogram
nbins = max(df_trees$life_time) 



fig <- plot_ly(alpha = 0.6, nbinsx=nbins)
fig <- fig %>% add_histogram(x = df_trees_alive$life_time, name="alive",
                             marker = list(color="green"),
                             opacity=0.6)
if (nrow(df_trees_dead!=0)){
  fig <- fig %>% add_histogram(x = df_trees_dead$life_time, name= "dead",
                             marker = list(color="red"),
                             opacity=0.6)
}
fig <- fig %>% layout(barmode = "overlay",
                      yaxis = list(title = "Num. of trees"),
                      xaxis = list(title = "Months of Life"))

return(fig)
  
}

generate_cumulative = function(df_trees, date){
  
# transform date in strings and remove Xs from column dates 
colnames(df_trees) = gsub("X", "", colnames(df_trees), fixed=T)
start_date = gsub("-",".", as.character(date[1]), fixed=T)

#remove past dates 
dfg = data.frame(colnames(df_trees)) 
start_date_index = which(dfg[,1]==start_date)
if(start_date != dfg[4,]){
  initial_months = seq(from=4,to=(start_date_index-1), by=1)
  df_trees = subset(df_trees, select = -initial_months)
}

# loop to count planted trees
df_planted_cum = data.frame(month=character(),planted=double())
df = df_trees
for(col in 4:ncol(df_trees)){
  
  
  # sum planted three in a specific month
  if(col!=4){
    n_planted = sum(df[,col], na.rm = TRUE)
  } else {n_planted = 0}
  
  # add new n of planted trees
  
  df_planted_cum[col-3,] = c(colnames(df)[col], n_planted)
  
  #filter by removing counted trees
  df = subset(df, is.na(df[,col]))
  
}

# create column with cumulative sum
df_planted_cum[,"planted_cum"] = cumsum(df_planted_cum$planted)


# loop to count downed trees
df_downed_cum = data.frame(month=character(),downed_cum=double())
df = df_trees
for(col in 4:ncol(df_trees)){
  
  # filter all with NAN
  df_nan = subset(df, is.na(df[,col]))
  
  # filter all existing tree before current time
  if(col==4) var = -1 else if(col==5) var = 0 else var = 1
  df_nan$rowsum = rowSums(df_nan[,4:(col-var)], na.rm=TRUE)
  df_downed = subset(df_nan, rowsum > 0)
  
  # find indexes of downed tress
  downed_indexes = as.numeric(rownames(df_downed))
  
  # sum planted three in a specific month
  if(col==4) n_downed = 0 else n_downed = dim(df_downed)[1]
  
  # add new n of planted treesdf_
  df_downed_cum[col-3,] = c(colnames(df)[col], n_downed)
}

print("_________________________________")
print(df_downed_cum)
print(df_planted_cum)
print("_________________________________")

#plot cumulative line
fig_cum = plot_ly(df_downed_cum, x= ~month)
fig_cum = fig_cum %>% add_lines(y=~downed_cum, name="downed", line=list(color="red"))
fig_cum = fig_cum %>% add_lines(y=df_planted_cum$planted_cum, name="planted",line=list(color="green"))
fig_cum = fig_cum %>% layout( yaxis = list(title = "cumulative number of trees"))
 return(fig_cum)
}

remove_nonexisting_trees= function(df_trees, date){
  
  # remove Xs from column dates 
  colnames(df_trees) = gsub("X", "", colnames(df_trees), fixed=T)
  
  # trasform date in stings
  start_date = gsub("-",".", as.character(date[1]), fixed=T)
  end_date = gsub("-",".", as.character(date[2]), fixed=T)
  
  # filter non existing trees
  last_index= tail(which(df_trees[ ,grepl(end_date, names(df_trees))] == 1), 1)
  
  df = df_trees[1: last_index, ]
  
  return(df)
  
}

create_colored_trees = function(df, date){
  
  # transform date in stings
  start_date = gsub("-",".", as.character(date[1]), fixed=T)
  end_date = gsub("-",".", as.character(date[2]), fixed=T)
  
  # create e column in the data-frame with conditional
  df$colorx = ifelse(is.na(df[ ,grepl(end_date, names(df))]), 
                     "red", "green")
  
  return(df)
}

treat_dataframe = function(df_trees, date){
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
return(df_trees)
}

convert_date = function(date){
  
  # date = as.Date(c("01/02/18", "01/02/20"), format="%m/%d/%y")
  date_converted = as.Date(c(paste0(substr(as.character(date[1]),1,8),"01"),
                              paste0(substr(as.character(date[2]),1,8),"01")
                             )
                           )
  
  print(date_converted) 
  return(date_converted)           

}

compute_balance = function(df, date){
  
  
  colnames(df_trees) = gsub("X", "", colnames(df_trees), fixed=T)
  start_date = gsub("-",".", as.character(date[1]), fixed=T)
  
  #remove past dates 
  dfg = data.frame(colnames(df_trees)) 
  start_date_index = which(dfg[,1]==start_date)
  if(start_date != dfg[4,]){
    initial_months = seq(from=4,to=(start_date_index-1), by=1)
    df_trees = subset(df_trees, select = -initial_months)
  }
  
  # loop to count planted trees
  df_planted_cum = data.frame(month=character(),planted=double())
  df = df_trees
  for(col in 4:ncol(df_trees)){
    
    
    # sum planted three in a specific month
    if(col!=4){
      n_planted = sum(df[,col], na.rm = TRUE)
    } else {n_planted = 0}
    
    # add new n of planted trees
    
    df_planted_cum[col-3,] = c(colnames(df)[col], n_planted)
    
    #filter by removing counted trees
    df = subset(df, is.na(df[,col]))
    
  }
  
  # create column with cumulative sum
  df_planted_cum[,"planted_cum"] = cumsum(df_planted_cum$planted)
  
  
  # loop to count downed trees
  df_downed_cum = data.frame(month=character(),downed_cum=double())
  df = df_trees
  for(col in 4:ncol(df_trees)){
    
    # filter all with NAN
    df_nan = subset(df, is.na(df[,col]))
    
    # filter all existing tree before current time
    if(col==4) var = -1 else if(col==5) var = 0 else var = 1
    df_nan$rowsum = rowSums(df_nan[,4:(col-var)], na.rm=TRUE)
    df_downed = subset(df_nan, rowsum > 0)
    
    # find indexes of downed tress
    downed_indexes = as.numeric(rownames(df_downed))
    
    # sum planted three in a specific month
    if(col==4) n_downed = 0 else n_downed = dim(df_downed)[1]
    
    # add new n of planted treesdf_
    df_downed_cum[col-3,] = c(colnames(df)[col], n_downed)
  }
  
  planted = as.numeric(tail(df_planted_cum$planted_cum,1))
  
  df_tmp_p = df_planted_cum %>% filter(month == gsub("-",".", as.character(date[2])))
  df_tmp_d = df_downed_cum %>% filter(month == gsub("-",".", as.character(date[2])))
  downed = as.numeric(df_tmp_d$downed_cum)
  planted = as.numeric(df_tmp_p$planted_cum)
  balance = planted-downed
  
  
  
  
  print(df_planted_cum)
  print(df_downed_cum)
  
  print(date)
  print(planted)
  print(downed)
  print(balance)

return(balance)
  
}


#load data 
df_trees = read.csv("tree-data.csv")

# define date interval
date = as.Date(c("01/01/13", "01/01/21"), format="%m/%d/%y")

# modify data-frame 
df_trees = treat_dataframe(df_trees, date)




