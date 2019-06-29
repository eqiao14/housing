library(dplyr)

split <- function (datatable) {

  #Getting colnames
  categories = unique(datatable)
  cat_housing = data.frame(datatable)
  
  #Fill dataframe with 0s and space for colname
  for (cat in categories) {
    
    cat_housing[,cat] = rep(0, times= nrow(cat_housing))
  }
  
  #Loop through rows of datatable
  #Grab the name of the datatable and set corresponding row to 1
  for(i in 1:length(cat_housing$datatable)){
    cat = as.character(cat_housing$datatable[i])
    cat_housing[,cat][i] = 1
  }

  #Delete first column 
  cat_columns = names(cat_housing)
  keep_columns = cat_columns[-1]
  cat_housing = select(cat_housing,one_of(keep_columns))
  
  
  return(cat_housing)
}


getmode <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
  
}

replace_na_mode <- function (datatable) {
  
  for (i in 1:nrow(datatable)) {
    for (j in 1:ncol(datatable)) {
      if(is.na(datatable[i,j])) {
        datatable[i,j] = getmode(datatable[,j])
      }
    }
  }
  return(datatable)
}

replace_any_mode <- function (datatable, thingtoreplace) {
  
  for (i in 1:length(datatable)) {
      if(datatable[i] == thingtoreplace) {
        datatable[i] = getmode(datatable)
      }
  }
  return(datatable)
}

explorer <- function(datatable, dimensions_vector, yaxis) {
  
  par(mfrow=dimensions_vector)
  
  n=TRUE
  start = 1
  
  while(n & (start+dimensions_vector[1] * dimensions_vector[2] - 1)<length(datatable)) {
    end = (start+dimensions_vector[1] * dimensions_vector[2] - 1)
    for(i in start:end) {
      plot(datatable[i], yaxis, xlab = names(datatable[i]))
    }
    
    start = start+dimensions_vector[1] * dimensions_vector[2]
    
    n <- readline(prompt="Continue? (T/F): ")
    n = as.logical(n)
    print(start:end)
    

  }
  
  for(i in start:length(datatable)) {
    plot(datatable[i], yaxis, xlab = names(datatable[i]))
  }
  
}