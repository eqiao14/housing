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
  
  if(is.data.frame(datatable)) {
    
    for (i in 1:nrow(datatable)) {
      for (j in 1:ncol(datatable)) {
        if(is.na(datatable[i,j])) {
          datatable[i,j] = getmode(datatable[,j])
        }
      }
    }
  } 
  else {
    for (i in 1:length(datatable)) {
      if(is.na(datatable[i])) {
        datatable[i] = getmode(datatable)
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
  
  namestokeep = data.frame(matrix(nrow=nrow(datatable)))
  column_names = colnames(datatable)
  column_name_index = c()
  
  while(n & (start+dimensions_vector[1] * dimensions_vector[2] - 1)<length(datatable)) {
    end = (start+dimensions_vector[1] * dimensions_vector[2] - 1)
    
    for(i in start:end) {
      plot(datatable[i], yaxis, xlab = names(datatable[i]))
    }
    
    keep = readline(prompt='Keep any? (T/F):' )
    keep = as.logical(keep)
    
    if (keep) {
      
      #Read in a vector, add 'end' which will bump to correct index
      whichone = readline(prompt ='Which one(s)? Enter vector. Order is (TL/TR/BL/BR)')
      whichonestr = eval(parse(text = whichone))
      whichonestr = whichonestr + start - 1
      column_name_index = c(column_name_index, whichonestr)
      newvec = datatable[,whichonestr]
      namestokeep = cbind(namestokeep, newvec)
      
    }
    
    n <- readline(prompt="Continue? (T/F): ")
    n = as.logical(n)
    print(start:end)
    
    start = start + dimensions_vector[1]*dimensions_vector[2]
  }
  
  ##Print last partial plots 
  if (n) {
    for(i in start:length(datatable)) {
      plot(datatable[i], yaxis, xlab = names(datatable[i]))
   }

  }
  namestokeep = namestokeep[,-1]
  colnames(namestokeep) = column_names[column_name_index]
  return(namestokeep)
  
}

split_data_table <- function (datatable) {
  
  holder = data.frame(matrix(nrow=nrow(datatable)))
  
  for (i in 2:ncol(datatable)) {
    
    split = split(datatable[,i])
    holder = cbind(holder, split)

  }
  
  return(holder)
  
}