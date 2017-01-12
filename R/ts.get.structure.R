
get.ts.structure <-
  function(vardata) {
    if (is.factor(vardata))
      vardata <- as.character(vardata)
    
    if (any(is.na(vardata))) {
      return(list(start = NA, freq = NA))
    }
    
    if (is.numeric(vardata)) {
      if (any(vardata != round(vardata)))
        return(list(start = NA, frequency = NA))
      vardata <- as.character(vardata)
      if (any(nchar(vardata) > 4))
        return(list(start = NA, frequency = NA))
    }
    
    firstval <- vardata[1]
    
    lastval <- vardata[length(vardata)]
    
  
    
  ### extract the first and the last year by finding the first few digits
    if(all(grepl("[Y]?[0-9]+", vardata,ignore.case = TRUE))){
      
      ## find the location of the year
      first.year.loc = regexpr("[Y]?[0-9]+", firstval,ignore.case = TRUE)
      
      last.year.loc = regexpr("[Y]?[0-9]+", lastval,ignore.case = TRUE)
      
      
      ##check if there is a Y in the year and substring the year
      if(attr(first.year.loc, "match.length") == 4){
        first.year = as.numeric(substr(firstval,first.year.loc, 
                                       attr(first.year.loc,"match.length") + first.year.loc -1))
        
        last.year =  as.numeric(substr(lastval, last.year.loc, 
                                       attr(last.year.loc,"match.length") + last.year.loc -1))
      }
      else 
        
      {
        first.year = as.numeric(substr(firstval,first.year.loc + 1, 
                                       attr(first.year.loc,"match.length") + first.year.loc -1))
        
        last.year =  as.numeric(substr(lastval, last.year.loc + 1, 
                                       attr(last.year.loc,"match.length") + last.year.loc -1))
      }
    }
    

  ### if the frequency is yearly and check for holes in the series
  if(all(grepl("^[Y]?[0-9]+$", vardata,ignore.case = TRUE))){
    
    
     if (last.year - first.year +1 == length(vardata)){
        
        start = c(first.year)
    
        freq = 1}
      else 
        
       return(list(start = NA, frequency = NA))
  }
    
    
  
  ### if the frequency is monthly and check for holes in the series
    
    if(all(grepl("^[Y]?[0-9]+[M][0-9]+$", vardata,ignore.case = TRUE))){
      
     
      first.month.loc = regexpr("[M][0-9]+", firstval,ignore.case = TRUE)
      
      last.month.loc = regexpr("[M][0-9]+", lastval,ignore.case = TRUE)
      
      first.month = as.numeric(substr(firstval, first.month.loc + 1,
                                      attr(first.month.loc, "match.length") + first.month.loc -1))
      
      last.month = as.numeric(substr(lastval, last.month.loc + 1,
                                      attr(last.month.loc, "match.length") + last.month.loc -1))
      

      if ( 12 - first.month + 1 + last.month + (last.year - first.year - 1)*12 == length(vardata)){
        
        start = c(first.year, first.month)
        
        freq = 12}
      else 
        
        return(list(start = NA, frequency = NA))
    }
    
    
  ### quarterly
    
    if(all(grepl("^[Y]?[0-9]+[Q][0-9]+$", vardata,ignore.case = TRUE))){
      
     
      first.quarter.loc = regexpr("[Q][0-9]+", firstval,ignore.case = TRUE)
      
      last.quarter.loc = regexpr("[Q][0-9]+", lastval,ignore.case = TRUE)
      
      first.quarter = as.numeric(substr(firstval, first.quarter.loc + 1,
                                      attr(first.quarter.loc, "match.length") + first.quarter.loc -1))
      
      last.quarter = as.numeric(substr(lastval, last.quarter.loc + 1,
                                     attr(last.quarter.loc, "match.length") + last.quarter.loc -1))
      
      if ( 4 - first.quarter + 1 + last.quarter + (last.year - first.year -1)*4 == length(vardata)){
        
        start = c(first.year, first.quarter)
        
        freq = 4}
      else 
        
        return(list(start = NA, frequency = NA))
    }
    
    
    
    ### weekly
    
    if(all(grepl("^[Y]?[0-9]+[W][0-9]+$", vardata,ignore.case = TRUE))){
     
      first.week.loc = regexpr("[W][0-9]+", firstval,ignore.case = TRUE)
      
      last.week.loc = regexpr("[W][0-9]+", lastval,ignore.case = TRUE)
      
      first.week = as.numeric(substr(firstval, first.week.loc + 1,
                                        attr(first.week.loc, "match.length") + first.week.loc -1))
      
      last.week = as.numeric(substr(lastval, last.week.loc + 1,
                                       attr(last.week.loc, "match.length") + last.week.loc -1))
      
      
      if ( 52 - first.week + 1 + last.week + (last.year - first.year -1)*52 == length(vardata)){
        
        start = c(first.year, first.week)
        
        freq = 52}
      else 
        
        return(list(start = NA, frequency = NA))
    }
    
    
    
    list(start = start, frequency = freq)
  }

