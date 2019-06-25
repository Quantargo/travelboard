get_places_data<-function(destination, type, filter_variables, filter_min, filter_max){
  data <- read_rds(paste0("~/workshop/data/google_places/",tolower(destination()),".rds"))[[type]]
  
  for(i in 1:length(filter_variables)){
    variable <- filter_variables[i]
    if(variable %in% names(data)){
      if(variable == "price_level"){
        data[[variable]][is.na(data[[variable]])] <- 0
      }
      data <- data[!is.na(data[[variable]]) & data[[variable]]>=filter_min[i] & data[[variable]]<=filter_max[i],]
    }
  }
  data
}