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

get_places_data_all<-function(destination, type_vector, filter_variables, filter_min, filter_max){
  allData <- data.frame()
  for( type in type_vector){
    data <- get_places_data(destination, type, filter_variables, filter_min, filter_max)
    if(!"price_level" %in% names(data)){
      data$price_level <- rep(NA,nrow(data)) 
    }
    if(!"rating" %in% names(data)){
      data$rating <- rep(NA,nrow(data)) 
    }
    if(!"user_ratings_total" %in% names(data)){
      data$user_ratings_total <- rep(NA,nrow(data)) 
    }
    data$type <- rep(type,nrow(data))
    allData <- rbind(data[,c("lat","lng","name","price_level","user_ratings_total","rating","type")],allData)
  }
  allData
}