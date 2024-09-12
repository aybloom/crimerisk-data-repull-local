#### Set up functions ####

### Function to Create dataframe to pull data ####
create_dataset<-function(variable_list=variable_list){
  var.cols<-data.frame(matrix(ncol=length(variable_list)))#make a columns for each variable in the list
  colnames(var.cols)<-variable_list #name the columns
  dataset<-var.cols%>%mutate(id=NA, year=NA, radius=NA)%>%filter(!is.na(id))#add the columns to the dataset
  return(dataset)
}

load_crimerisk<-function(){
  dataset_crimerisk<-read.csv('~/workspace/Inspace/data_pull_measures/dataset_crimerisk.csv')%>%dplyr::select(crimerisk_vars[1]:radius)
  return(dataset_crimerisk)
}

pull_crimerisk_data<-function(dataset_geocoded){
  
  source(paste0(filepath, '/external_data-file_loader.R'))
  source(paste0(filepath, '/GeocoderACMT.R'))
  source(paste0(filepath, '/crimerisk_data_settings.R'))
  
  # check for existing dataset_crimerisk and create one if it doesn't exist yet.
  if(file.exists('dataset_crimerisk.csv')==FALSE){
    dataset_crimerisk<-create_dataset(variable_list=crimerisk_vars)
    write.csv(dataset_crimerisk, 'dataset_crimerisk.csv',row.names = FALSE)
  }
  # set data pull settings
  year=2017
  radius_vector<-c(500, 1000, 5000)
  dataset_crimerisk<-
    load_crimerisk()
  
  for(i in 1:length(dataset_geocoded$id)){
    
    id<-dataset_geocoded$id[i]
    latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
    longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
    print(paste0('Currently pulling data for id # : ', id, '; (id number ', i, ' out of ', length(dataset_geocoded$id), ')')) #print the number to keep track of progress
    #print(c(latitude, longitude))
    
    for(r in 1:length(radius_vector)){
      # Check for user interrupts

      radius<-radius_vector[r]
      print(paste0('pulling data for radius: ', radius))
      
      dataset_crimerisk<-read.csv('dataset_crimerisk.csv')
      
      #check for existing data in dataset:
      tryCatch({
        if(length(dataset_crimerisk$id) != 0){
          if(id %in% dataset_crimerisk$id[dataset_crimerisk$year==year & dataset_crimerisk$radius==radius]) next #skip the row if the data is already there
        }
        
        dataset_crimerisk<-read.csv('dataset_crimerisk.csv')%>%dplyr::select(crimerisk_vars[1]:radius)
        invisible(capture.output(
        suppressMessages(
          suppressWarnings(
            environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=2022, codes_of_acs_variables_to_get = NULL, 
                                                            external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = FALSE)
          )
        )))
        crimerisk_measures<-environmental_measures %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=2022, radius=radius)
        
        #combine 
        dataset_crimerisk<-rbind(dataset_crimerisk, crimerisk_measures)
        
        write.csv(dataset_crimerisk, 'dataset_crimerisk.csv', row.names = FALSE)
        
      },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
    }
    
  }
  
  
}


## summary table functions
### Summary Table of environmental measures ####
table_missingness<-function(dataset) {
  if('radius' %in% colnames(dataset)){
    summary_table_na<-dataset %>% group_by(year, radius) %>% dplyr::summarise(count_na=sum(is.na(.)), 
                                                                              count_total=n())}
  else if(!('radius' %in% colnames(dataset)) & 'year' %in% colnames(dataset)){
    summary_table_na<-dataset%>%group_by(year) %>% dplyr::summarise(count_na=sum(is.na(.)), 
                                                                    count_total=n())
  }
  else if('t10_ldb_gen1_9000' %in% colnames(dataset)){
    summary_table_na<-dataset%>%dplyr::summarise(count_na=sum(is.na(.)), 
                                                 count_total=n())
  }
  
  return(summary_table_na)
}

table_summary<-function(dataset) {
  non_summary_vars<-c('id', 'year', 'radius', 'X', 'GeoName')
  if(nrow(dataset)>0){
    data_summary<-cbind(colnames(dataset)%>% as.data.frame() %>% dplyr::rename(., variable_name=.) %>% dplyr::filter(!(variable_name %in% non_summary_vars)),
                        
                        dataset %>% dplyr::select(-(any_of(non_summary_vars))) %>% summarise_all(list(min=min), na.rm=TRUE) %>%t()%>%as.data.frame() %>% round(., digits=2) %>% dplyr::rename(min=V1), 
                        dataset %>% dplyr::select(-(any_of(non_summary_vars))) %>% summarise_all(list(max=max), na.rm=TRUE) %>%t()%>%as.data.frame() %>% round(., digits=2)%>% dplyr::rename(max=V1), 
                        dataset %>% dplyr::select(-(any_of(non_summary_vars))) %>% summarise_all(list(median=median), na.rm=TRUE) %>%t()%>%as.data.frame() %>% round(., digits=2)%>% dplyr::rename(median=V1), 
                        dataset %>% dplyr::select(-(any_of(non_summary_vars))) %>% summarise_all(~sum(length(which(is.na(.))))) %>%t()%>%as.data.frame() %>% dplyr::rename(NA_count=V1))
    row.names(data_summary)<-NULL
  }
  else(
    data_summary<-NULL)
  return(data_summary)
  
}


check_crimerisk_data<-function(){
  
  # import crimerisk data
  crimerisk_data<-read.csv('dataset_crimerisk.csv') %>% unique()
  write.csv(crimerisk_data, 'dataset_crimerisk.csv')
  
  crimerisk_missing<-table_missingness(crimerisk_data)
  crimerisk_summary<-table_summary(crimerisk_data)
  
  # check for duplicates
  duplicates<-crimerisk_data %>% unique() %>% group_by(id) %>% 
    summarise(n=n()) %>% filter(n>3)

  print(paste0('Total participants in geocoded dataset (expect crimerisk data for all of them): ', length(dataset_geocoded$id)))
  
  print(paste0('Total participants with data for radius==500: ', crimerisk_missing$count_total[crimerisk_missing$radius==500]))
  print(paste0('Total participants with data for radius==1000: ', crimerisk_missing$count_total[crimerisk_missing$radius==1000]))
  print(paste0('Total participants with data for radius==5000: ', crimerisk_missing$count_total[crimerisk_missing$radius==5000]))
  
  print(paste0('variables with inaccurate values: ', crimerisk_summary$variable_name[crimerisk_summary$min<1]))
  print(paste0('variables with missing values: ', crimerisk_summary$variable_name[crimerisk_summary$count_na>0]))
  
}
