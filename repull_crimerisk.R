### Re-pull Crimerisk data ####
#install package rstudio api:
install.packages('rstudioapi')

# set up directory and filepath
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
filepath<-dirname(rstudioapi::getActiveDocumentContext()$path)

# run ACMT geocoder 
source(paste0(filepath, '/crimerisk_loop_functions.R'))

# import geocoded data
dataset_geocoded<-read.csv(paste0(filepath, '/dataset_geocoded.csv'))

# run loop to pull data -- check the console for the status of the data pull
pull_crimerisk_data(dataset_geocoded)

print('Data pull complete, running summary functions to check data...')

# run function to check completeness of pulled data 
# (wait until stop sign has disappeared to indicate it has completed pulling the data)
check_crimerisk_data()

## if data is not complete, go back and run the pull_crimerisk_data again, the function will skip over already pulled data. 

