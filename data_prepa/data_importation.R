#------ Data Importing ----------
library(data.table) 



cmd <- 'xsv search -s countryCode "PL" /Users/midou/Desktop/Projets_Data/appsilon_project/occurence.csv'

cmd 
Poland_data <- read.csv(pipe(cmd))

write.csv(Poland_data,'/Users/midou/Desktop/Projets_Data/appsilon_project/Poland_data.csv',row.names = FALSE ) 


