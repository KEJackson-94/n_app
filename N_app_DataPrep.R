############################################
# 1.2 Load relevant packages & data frames #
# & set proper working directory           #
############################################

# library(shiny)
# library(plotly)
# library(shiny)
# library(reshape2)
# library(readxl) # excel_sheets()
# library(Rcpp) # C++ integration allows current code which works with atomic vectors
# library(dplyr)

#setwd("C:\\GlobalNitrogen_app_v3\\") # depending on where you unzip file
getwd()

ISO_codes <- read.csv('ISO.csv') # https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv

my_file <- 'Zhang_et_al_2021.xlsx' # might need full path name
my_file_pathname <- file.path(getwd(), my_file)
sheets_lst <- excel_sheets(path = my_file_pathname)

###############################################################
# 1.2 join and melt supplemental data with proper ISO-3 codes #
###############################################################

FSU <- c("Estonia", "Georgia", "Kazakhstan", "Kyrgyzstan", "Latvia", 
         "Lithuania","Moldova, Republic of", "Russian Federation", "Tajikistan", 
         "Turkmenistan", "Ukraine","Uzbekistan")
dfs <- data.frame(matrix(ncol = 58, nrow = 0))
for (i in 1:length(sheets_lst)){
  df = read_excel(path = my_file_pathname, sheet = i)
  df$data_type <- sheets_lst[i]
  df['ISO3'] <- ISO_codes$alpha.3[match(df$...1, ISO_codes$name)]
  names(df) <- c("Country", as.character(c(1961:2015)), 'data_type', 'ISO3')
  old_names <- unique(df[is.na(df$ISO3), ]$'Country')
  new_names <- c("CÃ´te d'Ivoire", 'Congo, Democratic Republic of the', 
                 "Lao People's Democratic Republic", "Korea, Republic of", 
                 "Sudan", "Eswatini", 
                 "United Kingdom of Great Britain and Northern Ireland", 
                 "Tanzania, United Republic of", "FSU") 
  for (i in 1:length(old_names)){
    df$'Country'[df$'Country' == old_names[i]] <- new_names[i]}
  for (i in 1:length(FSU)){
    tst <- df[rep(df$'Country' == "FSU",), ]
    tst$'Country' <- FSU[i]
    df <- rbind(df, tst)} # bind FSU countries to data frame
  df['ISO3'] <- ISO_codes$alpha.3[match(df$'Country', ISO_codes$name)]
  df = filter(df, 'Country' != "FSU")
  print(unique(df[is.na(df$ISO3), ]$'Country')) # check for accuracy
  dfs <- rbind(dfs, df)
}
melt_df <- melt(dfs,id = c('Country','ISO3','data_type'),variable.name = 'Year')
write.csv(melt_df, 'melt3_df.csv')

