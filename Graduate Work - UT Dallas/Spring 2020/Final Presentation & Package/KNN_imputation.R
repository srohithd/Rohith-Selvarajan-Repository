##############################################################################################################
#  BASIC LOGIC - FIND SHORTEST DISTANCE BASED ON LAT & LONG AND IMPUTE MEAN OF 5 NEAREST NEIGHBOURS
#
################################################################################################################


#setwd("C:/Users/neerj/Desktop/Study Material/Spring 2020/DataSet")

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

packages = c("spdep", "nabor","e1071",
             "sjmisc", "sp","ggplot2","naniar","sqldf","DataExplorer","skimr")


package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)



require(spdep)
library(DataExplorer)
library(skimr)
library(sjmisc)
library(sqldf)
library(sp)
library(nabor)
library(e1071)
library(naniar)
library(ggplot2)


full_data <- read.csv("input_data.csv",header=TRUE)


###############################################################################################################
#
#                   EXPLORATORY DATA ANALYSIS
#
###############################################################################################################


plot_str(full_data)

### SUMARY STATISTICS

summary(full_data[2:17])
skim(full_data[2:17])


## Find Missing Columns
indx <- apply(full_data, 2, function(x) any(is.na(x)))
indx

## PLOT MISSING VALUES
gg_miss_var(full_data) + labs(y = "Number of rows missing")
plot_missing(full_data,group = list(Good = 0.05, Okay = 0.2, Poor = 0.4, Scarce = 1), 
             ggtheme = theme_gray()) + scale_fill_manual("Band", values = c("Good"="green2","Okay"="gold","Poor"="darkorange","Scarce"="firebrick2"))

 
  ### DATA DISTRIBUTION

#par(mar=c(1,1,1,1))




boxplot(full_data$X..ECE.enrolled,main = " % ECE Enrolled",
        col = "orange",
        border = "brown")



boxplot(full_data$school_poverty,main="School Poverty",col = "orange",
        border = "brown")# mean
   

boxplot(full_data$student_teacher_ratio,main="Student Teacher Ratio",col = "orange",
        border = "brown")  #median
 

boxplot(full_data$free_lunch,main="Free Lunch Ratio",col = "orange",
        border = "brown") 


boxplot(full_data$reduced_lunch,main="Reduced Lunch",col = "orange",
        border = "brown")
 

boxplot(full_data$Title1_school,main="Title1 School",col = "orange",
        border = "brown")    #median
 

boxplot(full_data$high_quality_ECE_centers,main="High Quality ECE Centers",col = "orange",
        border = "brown") ## Median

boxplot(full_data$med_hhinc2016,main="Median Household Income",col = "orange",
        border = "brown")  ## Median
 

boxplot(full_data$Poverty_prob_index,main="Poverty Probability Index",col = "orange",
        border = "brown") # Median
 

boxplot(full_data$MHLTH,main="Mental Health ratio",col = "orange",
        border = "brown")  
 

boxplot(full_data$PHLTH,main="Physical Health ratio",col = "orange",
        border = "brown")
 

boxplot(full_data$math.proficiency,main="Math proficiency",col = "orange",
        border = "brown")   # Median

boxplot(full_data$reading.proficiency,main="Reading Proficiency",col = "orange",
        border = "brown")  # Median




## Set Spatial Coordinates To Create A Spatial Object


xy <- full_data[,c("LAT","LONG")]

new_data<- SpatialPointsDataFrame(coords = xy, data = full_data,
                               proj4string = CRS("+proj=longlat +datum=WGS84 "))

str(new_data)

################################################
#  Nearest neighbours with LAT LONG distance
################################################


ed.knn<- knn( coordinates(new_data), coordinates(new_data), k=20)


ed.knn <- data.frame( knn=ed.knn[[1]][,2:20])  

## Now we have a dataframe which has corresponding neigbours for each point

#####################################
# Imputing School Poverty - Mean
#####################################

# getting indices of rows where null values


missing_index<- data.frame(which(is.na(new_data$school_poverty)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

# Checking if we have missing data in the column

if (is_empty(missing_index)){
  print("Yes, empty")
} else{             
for( i in missing_index)
{
    
   nearest <- ed.knn[i,]
 
}
## Here nearest has the neigbours for missing columns in School Poverty

# Retaining the actual missing points here  
missing_points<-as.numeric(rownames(nearest))
missing_points

## We need to access the row values to find corressponding data in the actual data. COnverting to dataframe for easy access

df <- data.frame(matrix(unlist(nearest), nrow=length(nearest), byrow=T))

## This section  looks up the data for the neighbours  and calculates the mean value of 5 nearest 
## neighbours where data is present and imputes our missing value

for ( i in 1:length(missing_points)){
  
  row_data<-df[,i]
 
  value_sckl<- vector("numeric")
  idx <- 1
  
  count<-1
  while(count <=5 & idx <=ncol(ed.knn)) { 
    if(!is.na(new_data@data$school_poverty[row_data[idx]])) {
      print(row_data[idx])
      value_sckl<-append(value_sckl,new_data@data$school_poverty[row_data[idx]],after=length(value_sckl))
      count <- count + 1 
   
  }
     
    idx <- idx + 1

  } 
  
  imputed_value<- mean(value_sckl)
 
  new_data@data$school_poverty[missing_points[i]]  <- imputed_value

}
}

### Rechecking if any missing data(Basically checking if values are imputed)
missing_index_sklpoverty_recheck<- data.frame(which(is.na(new_data$school_poverty)))

print(missing_index_sklpoverty_recheck)


################################################
##
#   Imputing Married Couple - Median
###############################################


# getting indices of rows where null values

 
missing_married<- data.frame(which(is.na(new_data$X..in.married_couple)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_married)){
  print("Yes, empty")
} else{
  for( i in missing_married)
  {
    
    nearest_married <- ed.knn[i,]
    
  }
  
  missing_points_married<-as.numeric(rownames(nearest_married))
  missing_points_married
  
  # for rows in missing values and for values in row values
  
  df_married <- data.frame(matrix(unlist(nearest_married), nrow=length(nearest_married), byrow=T))
  
  for ( i in 1:length(missing_points_married)){
   
    row_data_married<-df_married[,i]
    value_married<- vector("numeric")
    idx_married <- 1
    
    count_married<-1
    while(count_married <=2 & idx_married <=ncol(ed.knn)) { 
      if(!is.na(new_data@data$X..in.married_couple[row_data_married[idx_married]])) {
        print(row_data_married[idx_married])
        imputed_value_married<-append(value_married,new_data@data$X..in.married_couple[row_data_married[idx_married]],after=length(value_married))
        count_married <- count_married + 1 
        
      }
      
      
      idx_married <- idx_married + 1
      
    } 
  
    imputed_value_married<- mean(value_married)
   
    new_data@data$X..in.married_couple[missing_points_married[i]]  <- imputed_value_married
    
  }
}
### Rechecking if any missing data
missing_index_married_recheck<- data.frame(which(is.na(new_data$X..in.married_couple)))

print(nrow(missing_index_married_recheck))



##############################################
#
#  % ECE Enrolled
##############################################

# getting indices of rows where null values

missing_index_ece<- data.frame(which(is.na(new_data$X..ECE.enrolled)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_index_ece)){
  print("Yes, empty")
} else{
  for( i in missing_index_ece)
  {
    
    nearest_ece <- ed.knn[i,]
    
  }
  
  missing_points_ece<-as.numeric(rownames(nearest_ece))
  missing_points_ece
  
  # for rows in missing values and for values in row values
  
  df_ece <- data.frame(matrix(unlist(nearest_ece), nrow=length(nearest_ece), byrow=T))

  for ( i in 1:length(missing_points_ece)){
    
    row_data_ece<-df_ece[,i]
    
    imputed_value_ece<- 0
    idx_ece <- 1
    value_ece<- vector("numeric")
    count_ece<-1
    while(count_ece <=5  & idx_ece <=ncol(ed.knn)) { 
      if(!is.na(new_data@data$X..ECE.enrolled[row_data_ece[idx_ece]])) {
        print(row_data_ece[idx_ece])
        value_ece<-append(value_ece,new_data@data$X..ECE.enrolled[row_data_ece[idx_ece]],after = length(value_ece))
        count_ece <- count_ece + 1 
        
      }
      
      idx_ece <- idx_ece + 1
      
    } 
    
    imputed_value_ece<- mean(value_ece)
    
    new_data@data$X..ECE.enrolled[missing_points_ece[i]]  <- imputed_value_ece
    
  }
}
### Rechecking if any missing data
missing_index_ece_recheck<- data.frame(which(is.na(new_data$X..ECE.enrolled)))

print(missing_index_ece_recheck)

##############################################   
# 
#  Imputing Student Teacher Ratio- Median
##########################################

# getting indices of rows where null values

missing_index_stu_ratio<- data.frame(which(is.na(new_data$student_teacher_ratio)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_index_stu_ratio)){
  print("Yes, empty")
} else{
for( i in missing_index_stu_ratio)
{
  
  nearest_stu_ratio <- ed.knn[i,]
  
}

missing_points_stu_ratio<-as.numeric(rownames(nearest_stu_ratio))
missing_points_stu_ratio


# for rows in missing values and for values in row values

df_stu_ratio <- data.frame(matrix(unlist(nearest_stu_ratio), nrow=length(nearest_stu_ratio), byrow=T))

for ( i in 1:length(missing_points_stu_ratio)){
  
  row_data_stu_ratio<-df_stu_ratio[,i]

  value_stu_ratio<- vector('numeric')
  idx_stu <- 1
  
  count_stu<-1
  while(count_stu <=5 & idx_stu <=ncol(ed.knn)) { 
    if(!is.na(new_data@data$student_teacher_ratio[row_data_stu_ratio[idx_stu]])) {
      print(row_data_stu_ratio[idx_stu])
      value_stu_ratio<-append(value_stu_ratio,new_data@data$student_teacher_ratio[row_data_stu_ratio[idx_stu]])
      count_stu <- count_stu + 1 
      
    }
    
    
    idx_stu <- idx_stu + 1
    
  } 
  imputed_value_stu_ratio<- median(value_stu_ratio)
  print(imputed_value_stu_ratio)

  new_data@data$student_teacher_ratio[missing_points_stu_ratio[i]]  <- imputed_value_stu_ratio
  
}
}
### Rechecking if any missing data
missing_index_stu_recheck<- data.frame(which(is.na(new_data$student_teacher_ratio)))

print(missing_index_stu_recheck)

################################
# Imputing Free Lunch - Mean
#################################


# getting indices of rows where null values


missing_free_lunch_ratio<- data.frame(which(is.na(new_data$free_lunch)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_free_lunch_ratio)){
  print("Yes, empty")
} else{
  for( i in missing_free_lunch_ratio)
  {
    
    nearest_free_lunch_ratio <- ed.knn[i,]
    
  }
  
  missing_points_free_lunch_ratio<-as.numeric(rownames(nearest_free_lunch_ratio))
  missing_points_free_lunch_ratio
  
  
  # for rows in missing values and for values in row values
  
  df_free_lunch_ratio <- data.frame(matrix(unlist(nearest_free_lunch_ratio), nrow=length(nearest_free_lunch_ratio), byrow=T))
  
  for ( i in 1:length(missing_points_free_lunch_ratio)){
 
    row_data_free_lunch_ratio<-df_free_lunch_ratio[,i]
    
    value_free_lunch_ratio<- vector("numeric")
    idx_free_lunch <- 1
    
    count_free_lunch<-1
    while(count_free_lunch <=5 & idx_free_lunch <=ncol(ed.knn)) { 
      if(!is.na(new_data@data$free_lunch[row_data_free_lunch_ratio[idx_free_lunch]])) {
        print(row_data_free_lunch_ratio[idx_free_lunch])
        value_free_lunch_ratio<-append(value_free_lunch_ratio,new_data@data$free_lunch[row_data_free_lunch_ratio[idx_free_lunch]], after = length(value_free_lunch_ratio))
        count_free_lunch <- count_free_lunch + 1 
        
      }
      
      
      idx_free_lunch <- idx_free_lunch + 1
      
    } 
    
    imputed_value_free_lunch_ratio<- mean(value_free_lunch_ratio)

    new_data@data$free_lunch[missing_points_free_lunch_ratio[i]]  <- imputed_value_free_lunch_ratio
    
  }
}
### Rechecking if any missing data
missing_index_free_lunch_recheck<- data.frame(which(is.na(new_data$free_lunch)))

print(missing_index_free_lunch_recheck)


################################
# Imputing Reduced Lunch
###########################

missing_reduced_lunch_ratio<- data.frame(which(is.na(new_data$reduced_lunch)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_reduced_lunch_ratio)){
  print("Yes, empty")
} else{
  for( i in missing_reduced_lunch_ratio)
  {
    
    nearest_reduced_lunch_ratio <- ed.knn[i,]
    
  }
  
  missing_points_reduced_lunch_ratio<-as.numeric(rownames(nearest_reduced_lunch_ratio))
  missing_points_reduced_lunch_ratio
  
  
  # for rows in missing values and for values in row values
  
  df_reduced_lunch_ratio <- data.frame(matrix(unlist(nearest_reduced_lunch_ratio), nrow=length(nearest_reduced_lunch_ratio), byrow=T))
  
 
  
  
  for ( i in 1:length(missing_points_reduced_lunch_ratio)){

    row_data_reduced_lunch_ratio<-df_reduced_lunch_ratio[,i]
 
    
    value_reduced_lunch_ratio<- vector("numeric")
    idx_rlunch <- 1
    
    count<-1
    while(count <=5 & idx <=ncol(ed.knn)) { 
      if(!is.na(new_data@data$reduced_lunch[row_data_reduced_lunch_ratio[idx_rlunch]])) {
        print(row_data_reduced_lunch_ratio[idx_rlunch])
        value_reduced_lunch_ratio<-append(value_reduced_lunch_ratio,new_data@data$reduced_lunch[row_data_reduced_lunch_ratio[idx_rlunch]],after = length(value_reduced_lunch_ratio))
        count <- count + 1 
        
      }
      
      
      idx_rlunch <- idx_rlunch + 1
      
    } 
    
    imputed_value_reduced_lunch_ratio<- mean(value_reduced_lunch_ratio)
 
    new_data@data$reduced_lunch[missing_points_reduced_lunch_ratio[i]]  <- imputed_value_reduced_lunch_ratio
    
  }
}

### Rechecking if any missing data
missing_index_reduced_lunch_recheck<- data.frame(which(is.na(new_data$reduced_lunch)))

print(missing_index_reduced_lunch_recheck)
################################
# Imputing Title 1 school
###########################

missing_Title1_school_ratio<- data.frame(which(is.na(new_data$Title1_school)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_Title1_school_ratio)){
  print("Yes, empty")
} else{
  for( i in missing_Title1_school_ratio)
  {
    
    nearest_Title1_school_ratio <- ed.knn[i,]
    
  }
  
  missing_points_Title1_school_ratio<-as.numeric(rownames(nearest_Title1_school_ratio))
  missing_points_Title1_school_ratio
  
  
  # for rows in missing values and for values in row values
  
  df_Title1_school_ratio <- data.frame(matrix(unlist(nearest_Title1_school_ratio), nrow=length(nearest_Title1_school_ratio), byrow=T))

  for ( i in 1:length(missing_points_Title1_school_ratio)){
   
    row_data_Title1_school_ratio<-df_Title1_school_ratio[,i]
    
    value_Title1_school_ratio<- vector("numeric")
    idx_title <- 1
    
    count_title<-1
    while(count_title <=5 & idx_title <=ncol(ed.knn)) { 
      if(!is.na(new_data@data$Title1_school[row_data_Title1_school_ratio[idx_title]])) {
        print(row_data_Title1_school_ratio[idx_title])
        value_Title1_school_ratio<-append(value_Title1_school_ratio,new_data@data$Title1_school[row_data_Title1_school_ratio[idx_title]])
        count_title <- count_title + 1 
        
      }
      
      
      idx_title <- idx_title + 1
      
    } 
    
    imputed_value_Title1_school_ratio<- median(value_Title1_school_ratio)
   
    new_data@data$Title1_school[missing_points_Title1_school_ratio[i]]  <- imputed_value_Title1_school_ratio
    
  }
}

### Rechecking if any missing data
missing_index_title1_recheck<- data.frame(which(is.na(new_data$Title1_school)))

print(missing_index_title1_recheck)

####################################
# Imputing High ECE Centres - Median
#####################################

missing_high_quality_ECE_centers<- data.frame(which(is.na(new_data$high_quality_ECE_centers)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_high_quality_ECE_centers)){
  print("Yes, empty")
} else{
  for( i in missing_high_quality_ECE_centers)
  {
    
    nearest_high_quality_ECE_centers <- ed.knn[i,]
    
  }
  
  missing_points_high_quality_ECE_centers<-as.numeric(rownames(nearest_high_quality_ECE_centers))
  missing_points_high_quality_ECE_centers
  
  
  # for rows in missing values and for values in row values
  
  df_high_quality_ECE_centers <- data.frame(matrix(unlist(nearest_high_quality_ECE_centers), nrow=length(nearest_high_quality_ECE_centers), byrow=T))
  
  for ( i in 1:length(missing_points_high_quality_ECE_centers)){
    
    row_data_high_quality_ECE_centers<-df_high_quality_ECE_centers[,i]

    
    value_high_quality_ECE_centers<- vector("numeric")
    idx_Hece <- 1
    
    count_hece<-1
    while(count_hece <=5 & idx_Hece <=ncol(ed.knn)) { 
      if(!is.na(new_data@data$high_quality_ECE_centers[row_data_high_quality_ECE_centers[idx_Hece]])) {
        print(row_data_high_quality_ECE_centers[idx_Hece])
        value_high_quality_ECE_centers<-append(value_high_quality_ECE_centers,new_data@data$high_quality_ECE_centers[row_data_high_quality_ECE_centers[idx_Hece]],after = length(value_high_quality_ECE_centers))
        count_hece <- count_hece + 1 
        
      }
      
      
      idx_Hece <- idx_Hece + 1
      
    } 
    
    imputed_value_high_quality_ECE_centers<- median(value_high_quality_ECE_centers)
  
    new_data@data$high_quality_ECE_centers[missing_points_high_quality_ECE_centers[i]]  <- imputed_value_high_quality_ECE_centers
    
  }
}

### Rechecking if any missing data
missing_index_high_ECE_recheck<- data.frame(which(is.na(new_data$high_quality_ECE_centers)))

print(missing_index_high_ECE_recheck)


###################################
# Imputing Math Proficiency -Median
###################################
missing_math.proficiency<- data.frame(which(is.na(new_data$math.proficiency)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_math.proficiency)){
  print("Yes, empty")
} else{
  for( i in missing_math.proficiency)
  {
    
    nearest_math.proficiency<- ed.knn[i,]
    
  }
  
  missing_points_math.proficiency<-as.numeric(rownames(nearest_math.proficiency))
  missing_points_math.proficiency
  
  
  # for rows in missing values and for values in row values
  
  df_math.proficiency <- data.frame(matrix(unlist(nearest_math.proficiency), nrow=length(nearest_math.proficiency), byrow=T))
  
  for ( i in 1:length(missing_points_math.proficiency)){
  
    row_data_math.proficiency<-df_math.proficiency[,i]
  
    value_math.proficiency<- vector("numeric")
    idx_math <- 1
    
    count_math<-1
    while(count_math <=5 & idx_math <=ncol(ed.knn)) { 
      if(!is.na(new_data@data$math.proficiency[row_data_math.proficiency[idx_math]])) {
        print(row_data_math.proficiency[idx_math])
        value_math.proficiency<-append(value_math.proficiency,new_data@data$math.proficiency[row_data_math.proficiency[idx_math]],after=length(value_math.proficiency))
        count_math <- count_math + 1 
        
      }
      
      
      idx_math <- idx_math + 1
      
    } 
    
    imputed_value_math.proficiency<- median(value_math.proficiency)
    
    new_data@data$math.proficiency[missing_points_math.proficiency[i]]  <- imputed_value_math.proficiency
    
  }
}
### Rechecking if any missing data
missing_index_math_recheck<- data.frame(which(is.na(new_data$math.proficiency)))

print(missing_index_math_recheck)


#######################################
# Imputing Reading Proficiency - Median
#######################################

missing_reading.proficiency<- data.frame(which(is.na(new_data$reading.proficiency)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_reading.proficiency)){
  print("Yes, empty")
} else{
  for( i in missing_reading.proficiency)
  {
    
    nearest_reading.proficiency<- ed.knn[i,]
    
  }
  
  missing_points_reading.proficiency<-as.numeric(rownames(nearest_reading.proficiency))
  missing_points_reading.proficiency
  
  
  # for rows in missing values and for values in row values
  
  df_reading.proficiency <- data.frame(matrix(unlist(nearest_reading.proficiency), nrow=length(nearest_reading.proficiency), byrow=T))
  
  for ( i in 1:length(missing_points_reading.proficiency)){
  
    row_data_reading.proficiency<-df_reading.proficiency[,i]
    
    value_reading.proficiency<- vector("numeric")
    idx_read <- 1
    
    count_read<-1
    while(count_read <=5) { 
      if(!is.na(new_data@data$reading.proficiency[row_data_reading.proficiency[idx_read]])) {
        print(row_data_reading.proficiency[idx_read])
        value_reading.proficiency<-append(value_reading.proficiency,new_data@data$reading.proficiency[row_data_reading.proficiency[idx_read]])
        count_read <- count_read + 1 
        
      }
      
      
      idx_read <- idx_read + 1
      
    } 
    
    imputed_value_reading.proficiency<- median(value_reading.proficiency)
   
    new_data@data$reading.proficiency[missing_points_reading.proficiency[i]]  <- imputed_value_reading.proficiency
    
  }
}



### Rechecking if any missing data
missing_index_reading.proficiency_recheck<- data.frame(which(is.na(new_data$reading.proficiency)))

print(missing_index_reading.proficiency_recheck)


#############################################################################################################################################
##
##                         IMPUTING TARGET VARIABLES
##
#############################################################################################################################################




################################
# Imputing Median Income -Median
################################


missing_median_income<- data.frame(which(is.na(new_data$med_hhinc2016)))   ##look these indices up and find data for the nearest neighbour indices and get the mean


if (is_empty(missing_median_income)){
  print("Yes, empty")
} else{
  for( i in missing_median_income)
  {
    
    nearest_median_income<- ed.knn[i,]
    
  }
  
  missing_points_median_income<-as.numeric(rownames(nearest_median_income))
  missing_points_median_income
  
  
  # for rows in missing values and for values in row values
  
  df_median_income <- data.frame(matrix(unlist(nearest_median_income), nrow=length(nearest_median_income), byrow=T))
  
  
  
  
  for ( i in 1:length(missing_points_median_income)){
  
    row_data_median_income<-df_median_income[,i]
   
    
    value_median_income<- vector("numeric")
    idx_inc <- 1
    
    count_inc<-1
    while(count_inc <=4 & idx_inc <=ncol(ed.knn)){
       
      if(!is.na(new_data@data$med_hhinc2016[row_data_median_income[idx_inc]])) {
       
        print(row_data_median_income[idx_inc])
        value_median_income<-append(value_median_income,new_data@data$med_hhinc2016[row_data_median_income[idx_inc]],after=length(value_median_income))
        count_inc <- count_inc + 1 
        
      }
      
      
      idx_inc <- idx_inc + 1
      
    } 
   
    imputed_value_median_income<- median(value_median_income)
    
    new_data@data$med_hhinc2016[missing_points_median_income[i]]  <- imputed_value_median_income
    
  }
}


### Rechecking if any missing data
missing_index_median_income_recheck<- data.frame(which(is.na(new_data$med_hhinc2016)))

print(missing_index_median_income_recheck)

####################################
# Imputing Poverty Probability InDX-Median
####################################

missing_Poverty_prob_index<- data.frame(which(is.na(new_data$Poverty_prob_index)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_Poverty_prob_index)){
  print("Yes, empty")
} else{
  for( i in missing_Poverty_prob_index)
  {
    
    nearest_Poverty_prob_index<- ed.knn[i,]
    
  }
  
  missing_points_Poverty_prob_index<-as.numeric(rownames(nearest_Poverty_prob_index))
  missing_points_Poverty_prob_index
  
  
  # for rows in missing values and for values in row values
  
  df_Poverty_prob_index <- data.frame(matrix(unlist(nearest_Poverty_prob_index), nrow=length(nearest_Poverty_prob_index), byrow=T))
  
  length_missing_points_Poverty_prob_index=length(missing_points_Poverty_prob_index)
  for ( i in 1:length_missing_points_Poverty_prob_index){
    
    row_data_Poverty_prob_index<-df_Poverty_prob_index[,i]

    
    value_Poverty_prob_index<- vector("numeric")
    idx_prob <- 1
    
    count_prob<-1
    while(count_prob <=4 & idx_prob <=ncol(ed.knn)) { 
      
        if(!is.na(new_data@data$Poverty_prob_index[row_data_Poverty_prob_index[idx_prob]])) {
          print(row_data_Poverty_prob_index[idx_prob])
          value_Poverty_prob_index<-append(value_Poverty_prob_index,new_data@data$Poverty_prob_index[row_data_Poverty_prob_index[idx_prob]],after = length(value_Poverty_prob_index))
        
          count_prob <- count_prob + 1 
        
        
      }
      
     
      idx_prob <- idx_prob + 1
      
    } 
    
    imputed_value_Poverty_prob_index<- median(value_Poverty_prob_index)
    
    new_data@data$Poverty_prob_index[missing_points_Poverty_prob_index[i]]  <- imputed_value_Poverty_prob_index
    
  }
}

### Rechecking if any missing data
missing_index_povertyprob_recheck<- data.frame(which(is.na(new_data$Poverty_prob_index)))

print(missing_index_povertyprob_recheck)

################################
# Imputing Mental Health 
################################


missing_mental_health<- data.frame(which(is.na(new_data$MHLTH)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

if (is_empty(missing_mental_health)){
  print("Yes, empty")
} else{
  for( i in missing_mental_health)
  {
    
    nearest_mental_health<- ed.knn[i,]
    
  }
  
  missing_points_mental_health<-as.numeric(rownames(nearest_mental_health))
  missing_points_mental_health
  
  
  # for rows in missing values and for values in row values
  
  df_mental_health <- data.frame(matrix(unlist(nearest_mental_health), nrow=length(nearest_mental_health), byrow=T))
  
  
  
  
  for ( i in 1:length(missing_points_mental_health)){

    row_data_mental_health<-df_mental_health[,i]
 
    
    value_mental_health<- vector("numeric")
    idx_mlth <- 1
    
    count_mlth<-1
    while(count_mlth <=4 & idx_mlth <=ncol(ed.knn)) { 
     
      if(!is.na(new_data@data$MHLTH[row_data_mental_health[idx_mlth]])) {
        print(row_data_mental_health[idx_mlth])
        value_mental_health<-append(value_mental_health,new_data@data$MHLTH[row_data_mental_health[idx_mlth]],after=length(value_mental_health))
        count_mlth <- count_mlth + 1 
        
      }
      
      
      idx_mlth <- idx_mlth + 1
      
    } 
    
    imputed_value_mental_health<- mean(value_mental_health)

    new_data@data$MHLTH[missing_points_mental_health[i]]  <- imputed_value_mental_health
    
  }
}

### Rechecking if any missing data
missing_index_mentalhlth_recheck<- data.frame(which(is.na(new_data$MHLTH)))

print(missing_index_mentalhlth_recheck)

################################
# Imputing Physical Health
################################

# getting indices of rows where null values


missing_index_phy<- data.frame(which(is.na(new_data$PHLTH)))   ##look these indices up and find data for the nearest neighbour indices and get the mean

# Checking if we have missing data in the column

if (is_empty(missing_index_phy)){
  print("Yes, empty")
} else{             
  for( i in missing_index_phy)
  {
    
    nearest_phy <- ed.knn[i,]
    
  }
  ## Here nearest has the neigbours for missing columns in School Poverty
  
  # Retaining the actual missing points here  
  missing_points_phy<-as.numeric(rownames(nearest_phy))
  missing_points_phy
  
  ## We need to access the row values to find corressponding data in the actual data. COnverting to dataframe for easy access
  
  df_phy <- data.frame(matrix(unlist(nearest_phy), nrow=length(nearest_phy), byrow=T))
  
  ## This section  looks up the data for the neighbours 
  ## and calculates the mean value of 5 nearest neighbours where data is present and imputes our missing value
  
  for ( i in 1:length(missing_points_phy)){
    
    row_data_phy<-df_phy[,i]
 
    
    value_phy<- vector("numeric")
    idx_phy <- 1
    
    count<-1
    while(count <=4 & idx_phy <=ncol(ed.knn)) { 
      if(!is.na(new_data@data$PHLTH[row_data_phy[idx_phy]])) {
        print(row_data_phy[idx_phy])
        value_phy<-append(value_phy,new_data@data$PHLTH[row_data_phy[idx_phy]],after=length(value_phy))
        count <- count + 1 
        
      }
      
      idx_phy <- idx_phy + 1
      
    } 
    
    imputed_value_phy<- mean(value_phy)
   
    new_data@data$PHLTH[missing_points_phy[i]]  <- imputed_value_phy
    
  }
}

### Rechecking if any missing data(Basically checking if values are imputed)
missing_index_phy_recheck<- data.frame(which(is.na(new_data$PHLTH)))

print(missing_index_phy_recheck)
##########################################################
#           CONVERTING TO CSV
#
######################################################

write.csv(new_data[1:16], "KNN_Imputed_Dataset.csv",row.names=FALSE)






