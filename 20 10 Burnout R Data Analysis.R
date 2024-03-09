library(ggplot2)
library(corrplot)
library(dplyr)
library(factoextra)
library(tibble)
library(lubridate)
library(tidyverse)
library(reshape2)
library(skimr)
library(dbscan)

#Import Feb. EMR data
# RawOrigFebData <- read.csv("C:/Users/james/20 Burnout/20 02 Burnout Cerner Data.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")
# RawOrigFebData <- RawOrigFebData %>% select_all(~gsub("\\s+|\\.", "", .)) %>% select_all(tolower)
# str(RawOrigFebData)

RawFebDataAmbulatory <- read.csv("C:/Users/james/20 Burnout/20 02 Burnout Cerner Data Ambulatory.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")
str(RawFebDataAmbulatory)


#preview top 6 rows of data set
# head(RawOrigFebData)
# RawOrigFebData <- subset(RawOrigFebData, select = -date)
head(RawFebDataAmbulatory)


#check for NA's by column
# colSums(is.na(RawOrigFebData))
colSums(is.na(RawFebDataAmbulatory))   #no NA's


#Descriptive Analysis
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}  
# length(RawOrigFebData)
# length(RawOrigFebData[,1])
# summary(select_if(RawOrigFebData, is.numeric))

#Remove variables with no values greater than 0.017 (one minute), or with very low values (3rd quartile = 0)
# OrigFebData <- subset(RawOrigFebData, select = -c(HealthMaintTimePPH,MedRecTimePPH))

# names<-names(RawOrigFebData)
# classes<-sapply(RawOrigFebData,class)
# 
# for(name in names[classes == 'numeric']){
#   dev.new()
#   hist(RawOrigFebData[, name], main=paste(name))
#   boxplot(RawOrigFebData[, name] ~ RawOrigFebData[, "venue"], main=paste(name))
# }
# 
# ggplot(melt(RawOrigFebData),aes(x=value)) + geom_histogram() + facet_wrap(~variable)   #histograms
# 
# for (name in names[classes == 'character']){    #Position Specialty and Specialty Group
#   cat("\n\nVariable: ", name)
#   print(table(RawOrigFebData[,name]))
#   
#   cat("\nMode: ", name)
#   print(Mode(RawOrigFebData[,name])) 
# }

#summary statistics
skim(RawFebDataAmbulatory)

#Remove time variables with maximum values (p100) < 0.017 (one minute), or with few non-zero values (p75= 3rd quartile = 0)
FebDataAmbulatory <- subset(RawFebDataAmbulatory, select = -c(HealthMaintTimePPH,MedRecTimePPH,TotalCrashCountPP,MobileNotessignedPP,MobileOrdersSignedPP))

ambnames<-names(FebDataAmbulatory)
ambclasses<-sapply(FebDataAmbulatory, class)
for (name in ambnames[ambclasses == 'character']){    #Position Specialty and Specialty Group
    cat("\n\nVariable: ", name)
    print(table(FebDataAmbulatory[,name]))
    cat("Mode: ", name)
    print(Mode(FebDataAmbulatory[,name]))
}
for(name in ambnames[ambclasses == 'numeric']){
    dev.new()
    hist(FebDataAmbulatory[, name], main=paste(name)) 
    dev.new()
    boxplot(FebDataAmbulatory[, name], main=paste(name))
}
graphics.off()
#ggplot(data= select_if(FebDataAmbulatory, is.numeric)) + geom_boxplot() + facet_wrap(variable)
##debug


#Removed physicians (n=5) spending > 0.5 total hours per patient
hist(FebDataAmbulatory[, "ActualTimePPH"], main=paste("ActualTimePPH"))
FebDataAmbulatory[FebDataAmbulatory$ActualTimePPH>0.5, ] #displays data for 5 physicians
FebDataAmbulatory <- FebDataAmbulatory[!(FebDataAmbulatory$ActualTimePPH>0.5), ]

#OverridePercentage and PercAmb converted from decimals to percentage
FebDataAmbulatory$OverridePercentage<-FebDataAmbulatory$OverridePercentage*100
FebDataAmbulatory$PercAmb           <-FebDataAmbulatory$PercAmb*100

#Categorize variables by EMR metric type
FebAmbTimesPerPatient <- FebDataAmbulatory %>% select(contains("PPH"))
FebAmbTasksPerPatient <- FebDataAmbulatory %>% select(ends_with("PP"))
FebAmbPercentage      <- FebDataAmbulatory %>% select(contains("Perc"))

#Frequency Polygon plots (density vs value)
ggplot(melt(FebAmbTimesPerPatient),aes(x=value, y=after_stat(density), colour = variable))+ 
  xlab("Hours") + labs(title = "Density of Times Per Patient variables") + geom_freqpoly() 
ggplot(melt(FebAmbTasksPerPatient),aes(x=value, y=after_stat(density), colour = variable))+ 
  xlab("Tasks") + labs(title = "Density of Tasks Per Patient variables")+ geom_freqpoly() 
ggplot(melt(FebAmbPercentage),     aes(x=value, y=after_stat(density), colour = variable))+ 
  xlab("Percent") + labs(title = "Density of Percentage variables")+ geom_freqpoly() 


#Correlation and covariance 
# OrigFebData.cor <- cor(OrigFebData[,-1:-3]) #Columns 1-3 are identifiers
# OrigFebData.cov <- cov(OrigFebData[,-1:-3])
FebData.cor <- cor(FebDataAmbulatory[,-1:-3]) #Columns 1-3 are identifiers
dev.new()
corrplot(FebData.cor, method = "color", tl.cex=1) 
##must change axis labels for visibility
FebData.cov <- cov(FebDataAmbulatory[,-1:-3])


#Run Principle components on correlation matrix to allow for normalization of variables. 
# OrigFebData.PCA <- prcomp(OrigFebData[,-1:-3])
# Origpca.plot <- autoplot(OrigFebData.PCA, data=OrigFebData, colour = 'specialtygroup', label=TRUE)
# Origpca.plot

FebAmbTimesPerPatient$SpecialtyGroup <- FebDataAmbulatory$SpecialtyGroup 
FebAmbTasksPerPatient$SpecialtyGroup <- FebDataAmbulatory$SpecialtyGroup
FebAmbPercentage$SpecialtyGroup <- FebDataAmbulatory$SpecialtyGroup
FebData.PCA <- prcomp(subset(FebAmbTimesPerPatient, select = -c(SpecialtyGroup)))
autoplot(FebData.PCA, data=FebAmbTimesPerPatient, colour = 'SpecialtyGroup', label=TRUE, title="PCA of Times Per Patient variables")
FebDataTask.PCA <- prcomp(subset(FebAmbTasksPerPatient, select = -c(SpecialtyGroup)))
autoplot(FebDataTask.PCA, data=FebAmbTasksPerPatient, colour = 'SpecialtyGroup', label=TRUE, title="PCA of Times Per Patient variables")
FebDataPerc.PCA <- prcomp(subset(FebAmbPercentage, select = -c(SpecialtyGroup)))
autoplot(FebDataPerc.PCA, data=FebAmbPercentage, colour = 'SpecialtyGroup', label=TRUE, title="PCA of Times Per Patient variables")

##dbscan
dbscan(FebAmbTimesPerPatient, 0.1)

#Run Kmeans to data?? Study more and make object oriented
FebAmbTimesPerPatient<- subset(FebAmbTimesPerPatient, select = -c(SpecialtyGroup))
FebAmbTimesPerPatient.Kmeans <-kmeans(FebAmbTimesPerPatient, centers = 2, nstart=25)
fviz_cluster(FebAmbTimesPerPatient.Kmeans, data= FebAmbTimesPerPatient)
FebAmbTasksPerPatient<- subset(FebAmbTasksPerPatient, select = -c(SpecialtyGroup))
FebAmbTasksPerPatient.Kmeans <-kmeans(FebAmbTasksPerPatient, centers = 2, nstart=25)
fviz_cluster(FebAmbTasksPerPatient.Kmeans, data= FebAmbTasksPerPatient)
FebAmbPercentage<- subset(FebAmbPercentage, select = -c(SpecialtyGroup))
FebAmbPercentage.Kmeans <-kmeans(FebAmbPercentage, centers = 2, nstart=25)
fviz_cluster(FebAmbPercentage.Kmeans, data= FebAmbPercentage)



#Distance Matrix visualized
# SpecialtyGroup <- OrigFebData[,-1:-2] %>% group_by(specialtygroup) %>% dplyr::summarize_all(funs(mean),na.rm=TRUE) %>% column_to_rownames(var="specialtygroup")
# distance <- get_dist(SpecialtyGroup) 
# fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

SpecialtyGroup <- FebDataAmbulatory[,-1:-2] %>% group_by(SpecialtyGroup) %>% dplyr::summarize_all(funs(mean),na.rm=TRUE) %>% column_to_rownames(var="SpecialtyGroup")
distance <- get_dist(SpecialtyGroup) 
fviz_dist(distance, gradient = list(low = "#62D26F", mid = "#FFDD2B", high = "#FD612C"))


