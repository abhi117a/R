adult <- read.csv("adult_sal.csv")
head(adult)
colnames(adult)
adult$X <- NULL
head(adult)
table(adult$type_employer)


adult[is.na(adult$type_employer),]
library(Amelia)
missmap(adult, col = c("yellow","black"), legend = F)

unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)
table(adult$type_employer)

group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,group_emp)


table(adult$type_employer)
table(adult$marital)


group_Marital <- function(mar){
  mar <- as.character(mar)
  
  if(mar=="Seprated"|| mar=="Divorced"|| mar == "Widowed"){
    return("Not-Married")
  }
  else if(mar == "Never-married"){
    return(mar)
  }
  else {
    return("Married")
  }
}
adult$marital <- sapply(adult$marital,group_Marital)


table(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if(ctry %in% Asia){
    return("Asia")
  }
  else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)

table(adult$country)


adult$type_employer <- factor(adult$type_employer)
adult$country <- factor(adult$country)
adult$marital <- factor(adult$marital)

str(adult)

adult[adult == "?"] <- NA
table(adult$type_employer)

adult

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

adult <- na.omit(adult)


str(adult)
