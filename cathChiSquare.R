##Script Written By Mihir Trivedi
#################################

##loading data
  library(readr)
  cathData <- read_csv("F:/cath/cathCasesRaceCorrected.csv")
  View(cathData)
  
  ##removing weird line in data
  cathData<-cathData[-1,]
  
  ##reconfirming correct dataset/numbers correspond with those in the abstract
  table(cathData$caseindicator, cathData$indicatorforexclusion)

##Creating Chi-Square Tests of Independence to Generate Some P-Values For Tables

library(MASS)
chiSquares<-list()

chiSquareTestGen<-function(categoryOne, categoryTwo, nameTable){
  
  nameTable<-table(categoryOne, categoryTwo)

  
  return(chisq.test(nameTable))
}


categoryTwoList<-c("AGECAT","SEX","RACE_RECODE","INSURANCE","Mor_Teach","county2","ANEMIA","CERVASD","CANCER","COPD","AFIB","RENAL","DIABETES","HYPERTENSION")
categoryOneList<-rep("cathData$caseindicator", length(categoryTwoList))

prefix<-rep("cathData$", length(categoryTwoList))
tableNames <- paste(categoryTwoList, rep("table",length(categoryTwoList)), sep = "")
categoryTwoList<-paste(prefix, categoryTwoList, sep ="")

chiSquares<-lapply(c(1:length(categoryTwoList)), function(x){
  
  chiSquareTestGen(eval(parse(text = categoryOneList[x])), eval(parse(text = categoryTwoList[x])),tableNames[x])
  
})


##extracting pvalues from chisquares may be of use for tables

cstPvalues<-sapply(c(1:length(chiSquares)), function(x){
  chiSquares[[x]]$p.value
}) 


##variables of interest
chiSquares  ## chisquare Tests

cstPvalues  ## Pvalues 

categoryTwoList  ##Variable Names
