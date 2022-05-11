######
library(mice)
library(tidyverse)
library(rcompanion)
library(countrycode)
library(stringr)
library(SignifReg)
library(randomForest)
library(corrr)

dt <- read.csv("openpowerlifting.csv", header = TRUE)
attach(dt)

##check for missing data
MissingRatio <- function(x){sum(is.na(x))/length(x)*100}
apply(dt,2,MissingRatio)

##lots of missing values
##let's focus on the easy fixes
##According to the data description, if a contestant does not even attempt a lift it is not counted at all. Therefore we can simply fill in these data points with 0's as they effectively lifted nothing.

dt$Squat1Kg[is.na(dt$Squat1Kg)] <- 0
dt$Squat2Kg[is.na(dt$Squat2Kg)] <- 0
dt$Squat3Kg[is.na(dt$Squat3Kg)] <- 0
dt$Squat4Kg[is.na(dt$Squat4Kg)] <- 0

dt$Bench1Kg[is.na(dt$Bench1Kg)] <- 0
dt$Bench2Kg[is.na(dt$Bench2Kg)] <- 0
dt$Bench3Kg[is.na(dt$Bench3Kg)] <- 0
dt$Bench4Kg[is.na(dt$Bench4Kg)] <- 0


dt$Deadlift1Kg[is.na(dt$Deadlift1Kg)] <- 0
dt$Deadlift2Kg[is.na(dt$Deadlift2Kg)] <- 0
dt$Deadlift3Kg[is.na(dt$Deadlift3Kg)] <- 0
dt$Deadlift4Kg[is.na(dt$Deadlift4Kg)] <- 0

dt <- dt %>% 
  mutate(MaxDeadlift = pmax(dt$Deadlift1Kg, dt$Deadlift2Kg, dt$Deadlift3Kg, dt$Deadlift4Kg))

dt <- subset(dt, select = -c(Deadlift1Kg, Deadlift2Kg, Deadlift3Kg, Deadlift4Kg, Best3DeadliftKg))

#for the best of 3 examples, we can check what the max lift the contestest posted was and fill it with that. 
dt <- dt %>% 
  mutate(MaxSquat = pmax(dt$Squat1Kg, dt$Squat2Kg, dt$Squat3Kg, dt$Squat4Kg))

#similarly for the maxbench
dt <- dt %>% 
  mutate(MaxBench = pmax(dt$Bench1Kg, dt$Bench2Kg, dt$Bench3Kg, dt$Bench4Kg))

#remove unecessary columns now, the max columns contain all the information we need.
dt <- subset(dt, select = -c(Bench1Kg, Bench2Kg, Bench3Kg, Bench4Kg, Squat1Kg, Squat2Kg, Squat3Kg, Squat4Kg, Best3SquatKg, Best3BenchKg))

##convert to factors
dt[sapply(dt, is.character)] <- lapply(dt[sapply(dt, is.character)], as.factor)

##check levels
sapply(dt, levels)

#name is an identifier and won't help our cause, so we can go ahead and remove it
dt <- subset(dt, select = -c(Name))

#TotalKg is also going to give us the MaxDeadlift, so we must remove that as well
dt <- subset(dt, select = -c(TotalKg))

mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  map2_df(df_comb$X1, df_comb$X2, f)
}


#Simplify Dates to decades
dt$Date <- substring(dt$Date, 1, 4)
dt$Date <- as.numeric(as.character(dt$Date))
dt <- dt %>%
      mutate(Decade = case_when(
       Date <= 2020 & Date >= 2010 ~ "2010-2020",
       Date < 2010 & Date >= 2000 ~ "2000-2010",
       Date < 2000 & Date >= 1990 ~ "1990-2000",
       Date < 1990 & Date >= 1980 ~ "1980-1990",
       Date < 1980 & Date >= 1970 ~ "1970-1980",
       Date < 1970 & Date >= 1960 ~ "1960-1970"
  ) %>%
  as.factor()  
)

dt <- subset(dt, select = -c(Date))

#these levels are just too big, simplifying them would take too long to be considered worthwhile.
dt <- subset(dt, select = -c(MeetName))
dt <- subset(dt, select = -c(Division))

#these countries are not the same, but i am just trying to fit them all into Europe with the function, and they are all in Europe so it doesn't matter in the end.
levels(dt$Country)[match("England",levels(dt$Country))] <- "United Kingdom"
levels(dt$Country)[match("Scotland",levels(dt$Country))] <- "United Kingdom"
levels(dt$Country)[match("Serbia and Montenegro",levels(dt$Country))] <- "United Kingdom"
levels(dt$Country)[match("Transnistria",levels(dt$Country))] <- "United Kingdom"
levels(dt$Country)[match("Wales",levels(dt$Country))] <- "United Kingdom"
levels(dt$Country)[match("Yugoslavia",levels(dt$Country))] <- "United Kingdom"

dt$Region <- countrycode(sourcevar = dt[, "Country"], origin = "country.name", destination = "continent")
dt$Region <- as.factor(dt$Region)

#remove Countries
dt <- subset(dt, select = -c(Country))

#check BodyweightKg and WeightClassKg
summary(lm(BodyweightKg~WeightClassKg)) 
#R^2 of .9649, almost the same
dt <- subset(dt, select = -c(WeightClassKg))

cramerV(dt$MeetCountry, dt$Federation)
#pretty high at .6217, i would think to keep it but looking for reason to drop Federation
dt <- subset(dt, select = -c(Federation))

#impute as many Age as possible from the median of AgeClass
dt$AgeClass1 <- as.numeric(substring(dt$AgeClass, 1, 2))
dt$AgeClass2 <- as.numeric(str_sub(dt$AgeClass, -2, -1))
dt <- dt %>%
  rowwise() %>% 
  mutate(AgeClassMedian = median(c(AgeClass1, AgeClass2), na.rm = TRUE)) %>%
  ungroup

dt$Age <- ifelse(dt$Age == "", dt$AgeClassMedian, dt$Age)

#run a chi-square test
cramerV(Age, AgeClass)

#0.7295 correlation coefficient, they are almost the same.
dt <- subset(dt, select = -c(AgeClass))

#remove all children from the data, bigger question is why are 5 year olds deadlifting 
dt <- subset(dt, dt$Age > 7)
dt <- subset(dt, select = -c(AgeClass1, AgeClass2, AgeClassMedian))

#computer can just not run the model, need to remove Place and MeetState and MeetCountry
dt <- subset(dt, select = -c(MeetState, MeetCountry, Place))


dt_assoc <- mixed_assoc(dt)
checkAssoc <- dt_assoc[dt_assoc$assoc > .7, ]
checkAssoc

dt_assoc %>%
  select(x, y, assoc) %>%
  spread(y, assoc) %>%
  column_to_rownames("x") %>%
  as.matrix %>%
  as_cordf %>%
  network_plot()

dt <- dt[dt$MaxDeadlift > 0, ]


#since these columns are colinear, we should just remove the ones with NA's 
dt <- subset(dt, select = -c(Wilks, McCulloch, Glossbrenner, IPFPoints, MaxBench, Region))

mice <- mice(dt, m = 10, method = "pmm")
dt <- complete(mice, 1)

nullmodel = lm(MaxDeadlift~1, dt)
fullmodel = lm(MaxDeadlift~., dt)
scope = list(lower=formula(nullmodel),upper=formula(fullmodel))


fit1 <- nullmodel
forward_model <- SignifReg(fit1, scope = scope, direction = "forward")

fit2 <- fullmodel
backward_model <- SignifReg(fit2, scope = scope, direction = "backward")

n = length(dt$MaxDeadlift)

PRESS1 = rep(0,100)
PRESS3 = rep(0,100)

for (i in 1:500)
{
  
  ## 70\% of data to training dataset
  indices = sample(1:n, 0.70*n)
  train_data = dt[indices,]
  
  ## 30\% of data to testing dataset
  test_data = dt[setdiff(1:n, indices),]
  
  
  # renmove mpg from test_data
  test_data_without_y = subset(test_data, select = - c(MaxDeadlift))
  
  PRESS1[i] = sum((predict(forward_model,test_data_without_y ) - test_data$MaxDeadlift)^2)
  PRESS3[i] = sum((predict(backward_model,test_data_without_y ) - test_data$MaxDeadlift)^2)
  
  
  
}


mean(PRESS1) 
mean(PRESS3) 

