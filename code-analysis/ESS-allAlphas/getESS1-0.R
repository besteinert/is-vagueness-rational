library(gplots)
library(dplyr)
library(data.table)

### input data needs to be in the following long format:  | EU | types_S | types_L |
###                                                       --------------------------
###                                                       | ...| ...     | ...     | 
  
getESS <- function(path="../agentData_allAlphas.csv"){

### Read CSV into R
data <- read.csv(file=path, colClasses=c("numeric","character","character"), header = FALSE)
###

### Prepate Data
names(data)[1] <- "t1_mean"
names(data)[2] <- "t1_sigma"
names(data)[3] <- "t1_alpha"

names(data)[4] <- "t2_mean"
names(data)[5] <- "t2_sigma"
names(data)[6] <- "t2_alpha"

names(data)[7] <- "EU"

data$types_S <- paste("[",data$t1_mean,",",data$t1_sigma,",", data$t1_alpha , "]")
data$types_L <- paste("[",data$t2_mean,",",data$t2_sigma,",", data$t2_alpha ,"]")

data$t1_mean = NULL
data$t1_sigma = NULL
data$t1_alpha = NULL

data$t2_mean = NULL
data$t2_sigma = NULL
data$t2_alpha = NULL


###  EU(S,S) > EU(T,S) or
###  EU(S,S) == EU(S,T) and EU(S,T) > EU(T,T)
getWeakESS <- function(mat){
  result = data.frame(EU=numeric(), type1=character(), type2=character(), stringsAsFactors = FALSE)
  
  for(S in unique(mat$types_S)){
    
    SS = mat[(mat$types_S == S & mat$types_L == S),]
    
    # no difference in who is playing against who, because payoff is calculated like: 1/2(EU(t1,t2) + EU(t2,t1))
    TS = mat[(mat$types_L == S & mat$types_S != S),]
    ST = mat[(mat$types_S == S & mat$types_L != S),]
    TT = mat[(mat$types_S != S & mat$types_L != S),]
    
      if(SS$EU > max(TS$EU)) {
        result = rbind(result, SS)
      }
      else{
        for(T in unique(TT$types_S)){
          if((SS$EU == ST[ST$types_L == T,]$EU) & (ST[ST$types_L == T,]$EU > max(TT$EU))){
            result = rbind(result, SS)      
            print("2ndcond")
          }
        }
      }
  }
  
  return(result)
}


###  EU(S,S) >= EU(T,S) and
###  EU(S,T) > EU(T,T)
getStrongerESS <- function(mat){
  result = data.frame(EU=numeric(), type1=character(), type2=character(), stringsAsFactors = FALSE)
  
  for(S in unique(mat$types_S)){
    
      SS = mat[(mat$types_S == S & mat$types_L == S),]
      
      # no difference in who is playing againt who, because payoff is calculated like: 1/2(EU(t1,t2) + EU(t2,t1))
      TS = mat[(mat$types_L == S & mat$types_S != S),]
      ST = mat[(mat$types_S == S & mat$types_L != S),]
      TT = mat[(mat$types_S != S & mat$types_L != S),]
      
      if(SS$EU >= max(TS$EU)) {
        
        for(T in unique(TT$types_S)){
          if(ST[ST$types_L == T,]$EU > max(TT$EU)){
            result = rbind(result, SS)      

          }
        }
      }
  }
  return(result)
}

### save ESS in csv
weakESS = getWeakESS(data)
strongESS = getStrongerESS(data)
write.csv(weakESS, file="weakESS.csv", row.names = FALSE)
write.csv(strongESS, file="strongESS.csv", row.names = FALSE)

}
getESS()
