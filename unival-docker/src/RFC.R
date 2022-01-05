source("RforCFmeansRF.R")

# 3 RF
predCFoutRF <-
  function(dataFrame,
           outVarName,
           treatVarName,
           treatVal) {
    library("ranger")
    
    forest <-
      ranger(paste(outVarName, " ~ .", sep = ""), data = dataFrame)
    
    CFdata <- data.frame(dataFrame)
    
    CFdata[[treatVarName]] <-
      rep(treatVal, length(CFdata[[treatVarName]]))
    
    CFout <- predictions(predict(forest, CFdata))
    
    return(CFout)
    
  }

# 3 LM
predCFoutLM <-
  function(dataFrame,
           outVarName,
           treatVarName,
           treatVal) {
    M <- lm(paste(outVarName, " ~ .", sep = ""), data = dataFrame)
    
    CFdata <- data.frame(dataFrame)
    
    CFdata[[treatVarName]] <-
      rep(treatVal, length(CFdata[[treatVarName]]))
    
    CFout <- predict(M, CFdata)
    
    return(CFout)
    
  }

# 3 predict with lasso
predCFoutLM <-
  function(dataFrame,
           outVarName,
           treatVarName,
           treatVal) {
    library(glmnet)
    # newoutput <- apply(newoutput, 2, as.numeric)
    # Y <- apply(Y, 2, as.numeric)
    fit.glmnet <-
      cv.glmnet(x = data.matrix(dataFrame[,-1]),
                data.matrix(dataFrame[, 1]),
                family = "gaussian")
    
    CFdata <- data.frame(dataFrame)
    
    CFdata[[treatVarName]] <-
      rep(treatVal, length(CFdata[[treatVarName]]))
    
    
    # CFdata <- apply(CFdata, 2, as.numeric)
    CFout <-
      predict(fit.glmnet,
              newx = data.matrix(CFdata[,-1]),
              s = "lambda.min",
              type = 'response')
    
    # return(CFout);
  }
#prediction happens here
predCFprobRF <-
  function(dataFrame,
           outVarName,
           treatVarName,
           treatVal) {
    library("ranger")
    
    forest <-
      ranger(paste(outVarName, " ~ .", sep = ""),
             data = dataFrame,
             probability = TRUE)
    
    CFdata <- data.frame(dataFrame)
    
    CFdata[[treatVarName]] <-
      rep(treatVal, length(CFdata[[treatVarName]]))
    
    CFout <- predictions(predict(forest, data = CFdata))
    return(CFout[, 2])
  }

# 0
trainCFoutPredRF <- function(dataFrame, outVarName) {
  library("ranger")
  
  forest <-
    ranger(paste(outVarName, " ~ .", sep = ""), data = dataFrame)
  
  return(forest)
  
}

trainCFprobPredRF <- function(dataFrame, outVarName) {
  library("ranger")
  
  forest <-
    ranger(paste(outVarName, " ~ .", sep = ""),
           data = dataFrame,
           probability = TRUE)
  
  return(forest)
  
}

CFmeansForTreatRangeRF <-
  function(dataFrame,
           outVarName,
           treatVarName,
           treatVec,
           minTreat,
           maxTreat) {
    CFmeans <- rep(0, maxTreat - minTreat + 1)
    
    
    for (i in minTreat:maxTreat) {
      CFmeans[i] <-
        mean(predCFoutRF(dataFrame, outVarName, treatVarName, treatVec[i]))
      
    }
    
    return(CFmeans)
    
    
  }

# 2 RF
CFmeansForTreatVecRF <-
  function(dataFrame,
           outVarName,
           treatVarName,
           treatVec) {
    CFmeans <- rep(0, length(treatVec))
    
    
    
    for (i in 1:length(treatVec)) {
      CFmeans[i] <-
        mean(predCFoutRF(dataFrame, outVarName, treatVarName, treatVec[i]))
      
    }
    
    return(CFmeans)
    
    
  }

# 2 LM
CFmeansForTreatVecLM <-
  function(dataFrame,
           outVarName,
           treatVarName,
           treatVec) {
    CFmeans <- rep(0, length(treatVec))
    
    
    for (i in 1:length(treatVec)) {
      CFmeans[i] <-
        mean(predCFoutLM(dataFrame, outVarName, treatVarName, treatVec[i]))
      
    }
    
    return(CFmeans)
    
    
  }


CFprobsForTreatVecRF <-
  function(dataFrame,
           outVarName,
           treatVarName,
           treatVec) {
    CFprobs <- rep(0, length(treatVec))
    
    
    for (i in 1:length(treatVec)) {
      CFprobs[i] <-
        mean(predCFprobRF(dataFrame, outVarName, treatVarName, treatVec[i]))
      
    }
    
    return(CFprobs)
    
    
  }



# 1 LM for lasso
# CFmeansForDecileBinsLM <- function(dataFrame, outVarName, treatVarName) {
#   fivePercentQuantiles <- quantile(dataFrame[[treatVarName]], prob = seq(0, 1, length = 21), type = 5, na.rm = TRUE)
#   evenQuantiles <- fivePercentQuantiles[seq(2, 20, by=2)]
#
#   vec <- dataFrame[c(treatVarName)]
#   average <- mean(vec[,1])
#   s = 0
#   for (i in 1:nrow(dataFrame[c(treatVarName)])){
#     temp <- vec[i,]
#     s = s + abs(temp - average);
#   }
#   if(s > 0){
#     return(CFmeansForTreatVecLM(dataFrame, outVarName, treatVarName, evenQuantiles))
#   }else{
#     return(-1)
#   }
# }

#1 LM for Lasso
CFmeansForDecileBinsLM <-
  function(dataFrame, outVarName, treatVarName) {
    fivePercentQuantiles <-
      quantile(
        dataFrame[[treatVarName]],
        prob = seq(0, 1, length = 21),
        type = 5,
        na.rm = TRUE
      )
    evenQuantiles <- fivePercentQuantiles[seq(2, 20, by = 2)]
    
    # replace Inf with NA
    dataFrame <-
      do.call(data.frame, lapply(dataFrame, function(x)
        replace(x, is.infinite(x), NA)))
    # remove NaN and NA
    dataFrame <- dataFrame[complete.cases(dataFrame),]
    if ((nrow(dataFrame) == 0) || (mean(dataFrame$Y) == 0)) {
      return (-1)
    }
    else{
      vec <- dataFrame[c(treatVarName)]
      medianValue <- median(vec[, 1])
      count <- 0
      for (i in 1:nrow(dataFrame[c(treatVarName)])) {
        temp <- vec[i,]
        if (temp == medianValue) {
          count <- count + 1
        }
      }
      if (count < nrow(dataFrame[c(treatVarName)]) - 3) {
        return(CFmeansForTreatVecLM(dataFrame, outVarName, treatVarName, evenQuantiles))
      } else{
        return(-1)
      }
    }
  }

# 1 LM
CFmeansForDecileBinsLM <-
  function(dataFrame, outVarName, treatVarName) {
    fivePercentQuantiles <-
      quantile(
        dataFrame[[treatVarName]],
        prob = seq(0, 1, length = 21),
        type = 5,
        na.rm = TRUE
      )
    evenQuantiles <- fivePercentQuantiles[seq(2, 20, by = 2)]
    return(CFmeansForTreatVecLM(dataFrame, outVarName, treatVarName, evenQuantiles))
  }

#Where I quantile the treatment values
CFprobsForDecileBinsRF <-
  function(dataFrame, outVarName, treatVarName) {
    fivePercentQuantiles <-
      quantile(dataFrame[[treatVarName]],
               prob = seq(0, 1, length = 21),
               type = 5)
    evenQuantiles <- fivePercentQuantiles[seq(2, 20, by = 2)]
    return(CFprobsForTreatVecRF(dataFrame, outVarName, treatVarName, evenQuantiles))
  }
#Find the largest and smallest numbers and return the difference
maxContrast <- function(CFMeanVec) {
  maxCon <- 0
  index1 <- -1
  index2 <- -1
  
  for (i in 1:(length(CFMeanVec) - 1)) {
    for (j in (i + 1):length(CFMeanVec)) {
      if ((CFMeanVec[i] - CFMeanVec[j]) > maxCon) {
        maxCon <- CFMeanVec[i] - CFMeanVec[j]
        index1 <- i
        index2 <- j
      }
      else if ((CFMeanVec[j] - CFMeanVec[i]) > maxCon) {
        maxCon <- CFMeanVec[j] - CFMeanVec[i]
        index1 <- j
        index2 <- i
      }
    }
  }
  
  return(c(maxCon, index1, index2))
  
}

normalize <- function(x) {
  # From https://stats.stackexchange.com/questions/70801/how-to-normalize-data-to-0-1-range
  x <- as.matrix(x)
  minAttr = apply(x, 2, min)
  maxAttr = apply(x, 2, max)
  x <- sweep(x, 2, minAttr, FUN = "-")
  x = sweep(x, 2,  maxAttr - minAttr, "/")
  attr(x, 'normalized:min') = minAttr
  attr(x, 'normalized:max') = maxAttr
  return (x)
}
#Take the min and maximum of each outcome prediction (averaged) assign a suspiciousness for the variable
computeSuspiciousness <- function(dataframe) {
  headers <- names(dataframe)
  result <- c(1, 1, 1)
  for (i in 1:length(dataframe)) {
    currentVec <- unlist(dataframe[[i]], use.names = FALSE)
    if (length(currentVec) >= 2) {
      vec <- maxContrast(currentVec)
    } else{
      vec <- c(0, 0, 0)
    }
    result <- data.frame(result, vec)
  }
  result[, 1] <- NULL
  names(result) <- headers
  return(result)
}

getTheBiggest <- function(dataframe) {
  # return (names(dataframe)[order(-dataframe[1,])])
  return (dataframe[order(-dataframe[1,])])
}
#cleaning NA's
ditch <- function(x) {
  temp <- as.matrix(x)
  for (i in temp) {
    if (i == "NaN") {
      print(i)
      i <- 0
    }
    if (i == "Inf") {
      print(i)
    }
  }
  y <- as.matrix(temp)
  print(y)
  # ifelse(is.infinite(x), 2147483647, x)
}
# 1 RF
CFmeansForDecileBinsRF <-
  function(dataFrame, outVarName, treatVarName) {
    # replace Inf with NA
    dataFrame <-
      do.call(data.frame, lapply(dataFrame, function(x)
        replace(x, is.infinite(x), NA)))
    # remove NaN and NA
    dataFrame <- dataFrame[complete.cases(dataFrame), ]
    
    if ((nrow(dataFrame) < 1)) {
      return(0)
    }
    #print(treatVarName)
    if (grepl("P[0-9]", treatVarName, perl = TRUE)) {
      #if it is a predicate only make 2 bins for 1 and 0 cases
      
      quantiles <-
        quantile(
          dataFrame[[treatVarName]],
          prob = seq(0, 1, length = 2),
          type = 5,
          na.rm = TRUE
        )
      return(CFmeansForTreatVecRF(dataFrame, outVarName, treatVarName, quantiles))
    } else {
      if (is.finite(dataFrame[[treatVarName]]) &&
          is.numeric(dataFrame[[treatVarName]])) {
        fivePercentQuantiles <-
          quantile(
            dataFrame[[treatVarName]],
            prob = seq(0, 1, length = 21),
            type = 5,
            na.rm = TRUE
          )
        
        # Define the bins
        evenQuantiles <-
          fivePercentQuantiles[seq(2, 20, by = 2)] # 10 bins
        
        
        # evenQuantiles <- fivePercentQuantiles[seq(2, 20, by = 1)] # 19 bins
        return(CFmeansForTreatVecRF(dataFrame, outVarName, treatVarName, evenQuantiles))
      } else{
        if (includeStrings == 1) {
          suppressPackageStartupMessages({
            library(stringdist)
            library(dbscan)
          })
          dftemp <- dataFrame
          distmatrix <-
            stringdistmatrix(as.character(dftemp[[treatVarName]]),
                             as.character(dftemp[[treatVarName]]),
                             method = "dl")
          clustering.dbscan <-
            dbscan::dbscan(distmatrix, eps = 0.30, minPts = 10)
          
          dftemp$clusters <- clustering.dbscan$cluster
          
          dftemp <-
            aggregate(clusters ~ ., data = dftemp, FUN = median)
          
          return(CFmeansForTreatVecRF(dataFrame, outVarName, treatVarName, dftemp[[treatVarName]]))
        }
      }
    }
  }

computeESP <- function(S_p_obs, F_p_obs, NumF, dataFrame) {
  S_p <- nrow(subset(dataFrame, Y == 0))
  
  F_p <- nrow(subset(dataFrame, Y == 1))
  
  sensitivity <- log(F_p) / log(NumF)
  
  increase_p <- F_p / (S_p + F_p) - F_p_obs / (S_p_obs + F_p_obs)
  
  importance_p <-
    2 / ((1 / increase_p) + 1 / (log(F_p) / log(NumF)))
  return(importance_p)
}

CFmeansForESP <- function(dataFrame, outVarName, treatVarName) {
  NumF <- nrow(subset(dataFrame, Y == 1))
  # print("NumF")
  
  # replace Inf with NA
  dataFrame <-
    do.call(data.frame, lapply(dataFrame, function(x)
      replace(x, is.infinite(x), NA)))
  # remove NaN and NA
  dataFrame <- dataFrame[complete.cases(dataFrame), ]
  
  if (nrow(dataFrame) < 1) {
    return(-1)
  } else{
    if (is.finite(dataFrame[[treatVarName]]) &&
        is.numeric(dataFrame[[treatVarName]])) {
      elastic <- data.frame(importance = c(0, 0, 0, 0, 0, 0, 0, 0, 0))
      
      vec <- dataFrame[c(treatVarName)]
      mu <- mean(vec[, 1])
      tau <- sd(vec[, 1])
      
      S_p_obs <- nrow(subset(dataFrame, Y == 0))
      # print("S_p_obs")
      # print(S_p_obs)
      F_p_obs <- nrow(subset(dataFrame, Y == 1))
      # print("F_p_obs")
      # print(F_p_obs)
      
      elastic[1, 1] <-
        computeESP(S_p_obs,
                   F_p_obs,
                   NumF,
                   subset(dataFrame, eval(as.name(treatVarName)) < mu - 3 * tau))
      elastic[2, 1] <-
        computeESP(S_p_obs,
                   F_p_obs,
                   NumF,
                   subset(
                     dataFrame,
                     eval(as.name(treatVarName)) >= mu - 3 * tau &
                       eval(as.name(treatVarName)) < mu - 2 * tau
                   ))
      elastic[3, 1] <-
        computeESP(S_p_obs,
                   F_p_obs,
                   NumF,
                   subset(
                     dataFrame,
                     eval(as.name(treatVarName)) >= mu - 2 * tau &
                       eval(as.name(treatVarName)) < mu - tau
                   ))
      elastic[4, 1] <-
        computeESP(S_p_obs,
                   F_p_obs,
                   NumF,
                   subset(dataFrame, eval(as.name(treatVarName)) >= mu - tau &
                            eval(as.name(treatVarName)) < mu))
      elastic[5, 1] <-
        computeESP(S_p_obs, F_p_obs, NumF, subset(dataFrame, eval(as.name(treatVarName)) == mu))
      elastic[6, 1] <-
        computeESP(S_p_obs,
                   F_p_obs,
                   NumF,
                   subset(dataFrame, eval(as.name(treatVarName)) > mu &
                            eval(as.name(treatVarName)) <= mu + tau))
      elastic[7, 1] <-
        computeESP(S_p_obs,
                   F_p_obs,
                   NumF,
                   subset(
                     dataFrame,
                     eval(as.name(treatVarName)) > mu + tau &
                       eval(as.name(treatVarName)) <= mu + 2 * tau
                   ))
      elastic[8, 1] <-
        computeESP(S_p_obs,
                   F_p_obs,
                   NumF,
                   subset(
                     dataFrame,
                     eval(as.name(treatVarName)) > mu + 2 * tau &
                       eval(as.name(treatVarName)) <= mu + 3 * tau
                   ))
      elastic[9, 1] <-
        computeESP(S_p_obs,
                   F_p_obs,
                   NumF,
                   subset(dataFrame, eval(as.name(treatVarName)) > mu + 3 * tau))
      
      elastic <- data.frame(elastic[complete.cases(elastic),])
      
      if (nrow(elastic) == 0) {
        maxValue = -Inf
        
        
      } else{
        maxValue <- abs(sort(elastic[, 1])[length(elastic[, 1])])
      }
      
      return (maxValue)
    } else{
      if (includeStrings == 1) {
        return (-1)
      }
    }
  }
}
computeBaah <- function(dataFrame, outVarName, treatVarName) {
  if (includeStrings == 1) {
    suppressPackageStartupMessages({
      library(dplyr)
      library(tidyr)
    })
    #library(tidyverse)
    dataFrame %>% mutate_if(is.numeric, replace_na, 0) %>%
      mutate_if(is.character, replace_na, "0")
    #print(dataFrame)
    dataFrame[-1][!is.na(dataFrame[-1])] <- 1
    dataFrame[-1][is.na(dataFrame[-1])] <- 0
    dataFrame[] <-
      lapply(dataFrame, function(x)
        as.numeric(as.character(x)))
    model <-
      lm(paste(outVarName, " ~ .", sep = ""), data = dataFrame)
    
    return(model$coefficients[2])
  } else{
    if (is.numeric(dataFrame[[treatVarName]])) {
      suppressPackageStartupMessages({
        library(dplyr)
        library(tidyr)
      })
      #library(tidyverse)
      dataFrame %>% mutate_if(is.numeric, replace_na, 0) %>%
        mutate_if(is.character, replace_na, "0")
      #print(dataFrame)
      dataFrame[-1][!is.na(dataFrame[-1])] <- 1
      dataFrame[-1][is.na(dataFrame[-1])] <- 0
      dataFrame[] <-
        lapply(dataFrame, function(x)
          as.numeric(as.character(x)))
      model <-
        lm(paste(outVarName, " ~ .", sep = ""), data = dataFrame)
      
      return(model$coefficients[2])
    }
  }
}

is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

is.infinite.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.infinite))
}

#newoutput2 <- data.frame(t(newoutput))
#colnames(newoutput2) <- newoutput2[1,]
# =============================
# Start HERE
# input: newoutput, outY
args <- commandArgs(trailingOnly = TRUE)
includeStrings <- as.numeric(args[1])

newoutput <-
  read.table(
    "/unival/newoutput.txt",
    sep = "\t",
    quote = "",
    comment.char = "",
    stringsAsFactors = FALSE,
    fill = TRUE
  )


outY <-
  read.table(
    "/unival/outY.txt",
    quote = "\"",
    comment.char = ""
  )
# =============================
newoutput <- data.frame(newoutput)
rownames(newoutput) <- newoutput[, 1]
newoutput <- newoutput[,-1]
newoutput <- as.data.frame(t(newoutput), stringsAsFactors = FALSE)
newoutput[] <- lapply(newoutput, type.convert, as.is = TRUE)
# Y <- data.frame(t(outY))
# names(Y) <- c("Y")
outY <- data.frame(outY[, 2])
names(outY) <- c("Y")
numfldata <- newoutput
#newoutput <-do.call(data.frame, lapply(newoutput, function(x)replace(x,!is.finite(x), NA)))
fault_binerrs_all <- data.frame(outY, newoutput)

# trainCFoutPredRF(TestShimple_fault_binerrs_all, "Y")
CFmeanResult <- genCFmeansRF_fault_binerrs()
# for RF
maxContrastDF <- computeSuspiciousness(CFmeanResult)
result <- getTheBiggest(maxContrastDF)
# for ESP
resultESP <- getTheBiggest(genCFmeansESP_fault_binerrs())
#maxContrastESP<- compute(resultESP)
#resultESP<-getTheBiggest(maxContrastESP)

#=========For boxplot===============
# cbind the fault rate
faultRate <- sum(outY) / nrow(outY)
tempResultCF <- result
tempResultESP <- resultESP

write.csv(
  tempResultCF,
  file = "/unival/resultUniVal.csv",
)
write.csv(
  tempResultESP,
  file = "/unival/resultESP.csv"
)
# only the first time
#meanResult <- tempResult

# other runs (2nd, 3rd, 4th, 5th time)
#meanResult <- rbind(meanResult, tempResult)

# after 5 runs
# meanResult <- rbind(meanResult, colMeans(meanResult))
# meanResult <- meanResult[order(meanResult[6,], decreasing = T)]

# output to a excel
#write.csv(Result, file = "/result.csv")

#=========================

# rbind with each result, run FOR 10 TIMES
#meanResult <- rbind    (meanResult, result[1,])
# remove wrong lines
# meanResult<- meanResult[-c(2),]
# make a copy of meanResult
# meanResultCopy <- meanResult

# sort by the mean Y
#meanResult <- rbind(meanResult, colMeans(meanResult))
#meanResult <- meanResult[order(meanResult[6,], decreasing = T)]
# resultForPlot <- resultForPlot[-nrow(resultForPlot),]
# only cares about the top 20 variable in the rank
# resultForPlot <- resultForPlot[,1:20]
# boxplot(resultForPlot, las = 2)
