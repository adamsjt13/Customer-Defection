rm(list=ls())

start_ind <- "01/02/2006"
end_ind <- "08/10/2009"
start_dep <- "08/11/2009"
end_dep <- "02/02/2011"

defectionR <- function(start_ind,end_ind,start_dep,end_dep,evaluate=TRUE){
  
  # set format for dates
  f <- "%m/%d/%Y"
  t1 <- as.Date(start_ind, f)
  t2 <- as.Date(end_ind, f)
  t3 <- as.Date(start_dep, f)
  t4 <- as.Date(end_dep, f) #dump date 
  length_ind <- t2 - t1
  
  # load packages
  for (i in c("AUC","lift","glmnet")) {
    if (!require(i,character.only=TRUE,quietly=TRUE)) {
      install.packages(i, 
                       repos="http://cran.rstudio.com",
                       quietly = TRUE)
      require(i,
              character.only = TRUE,
              quietly = TRUE)
    }
  }
  
  # function for reading and preparing basetable
  readAndPrepareData <- function(train=TRUE,...){
    cat("Reading in data:")
    time <- Sys.time()
    
    # create class for reading in dates as date objects
    fd <- "%d/%m/%Y" 
    setClass('fDate') 
    setAs(from="character",
          to="fDate",
          def=function(from) as.Date(from, format=fd))
    
    # read in data tables
    complaints <- read.table("complaints.txt", 
                             sep = ";", 
                             header = TRUE,
                             colClasses = c("character",
                                            "character",
                                            "factor",
                                            "fDate",
                                            "factor",
                                            "factor",
                                            "factor"))
    
    credit <- read.table("credit.txt",
                         sep = ";",
                         header = TRUE,
                         colClasses = c("character",
                                        "character",
                                        "factor",
                                        "fDate",
                                        "factor",
                                        "numeric",
                                        "integer"))
    
    customers <- read.table("customers.txt",
                            sep = ";",
                            header = TRUE,
                            colClasses = c("character",
                                           "factor",
                                           "fDate",
                                           "factor",
                                           "character",
                                           "character"))
    
    delivery <- read.table("delivery.txt",
                           sep = ";",
                           header = TRUE,
                           colClasses = c("character",
                                          "character",
                                          "factor",
                                          "factor",
                                          "factor",
                                          "fDate",
                                          "fDate"))
    
    formula <- read.table("formula.txt",
                          sep = ";",
                          header = TRUE,
                          colClasses = c("character",
                                         "factor",
                                         "factor",
                                         "numeric"))
    
    subscriptions <- read.table("subscriptions.txt",
                                sep = ";",
                                header = TRUE,
                                colClasses = c("character",
                                               "character",
                                               "character",
                                               "factor",
                                               "fDate",
                                               "fDate",
                                               "integer",
                                               "integer",
                                               "fDate",
                                               "factor",
                                               "factor",
                                               "fDate",
                                               "character",
                                               rep("numeric",8)))
    
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    
    cat("Preparing basetable:")
    time <- Sys.time()
    #######################################
    # Data Prep
    
    # used when calling readAndPrepareData in predict function
    if (train==FALSE){
      dots <- list(...) # list(...) evaluates all arguments and
      # returns them in a named list
      t2 <- dots$end_ind
      t1 <- t2 - dots$length_ind
      rm(dots)
    }
    # get active subs
    recent_subs <- subscriptions[subscriptions$StartDate <= t2,]
    cust_sub <- merge(customers, recent_subs, all.y = TRUE)
    # calculate customer age in years
    cust_sub$Age <- as.numeric(t2 - cust_sub$DOB) / 365.25
    # calculate length of current sub
    cust_sub$SubLength <- as.numeric(cust_sub$EndDate - cust_sub$StartDate)
    # calculate avg total price
    avg_total_price <- aggregate(cust_sub$TotalPrice, 
                                 by = list("CustomerID" = cust_sub$CustomerID), 
                                 mean,
                                 na.rm = TRUE)
    names(avg_total_price)[2] <- "AvgTotalPrice"
    # calculate avg total discount
    avg_total_disc <- aggregate(cust_sub$TotalDiscount, 
                                by = list("CustomerID" = cust_sub$CustomerID), 
                                mean,
                                na.rm = TRUE)
    names(avg_total_disc)[2] <- "AvgTotalDiscount"
    # calculate avg sub length
    avg_sub_length <- aggregate(cust_sub$SubLength, 
                                by = list("CustomerID" = cust_sub$CustomerID), 
                                mean)
    names(avg_sub_length)[2] <- "AvgSubLength"
    # calculate num diff products per customer
    num_products_per_cust <- aggregate(cust_sub$ProductID, 
                                       by = list("CustomerID" = cust_sub$CustomerID), 
                                       unique)
    # calculate num of subs by a customer
    num_subs_per_cust <- aggregate(cust_sub$CustomerID, 
                                   by = list("CustomerID" = cust_sub$CustomerID), 
                                   length)
    names(num_subs_per_cust)[2] <- "NumSubs"
    # calculate avg num of papers by customer
    avg_num_papers <- aggregate(cust_sub$NbrNewspapers, 
                                by = list("CustomerID" = cust_sub$CustomerID), 
                                mean)
    names(avg_num_papers)[2] <- "AvgNumPapers"
    # get age for each customer from earlier calculation
    cust_age <- aggregate(cust_sub$Age, 
                          by = list("CustomerID" = cust_sub$CustomerID), 
                          mean)
    names(cust_age)[2] <- "Age"
    # customer's gender
    cust_gender <- data.frame("CustomerID" = customers$CustomerID,
                              "Gender" = customers$Gender)
    # calculate num complaints by customer
    num_complaints <- aggregate(complaints$CustomerID,
                                by = list("CustomerID" = complaints$CustomerID), 
                                length)
    names(num_complaints)[2] <- "NumComplaints"
    
    # get end date for current sub
    most_recent_end <- aggregate(cust_sub$EndDate,
                                 by = list("CustomerID" = cust_sub$CustomerID), 
                                 max)
    names(most_recent_end)[2] <- "EndDate"
    # get renewal date for current sub
    most_recent_renewal <- suppressWarnings(aggregate(cust_sub$RenewalDate,
                                                      by = list("CustomerID" = cust_sub$CustomerID), 
                                                      max,
                                                      na.rm = TRUE))
    names(most_recent_renewal)[2] <- "RenewDate"
    
    dates <- list(most_recent_end,
                  most_recent_renewal)
    # merge end date of active sub into cust_sub table
    most_recent_subs <- merge(most_recent_end, cust_sub)
    # merge end and renewal dates for each customer
    datetable <- Reduce(function(x,y) merge(x,y,by="CustomerID"), dates)
    
    if(train==TRUE){
      # calculate days between end date and renewal date
      datetable$renewStatus <- as.numeric(datetable$EndDate - datetable$RenewDate)
      # if negative or inf (result of missing renewal date) the customer is a churner
      datetable$Churn <- ifelse(is.infinite(datetable$renewStatus) | datetable$renewStatus < 0,
                                1,0)
      # only end dates that are in dependent period
      datetable <- datetable[datetable$EndDate > t3,]
      # get churn status for each cust ID
      churn_status <- datetable[,c("CustomerID", "Churn")]
    }
    
    # compile all calculations from above
    data <- list(avg_total_price,
                 avg_total_disc,
                 avg_sub_length,
                 num_subs_per_cust,
                 avg_num_papers,
                 cust_age,
                 cust_gender,
                 num_complaints)
    # merge them by cust ID
    basetable <- Reduce(function(x,y) merge(x,y,by="CustomerID", all.x = TRUE), data)
    
    if(train==TRUE){
      # add churn to basetable
      basetable <- merge(basetable, churn_status, by = "CustomerID")
    }
    
    # change num complaints from NA to 0
    basetable$NumComplaints[is.na(basetable$NumComplaints)] <- 0
    basetable$NumComplaints <- as.numeric(basetable$NumComplaints)

    # pull in other possible predictors    
    other_sub_info <- most_recent_subs[c(1,8,9,10,15,16,18,20)]
    
    basetable <- merge(basetable, other_sub_info, by = "CustomerID")
    basetable <- merge(basetable, formula, by = "FormulaID")
    
    recent_del <- delivery[delivery$StartDate <= t2 & delivery$EndDate >= t2,]
    
    basetable <- merge(basetable, recent_del, by = "SubscriptionID")
    
    num_credits_per_cust <- aggregate(CreditID ~ SubscriptionID, credit, length)
    names(num_credits_per_cust)[2] <- "NbrCredits"
    
    basetable <- merge(basetable, num_credits_per_cust, by = "SubscriptionID", all.x = TRUE)
    basetable$NbrCredits[is.na(basetable$NbrCredits)] <- 0
    
    # change relevant columns to factors as necessary
    basetable[c(2,14,24)] <- lapply(basetable[c(2,14,24)], factor)
    # removing unneccessary columns
    basetable$SubscriptionID <- basetable$StartDate <- basetable$EndDate <- NULL
    basetable$DeliveryID <- basetable$FormulaCode <- basetable$FormulaID <- NULL
    
    if(train==TRUE){
      # store customer ids for possible future use
      customer_ids <- basetable$CustomerID
      # store response variable
      y <- as.factor(basetable$Churn)
      # remove cust ID and churn from basetable 
      basetable$CustomerID <- basetable$Churn <- NULL
    }
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    
    if(train==TRUE){
      return(list("predictors"=basetable, "Churn"=y))
    } else {
      return(basetable)
    }
  } # end readAndPrepareData
  
  # call function to create basetable
  basetable <- readAndPrepareData()
  
  if(evaluate==TRUE){
    cat("Evaluating model:")
    time <- Sys.time()
    
    # split data into test, train, val for model evaluation
    allind <- sample(x=1:nrow(basetable$predictors),
                     size=nrow(basetable$predictors))
    trainind <- allind[1:round(length(allind)/3)]
    valind <- allind[(round(length(allind)/3)+1):round(length(allind)*(2/3))] 
    testind <- allind[round(length(allind)*(2/3)+1):length(allind)]
    
    basetabletrain <- basetable$predictors[trainind,]
    basetableval <- basetable$predictors[valind,]
    basetabletest <- basetable$predictors[testind,]
    basetabletrainbig <- rbind(basetabletrain,basetableval)
    
    ytrain <- basetable$Churn[trainind]
    yval <- basetable$Churn[valind]
    ytest <- basetable$Churn[testind]
    ytrainbig <- factor(c(as.character(ytrain),as.character(yval)))
    
    # fit glmnet model 
    glmnet_model <- glmnet(x= data.matrix(basetabletrain),
                           y = ytrain,  
                           family = "binomial")
    
    # initialize empty vector for AUCs
    aucstore <- numeric(length(glmnet_model$lambda))
    
    # test lambda values and evaluate AUC
    for(j in 1:length(glmnet_model$lambda)){
      glmnet_pred <- predict(glmnet_model, 
                             newx = data.matrix(basetableval), 
                             type = "response",
                             s = glmnet_model$lambda[j])
      aucstore[j] <- AUC::auc(roc(as.numeric(glmnet_pred),yval))
    }
    
    # store lambda with highest AUC
    glmnet.lambda <- glmnet_model$lambda[which.max(aucstore)]
    
    # build model and predict with optimal lambda
    glmnet_model <- glmnet(x= data.matrix(basetabletrainbig),
                           y = ytrainbig,  
                           family = "binomial")
    glmnet_pred <- predict(glmnet_model, 
                           newx = data.matrix(basetabletest), 
                           type = "response",
                           s = glmnet.lambda)
    
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4), 
        attr(Sys.time()- time,"units"), "\n")
    
    cat(" AUROC:", 
        round(auc(roc(glmnet_pred,ytest)),4),"\n")
    cat(" Top decile lift of:", 
        round(TopDecileLift(glmnet_pred,ytest),4),"\n")
    
    
  } # end evaluation loop
  
  cat("Creating model:")
  time <- Sys.time()
  
  # split data into train and test to obtain optimal lambda
  allind <- sample(x=1:nrow(basetable$predictors),
                   size=nrow(basetable$predictors))
  trainind <- allind[1:round(length(allind)/2)]
  testind <- allind[round(length(allind)/2):length(allind)]
  
  ytrain <- basetable$Churn[trainind]
  ytest <- basetable$Churn[testind]
  
  basetabletrain <- basetable$predictors[trainind,]
  basetabletest <- basetable$predictors[testind,]
  
  # fit glmnet model 
  glmnet_model <- glmnet(x= data.matrix(basetabletrain),
                         y = ytrain, 
                         family = "binomial")
  
  # initialize empty vector for AUCs
  aucstore <- numeric(length(glmnet_model$lambda))
  
  # test lambda values and evaluate AUC
  for(j in 1:length(glmnet_model$lambda)){
    glmnet_pred <- predict(glmnet_model, 
                           newx = data.matrix(basetabletest), 
                           type = "response",
                           s = glmnet_model$lambda[j])
    aucstore[j] <- AUC::auc(roc(as.numeric(glmnet_pred),ytest))
  }
  
  # store lambda with highest AUC
  glmnet.lambda <- glmnet_model$lambda[which.max(aucstore)]
  
  print(length(names(coef(glmnet_model)[,which.max(aucstore)])))
  # build model and predict with optimal lambda
  glmnet_pred <- predict(glmnet_model, 
                         newx = data.matrix(basetabletest), 
                         type = "response",
                         s = glmnet.lambda)
  
  
  cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4), 
      attr(Sys.time()- time,"units"), "\n")
  
  # return list of model, function, date format, length of ind period, optimal lambda
  l <- list("glmnet" = glmnet_model,
            "readAndPrepareData" = readAndPrepareData,
            "f" = f,
            "length_ind" = length_ind,
            "lambda" = glmnet.lambda) 
  
  # change class of return to defectionR for use with predict function
  class(l) <- "defectionR"
  return(l)
}


predict.defectionR <- function(object, dumpDate){
  
  # install packages
  for (i in c("AUC","lift","glmnet")) {
    if (!require(i,character.only=TRUE,quietly=TRUE)) {
      install.packages(i, 
                       repos="http://cran.rstudio.com",
                       quietly = TRUE)
      require(i,
              character.only = TRUE,
              quietly = TRUE)
    }
  }
  
  #Make sure all variables in the readAndPrepareData 
  #enclosing environment (where it was defined) are removed 
  #to avoid unexpected results 
  environment(object$readAndPrepareData) <- environment()
  
  basetable <- object$readAndPrepareData(train = FALSE,
                                         end_ind = as.Date(dumpDate, object$f),
                                         length_ind = object$length_ind)
  
  cat("Predicting: ")
  time <- Sys.time()
  
  # create data table with results
  ans <- data.frame("CustomerID" = basetable$CustomerID,
                    "Score" = predict(object = object$glmnet,
                                      newx = data.matrix(basetable[,-1]),
                                      type = "response",
                                      s = object$lambda)[,1])
  # change probability to percentage
  ans$Score <- ans$Score*100
  # order by most likely to churn
  ans <- ans[order(ans$Score, decreasing=TRUE),]
  
  cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4), 
      attr(Sys.time()- time,"units"), "\n")
  #return percentages for each customer
  ans
}