#If you don't have the required packages the below 3 lines will install it.
list.of.packages <- c("RMySQL", "DBI","ggplot2", "readr", "stringr", "wordcloud", "RColorBrewer", "randomForest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(DBI)
library(ggplot2)
library(readr)
library(RMySQL) 
library(stringr) #for parsing and cleaning the title strings.
library(wordcloud) #wordcloud generator
library(RColorBrewer) #colors for wordCloud
library(randomForest)
#We'll be using the below function a lot for creating indicator variables out of attributes.
indvar <- function(x,y,z) {
  return(str_count(data[x,y],z))
}

workingdirectory <- readline(prompt="Enter location of Git Repository for directory: ")
setwd(toString(workingdirectory))

passwd <- readline(prompt="Enter password (6 digits): ")
conn <- dbConnect(MySQL(), user='root', password=passwd, dbname='craigsdata', host='127.0.0.1')
dbGetQuery(conn, 'select * from "scrapeddata110519" a join "scrapeddata" b on a.link = b.link')
print('connection successful')

#Note: if you're replicating this and don't have your own SQL server setup you can use the CSVs:
if(FALSE){
  data1 <- read_csv("eb_apts_sample_clean101819.csv")
  data2 <- read_csv("eb_apts_sample_clean110519.csv")
  data <- rbind(data1, data2)
  rm(data1, data2)
}

#Generate columns for Furnished, Pet Indicators, laundry, parking and female preferred/only
data$furnished=NA
data$pet=NA
data$laundry=NA
data$parking=NA
data$female=NA
data$titlelen=NA
#Now we'll loop through every line and check which attributes it has
for (i in 1:nrow(data)) {
  # in col. 7 we'll populate an indicator var. for if it is furnished.
  data[i,7] <- indvar(i,6, 'furnished')
  # in col. 8 we'll populate an indicator var. for if they allow pets
  if(indvar(i,6, 'cats are OK')>0) {
    data[i,8] <- indvar(i,6, 'cats are OK')
  } else {
    data[i,8] <- indvar(i,6, 'dogs are OK')
  }
  # Now we'll check if it has laundry in either the bldg or on site
  if(indvar(i,6, 'laundry on site')>0) {
    data[i,9] <- indvar(i,6, 'laundry on site')
  } else if (indvar(i,6, 'laundry in bldg')>0) {
    data[i,9] <- indvar(i,6, 'laundry in bldg')
  } else {
    data[i,9] <- indvar(i,6, 'w/d in unit')
  }
  # in col. 10 we'll populate an indicator var. for if they have something besides street parking
  if(indvar(i,6, 'off-street parking')>0) {
    data[i,10] <- indvar(i,6, 'off-street parking')
  } else if (indvar(i,6, 'garage')>0) {
    data[i,10] <- indvar(i,6, 'garage')
  } else {
    data[i,10] <- indvar(i,6, 'carport')
  }
  # In col. 11 we'll populate an indicator var. for if they have a female preference/only female.
  data[i,11] <- indvar(i,3, 'female')
  # In col. 12 we'll populate with the length of the title of the posts.
  data[i,12]=nchar(str_trim(data[i,3]))
}

#Amenities will be the sum of positive attributes identified in the prior loop.
data$amenities=data$furnished+data$pet+data$laundry+data$parking
# Now let's just get a rough feel for if (in aggregate) we've probably identified useful amenities.  Later we'll use a regression to test their correlation individually.
scatter.smooth(x=data$amenities, y=data$prices, main="amenties vs prices")  
#The scatterplot shows a line of best fit that's roughly positive so it does appear as if we probably selected "good" amenities.

hist(data$prices, col = 'skyblue2', breaks = 20, main="Prices on Craigslist SF Rooms for Rent", xlab="Price ($)")
bargrouped <- ggplot(data, aes(x=prices)) +
                     geom_histogram(binwidth=100) +
                     stat_bin(binwidth=100, aes(label=..count..)) + 
                     theme_minimal()
#ggplot(data) +
#  aes(x = prices) +
#  geom_bar() + 
#  geom_text(stat = 'count',aes(label =..count.., vjust = -1)) + 
#  stat_bin(breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2500, 3000, 4000))

#ggplot(data) +
#  aes(x = prices) +
#  geom_bar() + 
#  geom_text(stat = 'count',aes(label =..count.., vjust = -1)) 

#If you look at the overall shape of prices we're can definitely see there's a left-skew with a bulge right below $1000 and then virtually nothing above $2,500.
#$1,000 may be a price point that renters and landlords psychologically place a lot of significance on.  To test for this we'll generate an indicator for that as well. (https://www.psychologytoday.com/us/blog/the-science-behind-behavior/201901/how-price-points-can-trick-us-making-purchase)
data$OneK=0
for (i in 1:nrow(data)) {
  if (data[i,5]>=1000) {
    data[i,14] <- 1
  }
}

#Now ideally we'd do something like A/B Testing to show optimal price points for a large range in quality may be below $1,000. Since we can't do true A/B testing we'll use our 
#attributes to forecast the price and then get the error and run a regression of the error against the OneK variable.

#Section 1: Linear model
#plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
linearmod <- lm(prices ~ furnished + pet + laundry + parking + female + titlelen, data=data)
summary(linearmod)
lmModerror <- residuals(linearmod)
data <- cbind(data, lmModerror)
pricepointtest <- lm(lmModerror ~ OneK, data=data)
summary(pricepointtest)
#It does appear as if $1,000 is a significant price point

#One observation we can make from our rough linear model (need to go back and deal with outliers) is that whether 
#it is furnished, whether it allows pets, and whether it is female preferred/only didn't have a significant impact
#on price.What seemed to matter was Laundry, and parking.
#Title length is tricky as it might be capturing other custom attributes possessed by the property or longer
#it may just reflect the landlord's need to justify the higher price or attempt to stir up more enthusiasm or
#attention (especially if they have to list it multiple times and it's been posted for days).

#Here's a bit of clean-up
data$OneK <- NULL
data$lmModerror <- NULL

#Section 2: Exploration of if we can build an algorithm to predict misleading listings.
#Note: we have one indicator of a misleading listing.  Prices $1 or less it's safe to say are not actually going to be for rent for $0-$1.
#at the very least there's probably some quid pro quo if the true price isn't higher.

#Let's build vocabulary of words used in the titles
parse_spaces <- function(string){
  words <- c()
  lastspace=0
  string <- gsub("[^[:alnum:]]", " ", string) # swaps out non-alphanumeric characters like parenthesis, backslash and foreign language characters ("[[:punct:]]" could have done all but the last) ()
  vectorofspaces <- unlist(gregexpr(" ", string))
  for (i in vectorofspaces) {
    if (i-lastspace == 1){
      #This condition avoids having empty entries in words if there are two spaces next to each other
      lastspace <- i
      next
    } 
    words <- c(words, substring(string, lastspace+1,i-1))
    lastspace <- i 
  }
  #Now the last word will be missing unless we do the below.  ALso, the if statements avoids having blank entries if it ends in a space
  if (nchar(string)-lastspace != 0){
    words <- c(words, substring(string, lastspace+1, nchar(string)))
  }
  return(words)
}
#parse_spaces returns a vector of words used in the string (i.e. the title)

#Let's remove the numbers since they're probably prices (except if it's below 6 because are probably room counts)
remove_numbers <- function(vector){
  numindex = c()
  for (i in 1:length(vector)){
    if (suppressWarnings(is.na(as.numeric(trimws(vector[i]))))){
      numindex = c(numindex,FALSE)
    } else if (as.numeric(trimws(vector[i]))<6) {
      numindex = c(numindex,FALSE)
    } else {
      numindex = c(numindex,TRUE)
    }
  }
  return(vector[! numindex])
}

vocabulary <- c()
frequency <- c()
data$parsed_words <- NA
for(i in 1:nrow(data)) {
  words_in_title <- remove_numbers(parse_spaces(data[i,3]))
  data$parsed_words[i] <- list(words_in_title)
  if (is.null(words_in_title)){next}
  for (k in 1:length(words_in_title)) {
    checkforentry <- grep(paste("^", tolower(words_in_title[k]), "$",sep=""), vocabulary) #we'll do a case insensitive search for other entries
    #Now if there is no preexisting entry we'll add it to the vocabulary
    if (length(checkforentry)==0){
      vocabulary <- c(vocabulary, tolower(words_in_title[k]))
      frequency <- c(frequency, 1)
    } else {
      frequency[checkforentry] = frequency[checkforentry] + 1
    }
  }
}
#vocabulary is now a list of all the unique words used in the titles. With frequency being how often they appear.
# Below we'll sort it by frequency.
for(i in 1:(length(vocabulary)-1)){
  for(k in 1:(length(vocabulary)-1)){
    if (frequency[k]<frequency[k+1]){
      temp <- frequency[k]
      frequency[k] <- frequency[k+1]
      frequency[k+1] <- temp
      temp <- vocabulary[k]
      vocabulary[k] <- vocabulary[k+1]
      vocabulary[k+1] <- temp
    }
  }
}

vocabdata <- data.frame(word = vocabulary, freq =frequency)
head(vocabdata, 12)

#Data Visualization
png(file="C:/Users/Joshua Fujino/Documents/Git repository/Wordcloud_of_keywords.png", width= 865, height=570)
wordcloud(words=vocabdata$word, freq = vocabdata$freq, 
          min.freq=1, 
          max.words=250, 
          random.order=FALSE, 
          rot.per = 0.35, 
          colors=brewer.pal(6, "Dark2"))
dev.off()

sourcecol <- dim(data)[2] # This is the number of columns before adding our dummy vars for each word used multiple times.
wordsusedfreq <- vocabulary[1:(grep("^2$", frequency)[1]-1)] #This creates list without any words used only once or twice.
dummyvarnames <- sapply(1:length(wordsusedfreq), function(x) paste("word_",x,sep=""))
data[dummyvarnames] <- 0 #Now we'll add them as columns (with 0 values) to our dataframe.

#let's populate the binary dummy variables with a 1 if the word is used in the title or 0 otherwise.
for (i in 1:nrow(data)){
  for (word in 1:length(data$parsed_words[[i]])){
    match <- grep(paste("^", tolower(data$parsed_words[[i]][word]), "$",sep=""), wordsusedfreq)
    if(length(match)>0){
      data[i,sourcecol+match] = data[i,sourcecol+match] + 1
    }
  }
}

# Fraud indicator
data$fraud <- ifelse(c(data$prices)<=1, 1 ,0)

###Random Forest ensemble model:
set.seed(7)
# We'll shuffle our data and then use 75% for training our model and 25% for testing it.
#shuffle data
newindex <- sample(nrow(data), nrow(data), replace = FALSE)
data <- data[newindex,]
#designate training dataset and test dataset
seventypercent <- round((7/10)*nrow(data), 0)
trainingdata <- data[1:seventypercent,]
testdata <- data[seventypercent+1:nrow(data),]

#Now let's cut out all of the unnecessary variables (e.g.title, link, count of amenities,etc.)
trainingdata <- cbind(trainingdata[,7:12], trainingdata[,15:ncol(data)])
testdata <- cbind(testdata[,7:12], testdata[,15:ncol(data)])
rm(list.of.packages, lmModerror, words_in_title, wordsusedfreq, vocabdata, newindex) #Just to clean up our workspace

#Setting up RandomForest Model
model1 <- randomForest(fraud ~ furnished + pet + laundry + parking + female + titlelen, data = trainingdata, ntree=500, mtry=5, importance = TRUE)
model1
model2 <- randomForest(fraud ~ ., data = trainingdata, ntree=500, mtry=5, importance = TRUE)
model2
# Predictions
predfraud <- predict(model2, testdata, type = "class")
# Checking classification accuracy
table(predfraud, testdata$fraud)  
mean(predfraud == testdata$fraud)    



# Logistic Regression comparison
logitmodel <- glm(formula = fraud ~ ., family = "binomial", 
                  data = logitdata)
testdata$PredFraudLogit <- predict(logitmodel, newdata=testdata, type="response")
mean(testdata$PredFraudLogit)


#P.S. I haven't gotten to it yet, but later I hope to clean up and incorporate neighborhood
#Neighborhood per below certainly does seem significant but it also appears to have a lot of outliers and we need to clean up the categories since many posters listed multiple neighborhoods (some at the county level, some at the city level, and some at a local sub-city level)
ggplot(data, aes(x=neighborhood, y=prices)) + geom_boxplot() + ggtitle("Neighborhood Prices Boxplot")

dbDisconnect(conn)