#If you don't have the required packages the below 3 lines will install it.
list.of.packages <- c("RMySQL", "DBI","ggplot2", "readr", "stringr")
new.packages <- list.of.packages[!(list.ofpackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(DBI)
library(ggplot2)
library(readr)
library(RMySQL)
library(stringr)

#We'll be using the below function a lot for creating indicator variables out of attributes.
indvar <- function(x,y,z) {
  return(str_count(data[x,y],z))
}

#conn <- dbConnect(MySQL(), user='josh', password='r3e3d3', dbname='craigsdata', host='192.168.0.108')
conn <- dbConnect(MySQL(), user='josh', dbname='craigsdata', host='127.0.0.1')
dbGetQuery(conn, 'select * from "scrapeddata"')
print('connection successful')
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

View(data)

hist(data$prices)
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
#It doesn't appear as if $1,000 is a significant price point

#One observation we can make from our rough linear model (need to go back and deal with outliers) is that whether 
#it is furnished, whether it allows pets, and whether it is female preferred/only didn't have a significant impact
#on price.What seemed to matter was Laundry, and parking.
#Title length is tricky as it might be capturing other custom attributes possessed by the property or longer
#it may just reflect the landlord's need to justify the higher price or attempt to stir up more enthusiasm or
#attention (especially if they have to list it multiple times and it's been posted for days).


#Section 2: Exploration of if we can build an algorithm to predict misleading listings.
#Note: we have one indicator of a misleading listing.  Prices $1 or less it's safe to say are not actually going to be for rent for $0-$1.
#at the very least there's probably some quid pro quo if the true price isn't higher.

#Let's build vocabulary of words used in the titles
parse_spaces <- function(string){
  words <- c()
  lastspace=0
  string <- gsub("[[:punct:]]", " ", string) # takes care of special characters like parenthesis and backslash
  vectorofspaces <- unlist(gregexpr(" ", string))
  for (i in vectorofspaces) {
    if (i-lastspace == 1){next} #avoids having empty entries in words if there are two spaces next to each other
    words <- c(words, substring(string, lastspace+1,i-1))
    lastspace <- i 
  }
  #Now the last word will be missing unless we do the below.  ALso, the if statements avoids having blank entries if it ends in a space
  if (nchar(string)-lastspace != 1){
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
for(i in 1:nrow(data)) {
  words_in_title <- remove_numbers(parse_spaces(data[i,3]))
  for (k in 1:length(words_in_title)) {
    checkforentry <- grep(representation(words_in_title[k]), vocabulary)
    #Now if there is no preexisting entry we'll add it to the vocabulary
    if (length(checkforentry)==0){
      vocabulary <- c(vocabulary, words_in_title[k])
    }
  }
}
#vocabulary is now a list of all the unique words used in the titles.

#INCOMPLETE LEFT OFF HERE



#P.S. I haven't gotten to it yet, but later I hope to clean up and incorporate neighborhood
#Neighborhood per below certainly does seem significant but it also appears to have a lot of outliers and we need to clean up the categories since many posters listed multiple neighborhoods (some at the county level, some at the city level, and some at a local sub-city level)
ggplot(data, aes(x=neighborhood, y=prices)) + geom_boxplot() + ggtitle("Neighborhood Prices Boxplot")

dbDisconnect(conn)