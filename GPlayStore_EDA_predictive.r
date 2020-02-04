library(tidyr)
library(dplyr)
library(readr)
library(ggplot2) 
library(ggthemes)
library(varhandle)
library(lubridate)
library(jtools)

#read dataset
playstore = read.csv('googleplaystore.csv', stringsAsFactors = FALSE)
#remove junk value and duplicates
playstore <- distinct(playstore)
playstore <- playstore %>%
  filter(Installs != "Free")
playstore <- playstore %>%
  filter(Type != "NaN")
playstore <- playstore %>%
  filter(Rating <= 5)



#Changing the datatype of Installs and making Installs a factor (saved as installsRefined)
playstore$installsRefined <- playstore$Installs
options(scipen = 999)
playstore$installsRefined <- gsub(",", "", gsub("\\.", "", playstore$installsRefined))
playstore$installsRefined <- as.character(playstore$installsRefined)
playstore$installsRefined = substr(playstore$installsRefined,1,nchar(playstore$installsRefined)-1)
playstore$installsRefined <- as.numeric(playstore$installsRefined)
playstore$installsRefined <- as.factor(playstore$installsRefined)

#Categorizing price (free, very cheap, normal, expensive, very expensive)
playstore$PriceUnfactor <- as.character(paste(playstore$Price))
playstore <- playstore %>%
  mutate(PriceUnfactor = case_when(PriceUnfactor == "0" ~ 0,
                                   PriceUnfactor != "0" ~ as.numeric(substring(PriceUnfactor,2))))
unique(playstore$PriceUnfactor)
playstore <- playstore %>%
  mutate(priceCategory = case_when(PriceUnfactor == 0 ~ "Free",
                                   PriceUnfactor > 0 & PriceUnfactor <= 2 ~ "Very Cheap",
                                   PriceUnfactor >2 & PriceUnfactor <= 5 ~ "Normal",
                                   PriceUnfactor >5 & PriceUnfactor <= 10 ~ "Expensive",
                                   PriceUnfactor >10 ~ "Very Expensive"))
unique(playstore$priceCategory)

#Adding month column of Last Updated Month based on Last.Updated
playstore$LastUpdatedMonth <- format(as.Date(playstore$Last.Updated, format = "%B %d, %Y"),"%b")
playstore$LastUpdatedMonth <- factor(playstore$LastUpdatedMonth,levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
levels(playstore$LastUpdatedMonth)
unique(playstore$LastUpdatedMonth)

#Changing necessary columns into factors for plots and regression analyses
playstore$Category <- as.factor(playstore$Category)
playstore$Rating <- as.numeric(playstore$Rating)
playstore$Reviews <- as.numeric(playstore$Reviews)
playstore$Type <- as.factor(playstore$Type)
playstore$Content.Rating <- as.factor(playstore$Content.Rating)
playstore$Genres <- as.factor(playstore$Genres)
playstore$priceCategory <- as.factor(playstore$priceCategory)


##Visualize data
###Top Categories in Google Play Store
orderedcategory <- playstore %>%
  group_by(Category) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))
ggplot(orderedcategory, aes( x=reorder(Category, -Count), y = Count, fill = Category )) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + theme(legend.position = "none")+labs(x = "", y = "Count")
summary(playstore$Category) #1074 data points for game apps
playstore = playstore %>% filter (Category == "GAME") #filter to only apps in the GAME category


###Plots of Installs and Rating vs. Reviews
ggplot(playstore, aes(x=log(Reviews), y=log(as.numeric(paste(installsRefined))))) + geom_point() + geom_smooth() +guides(fill=FALSE)+labs(title = "Installs vs. Reviews", x = "Reviews", y = "Installs")
ggplot(playstore, aes(x=log(Reviews), y=Rating)) + geom_point() + geom_smooth() +guides(fill=FALSE)+labs(title = "Rating vs. Reviews", x = "Reviews", y = "Rating")




###Plots of Installs and Rating vs. Type
ggplot(playstore, aes(x=Type, y=log(as.numeric(paste(installsRefined))), fill = Type)) + geom_boxplot() +guides(fill=FALSE)+labs(title = "Installs vs. Type", x = "Type", y = "Installs")
ggplot(playstore, aes(x=Type, y=Rating, fill = Type)) + geom_boxplot() +guides(fill=FALSE)+labs(title = "Rating vs. Type", x = "Type", y = "Rating")



###Plots of Installs and Rating vs. Content.Rating
ggplot(playstore, aes(x=Content.Rating, y=log(as.numeric(paste(installsRefined))), fill = Content.Rating)) + geom_boxplot() + theme(axis.text.x = element_text(size = 7)) + guides(fill=FALSE)+labs(title = "Installs vs. Content Rating", x = "Content Rating", y = "Installs")
ggplot(playstore, aes(x=Content.Rating, y=Rating, fill = Content.Rating)) + geom_boxplot() + theme(axis.text.x = element_text(size = 7)) + guides(fill=FALSE)+labs(title = "Rating vs. Content Rating", x = "Content Rating", y = "Rating")



###Plots of Installs and Rating vs. PriceCategory
ggplot(data=playstore, aes(x=priceCategory,y=log(as.numeric(paste(installsRefined))), fill=priceCategory)) + geom_boxplot() + scale_x_discrete(limits=c("Free", "Very Cheap", "Normal", "Expensive", "Very Expensive")) + labs(title = "Installs vs. Price Category", x = "Price Category", y = "Installs") + guides(fill=FALSE)
ggplot(data=playstore, aes(x=priceCategory,y=Rating, fill=priceCategory)) + geom_boxplot() + scale_x_discrete(limits=c("Free", "Very Cheap", "Normal", "Expensive", "Very Expensive")) + labs(title = "Rating vs. Price Category", x = "Price Category", y = "Rating") + guides(fill=FALSE)


###Plot of Rating vs. Installs
ggplot(playstore, aes(x=Rating, y=log(as.numeric(paste(installsRefined))))) + geom_point() + geom_smooth() +guides(fill=FALSE)+labs(title = "Installs vs. Ratings", x = "Ratings", y = "Installs")
ggplot(playstore, aes(x=installsRefined, y=Rating, fill = installsRefined)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + guides(fill=FALSE)+labs(title = "Rating vs. Installs", x = "Installs", y = "Rating")



###Plots of Installs and Rating vs. Last Updated Month
ggplot(playstore, aes(x=LastUpdatedMonth, y=log(as.numeric(paste(installsRefined))), fill = LastUpdatedMonth)) + geom_boxplot() + labs(title = "Installs vs. Last Updated Month", x = "Last Updated Month", y = "Installs") + guides(fill=FALSE)

ggplot(playstore, aes(x=LastUpdatedMonth, y=log(as.numeric(paste(installsRefined))), fill = Type)) + geom_boxplot() + labs(title = "Installs vs. Last Updated Month by Type", x = "Last Updated Month", y = "Installs")

ggplot(playstore, aes(x=LastUpdatedMonth, y=Rating, fill = Type)) + geom_boxplot() + labs(title = "Rating vs. Last Updated Month by Type", x = "Last Updated Month", y = "Rating")



##Regression
###Predicting Rating
####Base model gives R-squared of 10.1% and adjusted R-squared of 8.6%
ratingbase = lm(Rating~Reviews+as.numeric(paste(installsRefined))+Type+PriceUnfactor+Content.Rating+LastUpdatedMonth, data = playstore)
summ(ratingbase, digits = 3)


####Log transformed model gives R-squared of 24.2% and adjusted R-squared of 22.9%
ratinglog = lm(Rating~log(Reviews)+log(as.numeric(paste(installsRefined)))+Type+PriceUnfactor+Content.Rating+LastUpdatedMonth, data = playstore)
summ(ratinglog, digits = 3)



####Backward selection gives R-squared of 23.9% and adjusted R-squared of 23.9%
step(ratinglog, direction = 'backward')
#removed priceunfactor and content rating
ratingback =lm(formula = Rating~log(Reviews)+log(as.numeric(paste(installsRefined)))+Type+LastUpdatedMonth, data = playstore)
summ(ratingback, digits = 3)



###Predicting Installs
####Base model gives R-squared of 55.3% and adjusted R-squared of 54.5%
installbase =lm(formula = as.numeric(paste(installsRefined))~Reviews+Rating+Type+PriceUnfactor+Content.Rating+LastUpdatedMonth, data = playstore)
summ(installbase, digits = 3)



####Log transformed model gives R-squared of 93.8% and adjusted R-squared of 93.7%
installlog =lm(formula = log(as.numeric(paste(installsRefined)))~log(Reviews)+Rating+Type+PriceUnfactor+Content.Rating+LastUpdatedMonth, data = playstore)
summ(installlog, digits = 3)



####Backward selection gives R-squared of 93.7% and adjusted R-squared of 93.6%
step(installlog, direction = 'backward')
#removed LastUpdatedMonth
installback = lm(formula = log(as.numeric(paste(installsRefined))) ~ log(Reviews) + Rating + Type + PriceUnfactor + Content.Rating, data = playstore)
summ(installback, digits = 3)



####Manually removed PriceUnfactor - R-squared of 93.7% and adjusted R-squared of 93.6%
installback1 = lm(formula = log(as.numeric(paste(installsRefined))) ~ log(Reviews) + Rating + Type  + Content.Rating, data = playstore)
summ(installback1, digits = 3)



####Model with interactions gives R-squared of 93.9% and adjusted R-squared of 93.7%
installint = lm(formula = log(as.numeric(paste(installsRefined))) ~ log(Reviews) + Rating + Type*LastUpdatedMonth + Content.Rating, data = playstore)
summ(installint, digits = 3)



##Post Model Assumption Checks
###Plot of Dependent Variable - Installs
plot(log(as.numeric(paste(playstore$installsRefined))))


###Histogram of Residuals - Normality of Residuals
ggplot(installback1, aes(x=installback1$residuals)) + geom_histogram(color = 'white', fill = 'navy')



###Residuals vs. Fitted Values
ggplot(installback1, aes(x=installback1$fitted.values, y=installback1$residuals)) + geom_point() + geom_smooth(method = 'lm')





