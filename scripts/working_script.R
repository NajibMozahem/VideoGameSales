##load libraries
library(tidyverse)
library(tidyr)
library(ggthemes)
library(caret)
library(ranger)
library(randomForest)

##read the csv file
sales <- read_csv("data/Video_Games_Sales_as_at_22_Dec_2016.csv")

##inspect data set
##are there na's?
colSums(is.na(sales))
##there are too many missing values. Leave only records with complete information
sales <- sales[complete.cases(sales), ]
colSums(is.na(sales))
##no more na's. If we look at the structure, we note that year of release is not
##numeric.
str(sales)
##convert it to numeric and see what happens
as.integer(sales$Year_of_Release)
##notice that some NA values were introduced. Why?
unique(sales$Year_of_Release)
##we see that there is a string "N/A" that was missed when we checked for
##na's. Remove these records:
sales <- sales[sales$Year_of_Release != "N/A", ]
unique(sales$Year_of_Release)
##there are no more "N/A"s. now convert year to integer
sales$Year_of_Release <- as.integer(sales$Year_of_Release)
##there is a missing publisher
sum(sales$Publisher=="N/A")
##Remove the record
sales <- sales[sales$Publisher != "N/A", ]
##check other strings
sum(sales$Developer == "N/A")
sum(sales$Rating == "N/A")
#no more missing
##look at the sales figures to see whether there are strange 
##outliers
summary(sales$NA_Sales)
summary(sales$EU_Sales)
summary(sales$JP_Sales)
summary(sales$Other_Sales)
summary(sales$Global_Sales)
##looks good. Now look at other numbers
summary(sales$Critic_Score)
summary(sales$Critic_Count)
summary(sales$User_Count)
summary(sales$User_Score)
#User_score is character. Convert to numeric:
sales$User_Score <- as.numeric(sales$User_Score)
##check user score summary again
summary(sales$User_Score)
##notice that user score is out of ten while critic score is out of 100
##it would make sense to have them both out of 100
sales$User_Score <- sales$User_Score * 10

##check out the rating
sales %>% count(Rating)
##we see that there is only one record for some ratings. Collapse the
##variable so that these few observations in these categories do not
##distort our results
sales <- sales %>% mutate(Rating = ifelse(Rating == "AO", "M", Rating))
sales <- sales %>% mutate(Rating = ifelse(Rating == "K-A", "E", Rating))
sales <- sales %>% mutate(Rating = ifelse(Rating == "RP", "E", Rating))
##now look at the frequency of each
sales %>% count(Rating) %>% ggplot() + geom_bar(aes(Rating, n), stat = "identity")
##now check sales
sales %>% group_by(Rating) %>% summarise(sales = sum(Global_Sales)) %>% 
  ggplot() + geom_bar(aes(Rating, sales), stat = "identity", fill = "blue")

################################ We are good to go
##################################################
##look at global sales for each title
ggplot(sales) + geom_histogram(aes(Global_Sales))
##the variable is skewed significantly. It would 
##therefore make sense to log scale axis:
ggplot(sales) + geom_histogram(aes(Global_Sales)) + 
  scale_x_log10()
#based on this, it would be best to take the log of the variable 
#later when we fit the models

##Check out the histogram of the years to know if the database
##contains video games from a certain time or they are evenly
##distributed
sales %>% group_by(Year_of_Release) %>% 
  count() %>% ggplot() + 
  geom_bar(aes(Year_of_Release, n), stat = "identity", 
           fill = "blue") + theme(axis.text.x = element_text(angle = 90))
##we see that there is a clear peak. Let us look at the sales
##in each year and compare to the release. 
color <- c("Titles released" = "red", "Global sales" = "blue")
sales %>% group_by(Year_of_Release) %>% 
  summarise(sales = sum(Global_Sales), count = n()) %>% 
  ggplot() + geom_line(aes(Year_of_Release, count, group = 1, color = "Titles released")) + 
  geom_line(aes(Year_of_Release, sales, group = 1, color = "Global sales")) + 
  xlab("Year of Release") + ylab("Titles released") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
  scale_color_manual(values = color) + labs(color = "")
##we see that the more titles, the more the revenue
##we see that the peak sales corresponds to the peak in the 
##year histograms. So the data in the database is mostly from
##2007-2009. this can be seen by looking at the median and mean
summary(sales$Year_of_Release)

##now look at the sales in each geographic area
sales %>% gather(area, sales, NA_Sales:Other_Sales, 
                 factor_key = TRUE) %>% 
  group_by(area, Year_of_Release) %>% 
  summarise(sales = sum(sales)) %>% 
  ggplot() + 
  geom_line(aes(Year_of_Release, sales, group = area, color = area)) + 
  xlab("Year of release") + ylab("Sales") + labs(color = "") + 
  theme(legend.text = element_text(size = 7), 
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))
## NA is highest, followed by Europe

##let us look at the same graph for the platforms and
## global sales
##first the platforms
sales %>% group_by(Platform) %>% 
  summarise(sales = sum(Global_Sales)) %>% ggplot() + 
  geom_bar(aes(reorder(Platform, sales), sales), stat = "identity", 
           fill = "blue") + 
  xlab("Platform") + ylab("Global sales") + 
  coord_flip()
##data set contains mostly records for ps2 and x360

##create new platform variable that further collapses the 
##levels of the original platform
sales <- sales %>% mutate(platform2 = case_when(
  Platform %in% c("Wii", "DS", "3DS", "WiiU", "GC", "GBA") ~ "Nintendo",
  Platform %in% c("X360", "XB", "XOne") ~ "XBox",
  Platform %in% c("PS3", "PS4", "PS2", "PS", "PSP", "PSV") ~ "PS",
  Platform == "PC" ~ "PC",
  Platform == "DC" ~ "Sega"
)) 
##Now plot the global sales each year for each
sales %>% group_by(platform2, Year_of_Release) %>%
  summarise(sales = sum(Global_Sales)) %>% 
  ggplot() + 
  geom_line(aes(Year_of_Release, sales, group = platform2, color = platform2)) +
  xlab("Year of release") + ylab("Global Sales") + labs(color = "") + 
  theme(legend.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5, size = 6))

##now look at global sales for genres
sales %>% group_by(Genre) %>%
  summarise(sales = sum(Global_Sales)) %>%  
  ggplot() + 
  geom_bar(aes(reorder(Genre, sales), sales), stat = "identity", 
           fill = "blue") + 
  ylab("Global Sales") + xlab("Genre") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5)) + 
  coord_flip()

##look at the global sales per developer
sales %>% group_by(Developer) %>% 
  summarise(sales = sum(Global_Sales)) %>% 
  arrange(desc(sales)) %>% slice(1:10) %>% 
  ggplot() + 
  geom_bar(aes(reorder(Developer, sales), sales), stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Developer") + ylab("Global sales")
##Nintendo develops the highest grossing games

##look at the global sales for each platform in each genre
sales %>% group_by(Platform, Genre) %>% 
  summarise(sales = sum(Global_Sales)) %>% 
  ggplot() + geom_raster(aes(Genre, Platform, fill = sales)) + 
  ylab("") + xlab("") + 
  scale_fill_gradient(low = "#ebebe5", high = "red") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 5),
        legend.text = element_text(size = 7)) + labs(fill = "Sales")

##now the same but from other platform variable
##note that I remove sega because there are too few records and it is
##just taking up space
sales %>% filter(platform2 != "Sega") %>% group_by(platform2, Genre) %>% 
  summarise(sales = sum(Global_Sales)) %>% 
  ggplot() + geom_raster(aes(Genre, platform2, fill = sales)) + 
  ylab("Platform") + scale_fill_gradient(low = "#ebebe5", high = "red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
##we see that the most popular is PS, specifically in the action genre

##Now let us look at the relationship between critic and user scores
## and sales in each area
colors <- c("Critic score" = "blue", "User score" = "red")
ggplot(sales) + 
  geom_smooth(aes(Critic_Score, Global_Sales, color = "Critic score")) + 
  geom_smooth(aes(User_Score, Global_Sales, color = "User score")) +
  labs(color = "") + xlab("Score") + ylab("Global sales") + 
  scale_color_manual(values = colors)
##we note that there is a stronger relationship between the critic score and the
##sales than between user score. We also note that in both cases the relationship
##seems to kick in at high values of scores. The relationship is not linear



##top publishers
publishers_top <- (sales %>% group_by(Publisher) %>%
  summarise(sales = sum(Global_Sales)) %>% arrange(desc(sales)) %>% 
  top_n(10) %>% distinct(Publisher))$Publisher
##top developers
developers_top <- (sales %>% group_by(Developer) %>%
                    summarise(sales = sum(Global_Sales)) %>% arrange(desc(sales)) %>% 
                    top_n(10) %>% distinct(Developer))$Developer

sales <- sales %>% 
  mutate(publisher_top = ifelse(Publisher %in% publishers_top, TRUE, FALSE),
         developer_top = ifelse(Developer %in% developers_top, TRUE, FALSE))

##number of platforms
sales <- sales %>% group_by(Name) %>% mutate(num_of_platforms = n()) %>% ungroup(Name)

####Create train and test sets

set.seed(1982, sample.kind = "Rounding")

test_index <- createDataPartition(sales$Global_Sales, p = 0.9, list = FALSE)
train_set <- sales[-test_index, ]
test_set <- sales[test_index, ]

##make sure that all levels of the predictors are present in
##the train set
totalData <- rbind(train_set, test_set)
for (f in 1:length(names(totalData))) {
  levels(train_set[, f]) <- levels(totalData[, f])
}

# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

##linear model
model_lm <- train(log(Global_Sales) ~ Critic_Score + 
                    User_Score + Genre + 
                    Year_of_Release + Platform + Critic_Count +
                    User_Count + Rating + 
                    publisher_top + developer_top + 
                    num_of_platforms, method = "lm", data = train_set)
test_set$predicted_lm <- predict(model_lm, test_set)
rmse_results <- data.frame(Method = "Linear regression", 
                           RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_lm))

## SVMLinear
model_svm <- train(log(Global_Sales) ~ Critic_Score + 
                     User_Score + Genre + 
                     Year_of_Release + Platform + Critic_Count +
                     User_Count + Rating + 
                     publisher_top + developer_top + 
                     num_of_platforms, method = "svmLinear",
                   data = train_set)
test_set$predicted_svm <- predict(model_svm, test_set)
rmse_results <- rmse_results %>% 
  add_row(Method = "SVM Linear", 
          RMSE = RMSE(log(test_set$Global_Sales), 
                      test_set$predicted_svm))

##run a random forest model with cross validation
cntrl <- trainControl(method = "repeatedcv", number = 10,
                      repeats = 3)
tunegrid <- expand.grid(.mtry=c(1:5), 
                        .min.node.size = seq(1, 5, 1),
                       .splitrule = c("extratrees", "variance"))
model_rf <- train(log(Global_Sales) ~ Critic_Score + 
                    User_Score + Genre + 
                    Year_of_Release + Platform + Critic_Count +
                    User_Count + Rating + 
                    publisher_top + developer_top + 
                    num_of_platforms, data = train_set,
                  method = "ranger", trControl = cntrl,
                  tuneGrid = tunegrid)
test_set$predicted_rf <- predict(model_rf, test_set)
rmse_results <- rmse_results %>% add_row(Method = "Random forest", RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_rf))
rmse_results
##visually check whether the predicted values are close to the
##true values
ggplot(test_set) + 
  geom_point(aes(log(Global_Sales), predicted_rf)) + 
  geom_line(aes(log(Global_Sales), log(Global_Sales))) + 
  xlab("Actual values") + ylab("Predicted values") + 
  labs(caption = 
         paste("R-squared", 
               format(model_rf$finalModel$r.squared, 
                      digits = 2)))
##check the errors to see if there is a pattern
ggplot(test_set) + geom_point(aes(log(Global_Sales) - predicted_rf,
                                  Global_Sales)) + 
  xlab("Error") + ylab("Global sales")
##it seems that the errors are largest for larger values of
##global sales. the model is not performing well in that area

##check the variable importance for the random forest:
nodesize_best <- model_rf$bestTune$min.node.size
mtry_best <- model_rf$bestTune$mtry
model_rf_final <- randomForest(log(Global_Sales) ~ Critic_Score + 
                       User_Score + Genre + 
                       Year_of_Release + Platform + Critic_Count +
                       User_Count + Rating + 
                       publisher_top + developer_top + 
                       num_of_platforms, data = train_set, mtry = mtry_best, nodesize_best = nodesize)

as.data.frame(importance(model_rf_final)) %>% arrange(desc(IncNodePurity))

