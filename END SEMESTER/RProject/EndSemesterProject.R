# QUESTION 1 OBSERVATIONS AND FEATURES
dim(Yield_Factors_All_Years)
nrow(Yield_Factors_All_Years)
ncol(Yield_Factors_All_Years)

# QUESTION 2 GETTING THE STRUCTURE AND SUMMARY OF THE DATA SET
# STRUCTURE
str(Yield_Factors_All_Years)

#SUMMARY USING SUMMARY()
summary(Yield_Factors_All_Years)

# QUESTION 3 FILTER GRAZING LAND 2016
library(dplyr)
Yield_Factors_All_Years %>% 
  filter(Year == 2016, Land_type == "Grazing Land")

# QUESTION 4 NEW COLUMN "YIELD_FACTOR" WITH NATIONAL_YIELD DIVIDED WITH WORLD_YIELD AS IT'S QUOTIENT
Yield_Factors_All_Years ["YIELD_FACTOR"] <- Yield_Factors_All_Years$National_Yield/Yield_Factors_All_Years$World_Yield
Yield_Factors_All_Years

# ANOTHER WAY OF CREATING A NEW COLUMN
library(dplyr)
Yield_Factors_All_Years %>% 
  mutate(Yield_Factor = National_Yield/World_Yield)

# QUESTION 5 CREATE AN "INFRASTRUCTURE" DATASET AND USE IT TO GENERATE A SCATTER PLOT WITH "NATIONAL_YIELD" AS ITS X-AXIS AND "WORLD_YIELD" AS ITS Y-AXIS.
plot(Yield_Factors_All_Years$National_Yield, Yield_Factors_All_Years$World_Yield)
# To appropriately plot the X and Y Values
plot(Yield_Factors_All_Years$National_Yield, Yield_Factors_All_Years$World_Yield, xlab = 'National_Yield', ylab = 'World_Yield', main = 'Infrastrucutre')


# QUESTION 6 CREATE A SCATTER PLOT OF "YEAR_2016" WITH X-AXIS REP "NATIONAL_YIELD" AND Y-AXIS "WORLD_YIELD" AND FACETED TO HAVE ONE SUBPLOT PER "LAND_TYPE"
library(ggplot2)
library(dplyr)
Year_2016 <- Yield_Factors_All_Years %>% 
  filter(Year == 2016)
ggplot(Year_2016, aes(X = National_Yield, Y = World_Yield)) +
  geom_point(aes(x = National_Yield, y = World_Yield))+
  scale_x_log10()+
  facet_wrap(~ Land_type)

# QUESTION 7 MEAN AND MEDIAN
Year_2016 %>%
  filter(Year == 2016, Land_type == "Marine Fishing Grounds")
median(Year_2016$National_Yield, na.rm = TRUE)
Year_2016 %>%
  filter(Year == 2016, Land_type == "Grazing Land")
median(Year_2016$World_Yield, na.rm = TRUE)
Year_2016 %>%
  filter(Year == 2016, Land_type == "Forest Land")
median(Year_2016$YIELD_FACTOR, na.rm = TRUE)

# QUESTION 8 CREATE A LINE PLOT SHOWING THE CHANGE IN MEDIAN YILED FACTOR BY Land_type overtime
library(ggplot2)
library(dplyr)
Yield_Factors_All_Years %>% 
  filter(median(YIELD_FACTOR))
ggplot(Year_2016, aes(X = National_Yield, Y = World_Yield)) +
  geom_point(aes(x = National_Yield, y = World_Yield))+
  scale_x_log10()+
  facet_wrap(~ Land_type)