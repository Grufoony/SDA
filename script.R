# DATAFRAME INITIALIZATION
df = read.csv("./data/population.csv")
library(ggplot2)

head(df)
unique(df$Country)
head(filter)

filter <- df[df$Country == "China",]
ggplot(data = filter, aes(x = Year, y = MedianAge)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = filter, aes(x = Year, y = FertilityRate)) + geom_point() + geom_smooth(method = "lm")

head(filter)
ggplot(data = db, aes(x=Educational_level) ) + geom_bar(width = 0.5) 
ggplot(data = db, aes(x = age, y = charges), fill = smoker) + geom_boxplot( notch = TRUE ) + stat_boxplot( geom = "errorbar" )

############################################################################################################################################################################
# POPULATION GROWTH ANALISYS ==== LINEAR REGRESSION
############################################################################################################################################################################

# USA ANALYSIS
usa <- df[df$Country == "United States",]

ggplot(data = usa, aes(x = Year, y = Population)) + geom_point() + geom_smooth(method = "lm") + labs(title = "USA Population", x = "Year", y = "Population")

# PAKISTAN ANALYSIS
# NOTE pakistan has an exponential growth so we need to use log scale to fit a linear model

pak <- df[df$Country == "Pakistan",]

pak[, "log_Population"] <- log(pak[, "Population"] + 1)
ggplot(data = pak, aes(x = Year, y = Population)) + geom_point() + geom_smooth(method = "lm") + labs(title = "USA Population", x = "Year", y = "Population")
ggplot(data = pak, aes(x = Year, y = log_Population)) + geom_point() + geom_smooth(method = "lm") + labs(title = "USA Population", x = "Year", y = "Population")

############################################################################################################################################################################
# FERTILITY RATE ANALYSIS ==== MULTILINEAR REGRESSION
############################################################################################################################################################################

filter <- df[df$Country == "Albania",]
filter[, "exp_FertilityRate"] <- sqrt(filter[, "FertilityRate"])
ggplot(data = filter, aes(x = MedianAge, y = exp_FertilityRate)) + geom_point() + geom_smooth(method = "lm")


medianAge <- filter[, "MedianAge"]
fertilityRate <- filter[, "FertilityRate"]
t.test(medianAge, fertilityRate)
