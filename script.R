# DATAFRAME INITIALIZATION
df = read.csv("./data/population.csv")
library(ggplot2)

head(df)
unique(df$Country)
head(filter)

filter <- df[df$Country == "Italy",]
ggplot(data = filter, aes(x = Year, y = MedianAge)) + geom_point() + geom_smooth(method = "lm")


head(filter)
ggplot(data = db, aes(x=Educational_level) ) + geom_bar(width = 0.5) 
ggplot(data = db, aes(x = age, y = charges), fill = smoker) + geom_boxplot( notch = TRUE ) + stat_boxplot( geom = "errorbar" )

# USA ANALYSIS
usa <- df[df$Country == "United States",]

ggplot(data = usa, aes(x = Year, y = Population)) + geom_point() + geom_smooth(method = "lm") + labs(title = "USA Population", x = "Year", y = "Population")

# PAKISTAN ANALYSIS
# NOTE pakistan has an exponential growth so we need to use log scale to fit a linear model

pak <- df[df$Country == "Pakistan",]

pak[, "log_Population"] <- log(pak[, "Population"] + 1)
ggplot(data = pak, aes(x = Year, y = Population)) + geom_point() + geom_smooth(method = "lm") + labs(title = "USA Population", x = "Year", y = "Population")
ggplot(data = pak, aes(x = Year, y = log_Population)) + geom_point() + geom_smooth(method = "lm") + labs(title = "USA Population", x = "Year", y = "Population")
