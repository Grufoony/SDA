# DATAFRAME INITIALIZATION
df <- read.csv("./data/population.csv")
library(ggplot2)

head(df)
unique(df$Continent)

###############################################################################
# POPULATION GROWTH ANALISYS ==== LINEAR REGRESSION
###############################################################################

# USA ANALYSIS
usa <- df[df$Country == "United States", ]

model <- lm(Population ~ Year, data = usa)
summary(model)

# Pearson's product-moment correlation
cor.test(usa$Population, usa$Year)

ggplot(data = usa, aes(x = Year, y = Population)) +
    geom_point() + geom_smooth(method = "lm") +
    labs(title = "U.S.A. Population", x = "Year", y = "Population")
ggsave("./img/usa.png")

# PAKISTAN ANALYSIS
# Pakistan has an exponential growth -> log scale to fit a linear model

pak <- df[df$Country == "Pakistan", ]
pak[, "log_Population"] <- log(pak[, "Population"] + 1)

model <- lm(log_Population ~ Year, data = pak)
summary(model)
# Pearson's product-moment correlation
cor.test(pak$Population, pak$Year)
cor.test(pak$log_Population, pak$Year)

ggplot(data = pak, aes(x = Year, y = Population)) +
    geom_point() + geom_smooth(method = "lm") +
    labs(title = "Pakistan population", x = "Year", y = "Population")
ggsave("./img/pak.png")
ggplot(data = pak, aes(x = Year, y = log_Population)) +
    geom_point() + geom_smooth(method = "lm") +
    labs(title = "Pakistan Population", x = "Year", y = "Population logarithm")
ggsave("./img/pak_log.png")

###############################################################################
# POPULATION GROWTH BETWEEN CONTINENTS==== HYPOTHESIS TESTING
###############################################################################

unique(df$Continent)

oceania <- log(df[df$Continent == "Oceania", "Population"])
aisa <- log(df[df$Continent == "Asia", "Population"])

ggplot() +
    geom_histogram(aes(x = oceania), fill = "red", alpha = 0.5, bins = 20) +
    geom_histogram(aes(x = asia), fill = "blue", alpha = 0.5, bins = 20)

qqnorm(oceania, main = "Normal qq-plot (Oceania)",
    ylab = "Empirical Quantiles", xlab = "Theoretical Quantiles")
qqline(oceania, col = "red", lty = 4, lwd = 2)
ggsave("./img/qqnorm_oceania.png")
qqnorm(asia, main = "Normal qq-plot (Population 2)",
    ylab = "Empirical Quantiles", xlab = "Theoretical Quantiles")
qqline(asia, col = "red", lty = 4, lwd = 2)

t.test(oceania, asia)

###############################################################################
# YEARLY CHANGE ==== MULTILINEAR REGRESSION
###############################################################################

head(df)
filter <- df[df$Country == "France", ]

result <- lm(Yearly.Change ~ Population + Migrants.net.
    + FertilityRate + MedianAge + UrbanPopulation, data = filter)
summary(result)


###############################################################################
###############################################################################
#other things to try

corYearAge <- cor(filter[, "Year"], filter[, "MedianAge"])
corFertilityYear <- cor(filter[, "FertilityRate"], filter[, "Year"])
corAgeFertility <- cor(filter[, "MedianAge"], filter[, "FertilityRate"])
print(c(corYearAge, corFertilityYear, corAgeFertility))


# plots are not that good

ggplot(data = filter, aes(x = Migrants.net., y = Yearly.Change)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = filter, aes(x = Migrants.net., y = FertilityRate)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = filter, aes(x = Migrants.net., y = MedianAge)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = filter, aes(x = Migrants.net., y = UrbanPopulation)) + geom_point() + geom_smooth(method = "lm")

ggplot(data = filter, aes(x = MedianAge, y = exp_FertilityRate)) + geom_point() + geom_smooth(method = "lm")