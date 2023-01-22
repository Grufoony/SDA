# DATAFRAME INITIALIZATION
df <- read.csv("./data/population.csv")
library(ggplot2)

head(df)
unique(df$Country)
head(filter)

filter <- df[df$Country == "China",]
ggplot(data = filter, aes(x = Year, y = MedianAge)) +
    geom_point() + geom_smooth(method = "lm")
ggplot(data = filter, aes(x = Year, y = FertilityRate)) +
    geom_point() + geom_smooth(method = "lm")

head(filter)

###############################################################################
# POPULATION GROWTH ANALISYS ==== LINEAR REGRESSION
###############################################################################

# USA ANALYSIS
usa <- df[df$Country == "United States",]

model <- lm(Population ~ Year, data = usa)
summary(model)

ggplot(data = usa, aes(x = Year, y = Population)) +
    geom_point() + geom_smooth(method = "lm") +
    labs(title = "USA Population", x = "Year", y = "Population")

# PAKISTAN ANALYSIS
# Pakistan has an exponential growth -> log scale to fit a linear model

pak <- df[df$Country == "Pakistan",]
pak[, "log_Population"] <- log(pak[, "Population"] + 1)

model <- lm(log_Population ~ Year, data = pak)
summary(model)

ggplot(data = pak, aes(x = Year, y = Population)) +
    geom_point() + geom_smooth(method = "lm") +
    labs(title = "USA Population", x = "Year", y = "Population")
ggplot(data = pak, aes(x = Year, y = log_Population)) +
    geom_point() + geom_smooth(method = "lm") + 
    labs(title = "USA Population", x = "Year", y = "Population")

###############################################################################
# ==== HYPOTHESIS TESTING
###############################################################################

pop1 <- df[df$Country == "United States", "Population"]
pop2 <- df[df$Country == "Italy", "Population"]

ggplot( data = df, aes(x=Population) ) + geom_histogram( binwidth = 100000, color="black", fill="skyblue")

qqnorm(pop1, main = "Normal qq-plot (Population 1)", ylab = "Empirical Quantiles", xlab = "Theoretical Quantiles")
qqnorm(pop2, main = "Normal qq-plot (Population 2)", ylab = "Empirical Quantiles", xlab = "Theoretical Quantiles")
qqline(pop1, col="red", lty=4, lwd=2)
qqline(pop2, col="red", lty=4, lwd=2)

t.test(pop1, pop2)

############################################################################################################################################################################
# FERTILITY RATE ANALYSIS ==== MULTILINEAR REGRESSION
############################################################################################################################################################################

filter <- df[df$Country == "Albania",]
corYearAge <- cor(filter[, "Year"], filter[, "MedianAge"])
corFertilityYear <- cor(filter[, "FertilityRate"], filter[, "Year"])
corAgeFertility <- cor(filter[, "MedianAge"], filter[, "FertilityRate"])
print(c(corAgeYear, corFertilityYear, corAgeFertility))

result <- lm(FertilityRate ~ Year + MedianAge, data = filter)
summary(result)

ggplot(data = filter, aes(x = Year, y = MedianAge)) + geom_point() + geom_smooth(method = "lm")

ggplot(data = filter, aes(x = MedianAge, y = exp_FertilityRate)) + geom_point() + geom_smooth(method = "lm")


medianAge <- filter[, "MedianAge"]
fertilityRate <- filter[, "FertilityRate"]
t.test(medianAge, fertilityRate)
