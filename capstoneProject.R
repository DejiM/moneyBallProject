# Capstone project (Money Ball-Data science and machine learning in R-Jose Portilla-Udemy)
# 16-07-2016

package_vec <- c("dplyr", "ggplot2")
lapplypackage_vec(package_vec, require, character.only = TRUE)


# read in datasets
salariesData <- read.csv("Salaries.csv")

battingData <- read.csv("Batting.csv")

str(battingData)
summary(battingData)


# create new column BA (see https://en.wikipedia.org/wiki/Batting_average) 
battingData$BA <- with(battingData, H/AB, 2)

head(battingData$BA)

# create new variable-on base percentage (https://en.wikipedia.org/wiki/On-base_percentage)
battingData$OBP <- with(battingData, (H+BB+HBP)/(AB+BB+HBP+SF))

# create new variable-slugging percentage (https://en.wikipedia.org/wiki/Slugging_percentage)
battingData$SLG <- with(battingData, ((H-X2B-X3B-HR)+(2*X2B)+(3*X3B)+(4*HR))/(AB))

summary(battingData)
str(battingData)

# merge salary data with batting data
combo <- merge(battingData, salariesData, c("playerID", "yearID"))

# names of missing players in need of replacement
vec_names <- c("giambja01", "damonjo01", "saenzol01")

lostPlayers <- droplevels(combo[combo$playerID %in% vec_names, ] )

# constraints for replacement
# The total combined salary of the three players can not exceed 15 million dollars.
# Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
# Their mean OBP had to equal to or greater than the mean OBP of the lost players

lostPlayers2001 <- subset(lostPlayers, yearID == 2001)
sum(lostPlayers2001$AB)
mean(lostPlayers2001$OBP)
# combined AB >= 1469 (min), max salary = 15000000, mean OBP >= 0.3638687

# subset from year 2001 only
combo2001 <- subset(combo, yearID == 2001)

# randomly select 3 players from the new list
sample_n(combo2001, 3)


# function to do the task
# outputs total AB, total salary and mean OBP for any three players selected
# returns the dataframe
playerComb <- function(df){
  
  combAB <- sum(df$AB)
  combSalary <- sum(df$salary)
  meanOBP <- mean(df$OBP)
  if(combAB < 1469){
    print("AB needs to be higher-try again!")
  }
  if(combSalary > 15000000){
    print("Combined salary is too high! Try again!")
  }
  if(meanOBP < 0.3638687){
    print("mean OBP too low, keep trying!")
  }
  else if(combAB >= 1469 & combSalary <= 15000000 & meanOBP >= 0.3638687)
  print("bang on target! Correct combination")
  res <- c("AB" = combAB, "salary" = combSalary, "OBP" = meanOBP)
  return(list(res, df))

}


# subset combo2001 to include only complete data (exclude NAs)
combo2001a <- combo2001[, c("playerID", "AB", "salary", "OBP")]
combo2001b <- combo2001a[complete.cases(combo2001a), ]

# narrow down range to bring together matching combinations
combo2001c <- filter(combo2001b, OBP > 0.18, salary > 3000000 & salary < 5000000, AB > 450)

set.seed(2005)

# combo2001c is a much smaller pool of likely players
playerComb(sample_n(combo2001c, 3))


# try a few choices from result of function
choice_1 <- droplevels(combo2001b[combo2001b$playerID %in% c("venturo01", 
                                                             "johnsru01", "floydcl01"), ])
choice_2 <- droplevels(combo2001b[combo2001b$playerID %in% c("venturo01",
                                                             "mientdo01", "floydcl01"), ])
choice_3 <- droplevels(combo2001b[combo2001b$playerID %in% c("venturo01",
                                                             "pierrju01", "floydcl01"), ])


playerComb(choice_3)

# some correct answers:
# venturo01, mientdo01, floydcl01
# venturo01, pierrju01, floydcl01
# ordonma01, younger01, floydcl01
# hernajo01, abreubo01, gonzalu01
# youngdm01, heltoto01, camermi01
# gonzalu01, giambja01, erstada01
# gonzalu01, heltoto01, aurilri01
# anderga01, gutieri01, giambja01
# cirilje01, hernajo01, giambja01
# boonebr01, youngdm01, heltoto01
# posadjo01, boonebr01, giambja01
# posadjo01, heltoto01, erstada01

# To be continued-function can still be improved



























