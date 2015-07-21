setwd("C:/Data/Kickstarter Project")

# Read data percentage and 
fundedPerc <- read.csv("kickstarter-etter-cosn2013/statusPercentage.csv")
names(fundedPerc) <- c("percentage","numBackers")
str(fundedPerc)
head(fundedPerc)

# Read project goal and status
projectData <- read.csv("kickstarter-etter-cosn2013/projects.csv")
names(projectData) <- c("id","goal","status","startDate","endDate")
str(projectData)
head(projectData)

# Merge data
pData <- merge(fundedPerc, projectData, by="row.names", all=TRUE)
str(pData)
head(pData)

# Create revenue variable
pData$revenue <- pData$percentage * pData$goal * pData$status * 0.05

# Drop projects that ask for less or equal to $100. 
sum(pData$goal <= 100) / nrow(pData) * 100
sum(pData$revenue[pData$goal <= 100]) / sum(pData$revenue) * 100

pData <- pData[pData$goal > 100,]
summary(pData)

