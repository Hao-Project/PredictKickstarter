# Subset data, keep those have succeeded
succeeded <- pData[pData$percentage > 1,]
summary(succeeded)

# Break data according to the amount of pledged money relative to the campaign's goal
perBreaks <- c(1,1.1,1.2,2,Inf)
succeeded$cut <- cut(succeeded$percentage,breaks = perBreaks)
str(succeeded$cut)

count <- with(data = succeeded, tapply(revenue,cut, length))
sumRevenue <- with(data = succeeded, tapply(revenue,cut, sum))

# Draw Pie Graph
lbls <- c("(1,1.1]","(1.1,1.2]","(1.2,2]","(2,Inf)" )

png(file = "Breakdown.png", width = 960, height = 480)
par(mfrow=c(1,2),oma = c(1.5, 0, 3, 0),mar = c(0,0,2,0) + 0.1)
pctC <- round(count / sum(count) * 100)
lblsC <- paste0(" ",pctC, "%")  # add percents to labels 
pie(count, labels = lblsC,main = "Share of Successful Projects", col =  rainbow(4))
legend("topright", title = "Funding Ratio", lbls, fill = rainbow(4))
pctP <- round(sumRevenue / sum(sumRevenue) * 100)
lblsS <- paste0(" ",pctP, "%")  # add percents to labels 
pie(sumRevenue, labels = lblsS, main = "Share of Total Revenue", col =  rainbow(4))
#legend("topleft", title = "Funding Ratio", lbls, fill = rainbow(4))
mtext("Breakdown of Successful Projects on Kickstarter.com,\n Sep 2012 - May 2013", outer = TRUE, cex = 1.5)
mtext("Note: Funding Ratio = Amount of money pledged / Funding Goal", outer = TRUE, side = 1,cex = 1.2)
dev.off()


# Histogram
library(lattice)
pData$percentage2 <- pData$percentage
pData$percentage2[pData$percentage >2] <- 2 
hist(pData$percentage2)

png(file = "Hist.png", width = 960, height = 720)
p <- qplot(percentage2, data=pData, geom="histogram",binwidth=0.1)
p + xlab("Funding Ratio") + ylab("Number of Projects") + 
    ggtitle("Distribution of Funding Ratio: Amount of Money Pledged / Funding Goal") + 
    annotate("text", x = 2.1, y = 1700, label = "Funding Ratio > 2 \nCombined")
dev.off()


