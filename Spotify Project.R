setwd("~/Desktop/STA4211")

###DATA CLEANING###
spotify <- read.csv("top10s (1).csv")
spotify <- data.frame(spotify[ , -1])
spotify$year <- substr(spotify$year, 4, 4) #coded year variables (2010-2019)
spotify$year <- factor(spotify$year)
spotify$year #use years as blocks ??
genrecodelist <- list()
for(i in 1:nrow(spotify))
{
  if(length(grep("pop", spotify$top.genre[i])) == 1)
  {
    genrecodelist[[i]] <- 1
  }
  else
  {
    genrecodelist[[i]] <- 0
  }
}
genrecode <- unlist(genrecodelist)
spotify <- data.frame(spotify, genrecode)
spotify$genrecode <- factor(spotify$genrecode)
rm(genrecode)
which(spotify$pop == 0)
spotify <- data.frame(spotify[c(-51, -139, -268, -363, -443), ])
attach(spotify)
#####################

###ASSUMPTIONS###
library(ggplot2)
pdf("residuals v fitted.pdf")
ggplot() + aes(residuals(mod), fitted.values(mod)) + geom_point(color = "cornflowerblue") + geom_hline(yintercept = 66.17907, color = "black") + ggtitle("Residuals vs. Predicted Values") + xlab("Residuals") + ylab("Fitted Values") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
dev.off()
pdf("normal qq plot.pdf")
ggplot(data = spotify, aes(sample = pop)) + stat_qq(color = "cornflowerblue") + stat_qq_line() + ggtitle("Normal Probability Plot") + xlab("Expected Value") + ylab("Residual") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
dev.off()
pdf("nrgy v pop.pdf")
ggplot() + aes(nrgy, pop) + geom_point(color = "cornflowerblue") + geom_smooth(method = "lm", color = "black", se = FALSE) + labs(title = "Energy Score vs. Popularity Score", subtitle = "Weak, negative correlation of -0.09176731") + xlab("Energy") + ylab("Popularity") + theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic"))
dev.off()
pdf("dnce v pop.pdf")
ggplot() + aes(dnce, pop) + geom_point(color = "cornflowerblue") + geom_smooth(method = "lm", color = "black", se = FALSE) + labs(title = "Danceability Score vs. Popularity Score", subtitle = "Weak, positive correlation of 0.07384394") + xlab("Danceability") + ylab("Popularity") + theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic"))
dev.off()
pdf("db v pop.pdf")
ggplot() + aes(dB, pop) + geom_point(color = "cornflowerblue") + geom_smooth(method = "lm", color = "black", se = FALSE) + labs(title = "Loudness vs. Popularity Score", subtitle = "Weak, positive correlation of 0.02541728") + xlab("Loudness (dB)") + ylab("Popularity") + theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic"))
dev.off()
pdf("dur v pop.pdf")
ggplot() + aes(dur, pop) + geom_point(color = "cornflowerblue") + geom_smooth(method = "lm", color = "black", se = FALSE) + labs(title = "Duration vs. Popularity Score", subtitle = "Weak, negative correlation of -0.1129635") + xlab("Duration (seconds)") + ylab("Popularity") + theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5, face = "italic"))
dev.off()
#################

###ANALYSIS###
library(MASS)
stepAIC(lm(pop~bpm+nrgy+dnce+dB+live+val+dur+acous+spch+genrecode), direction = "both")
mod <- lm(pop~nrgy+dnce+dB+dur)
summary(mod)
#############



