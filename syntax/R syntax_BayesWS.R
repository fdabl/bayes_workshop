
# Set working directory; set your own directory here!
setwd("C:/Users/petere/Documents/1I_ConfCongWorS/2015 EFPSA Congress Czech/bayes_workshop/data")

risk_bayes <- read.csv("riskdata_bayes.csv")

attach(risk_bayes)

# Multiple regression
summary(lm(risksum~ZPop+ZLike))