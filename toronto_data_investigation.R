# df<-read.csv("**insert_path_to_csv_file_here**/Toronto_temp_edited.csv", header=T)
attach(df)
df$Season <- as.factor(df$Season)
df$Year <- as.factor(df$Year)

# Scatterplot of mean temperature vs total precipitation of all data (2013-2018)
plot(MeanTemp, TotalPrecip, main = 'Precipitation vs. Temperature from 2013-2018', 
     xlab = 'Mean Temperature (C)', ylab = 'Total Precipitation (mm)')
abline(lm(MeanTemp~TotalPrecip))

summary(df)

# Correlation
cor.test(TotalPrecip,MeanTemp)

# Model Fitting
summary(lm(TotalPrecip~MeanTemp))

# Hypothesis Tests
anova(lm(TotalPrecip~MeanTemp))

# Prediction Interval
model <- lm(TotalPrecip~MeanTemp)
fr = data.frame(MeanTemp = 10) 
predict(model,newdata=fr,interval="prediction") 

# Boxplot
plot(TotalPrecip~factor(Year), main = '2013-2018 Annual Precipitation Distribution', 
     xlab='Year', ylab = 'Total Precipitation (mm)', ylim=c(0,8)) 

# Interaction Investigation
interaction.plot(factor(Season), factor(Year), TotalPrecip, 
                 fun = mean,ylim = range(0:3),col=c("blue","red","green", "orange", "black", "purple"),
                 main = "Interaction Plot", xlab = "Season", ylab = "Total Precipitation")

summary(lm(TotalPrecip~factor(Season) + factor(Year) + factor(Year)*factor(Season)))

# Creating and Testing Models

model1 <- lm(TotalPrecip~MeanTemp)
model2 <- lm(TotalPrecip~factor(Year))
model3 <- lm(TotalPrecip~factor(Season))
model4 <- lm(TotalPrecip~MeanTemp+factor(Year))
model5 <- lm(TotalPrecip~factor(Year)+factor(Season))
model6 <- lm(TotalPrecip~MeanTemp+factor(Season))
model7 <- lm(TotalPrecip~MeanTemp+factor(Year)+factor(Season))
model8 <- lm(TotalPrecip~MeanTemp+factor(Year)+MeanTemp*factor(Year))

library(leaps)
library(MPV)
library(olsrr)
library(sp)

AIC(model1,model2,model3,model4,model5,model6,model7,model8)

BIC(model1,model2,model3,model4,model5,model6,model7,model8)

PRESS(model1)
PRESS(model2)
PRESS(model3)
PRESS(model4)
PRESS(model5)
PRESS(model6)
PRESS(model7)
PRESS(model8)
 
model1_summ <- summary(model1)
mean(model1_summ$residuals^2)
model2_summ <- summary(model2)
mean(model2_summ$residuals^2)
model3_summ <- summary(model3)
mean(model3_summ$residuals^2)
model4_summ <- summary(model4)
mean(model4_summ$residuals^2)
model5_summ <- summary(model5)
mean(model5_summ$residuals^2)
model6_summ <- summary(model6)
mean(model6_summ$residuals^2)
model7_summ <- summary(model7)
mean(model7_summ$residuals^2)
model8_summ <- summary(model8)
mean(model8_summ$residuals^2)

model1_summ$r.squared
model1_summ$adj.r.squared
model2_summ$r.squared
model2_summ$adj.r.squared
model3_summ$r.squared
model3_summ$adj.r.squared
model4_summ$r.squared
model4_summ$adj.r.squared
model5_summ$r.squared
model5_summ$adj.r.squared
model6_summ$r.squared
model6_summ$adj.r.squared
model7_summ$r.squared
model7_summ$adj.r.squared
model8_summ$r.squared
model8_summ$adj.r.squared

anova(model4,model7) 
anova(model5,model7) 
