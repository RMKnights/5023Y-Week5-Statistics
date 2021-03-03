### 03/03/2021, Week 5 workshop

library(tidyverse)
library(modelr)
library(car)
library(qqplotr)
library(praise)
library(patchwork)
library(stargazer)
library(GGally)

mammals <- read.csv("data/mammals.csv")

### this data was used to analyze the relationship between constitutional
### and ecological factors and sleeping in mammals. two qualitatively 
### different sleep variables (dreaming and non dreaming) were 
### recorded. constitutional variables such as life span, body weight, 
### brain weight and gestation time were evaluated. ecological 
### variables such as severity of predation, safety of sleeping place
### and overall danger were inferred from field observations in the 
### literature

mammals %>% 
  select(!species) %>% 
  GGally::ggpairs()

p <- mammals %>% 
  ggplot(aes(x=gestation, y=body_wt))+
  geom_point()+
  ggtitle("Gestation (days) against \n Body Weight (kg)")

p
### linear relationship: Maybe, though it is difficult to see because 
### of the very narrow distribution of bodyweights across the mammals

### modeling concerns, definitely - it would be difficult to fit a 
### good least squares line through this data

### clearly two very large outliers on both gestation and body weight
### at the high end of the scale

### also clearly a funnel shape as the values increase, so major 
### concerns there will be heteroscedasticity (unequal residual 
### variance across the fit of our regression).

gestation_model <- lm(body_wt~gestation, data=mammals)
summary(gestation_model)
### slope of the line = 4.12
### y-intercept = -394.97

tidy_gestation <- broom::tidy(gestation_model, conf.int=T)
tidy_gestation
tidy_gestation$estimate[1]### extracting intercept
tidy_gestation$estimate[2]### extracting slope

glance_gestation <- broom::glance(gestation_model)
glance_gestation
### degrees of freedom, F-statistic, p-value, etc.

augment_gestation <- broom::augment(gestation_model, interval="confidence")
augment_gestation
### predicted values and the residuals for each species adult body 
### weight for their gestation period

augment_gestation %>% 
  ggplot(aes(x = .std.resid)) +
  geom_histogram()+
  ggtitle("Histogram of the model residuals")
augment_gestation %>%
  ggplot(aes(sample = .std.resid)) +
  geom_qq()+
  stat_qq_line()+
  ggtitle("QQ plot")
### at least one outlier, overall left-skew with one extreme outlier

augment_gestation %>%
  ggplot(aes(x=.fitted, y= .std.resid)) +
  geom_point()+
  ggtitle("Standardised residuals against Fitted values from the model")
augment_gestation %>%
  ggplot(aes(x=.fitted, y= .resid)) +
  geom_point()+
  ggtitle("Residuals against Fitted values from the model")
### pattern is identical, the scale is very different
### because residuals are on the original scale of the dependent 
### variable, whereas your standardised residuals have been fitted 
### onto a distribution of standard deviations
### Clearly both an increase in the variance of the residuals as the 
### fitted values increase AND there is a clear trend in the residuals

cook_limit <- as.numeric(4 / count(augment_gestation))
cook_limit
### augmented used incase of NA values being dropped somewhere

augment_gestation %>% 
  ggplot(aes(x = as.numeric(rownames(augment_gestation)), y = .cooksd)) +
  geom_col() +
  geom_hline(yintercept = cook_limit,
             color = "red",
             linetype = "dashed")+
  ggtitle("Cook's Distance")

augment_gestation %>% 
  ggplot(aes(x=gestation, y=body_wt))+
  geom_line(aes(x=gestation, y=.fitted))+
  geom_line(aes(x=gestation, y=.upper), linetype="dashed")+
  geom_line(aes(x=gestation, y=.lower), linetype="dashed")+
  geom_point()+
  ggtitle("Linear trend")
### data not a great fit for this model

plot1 <- mammals %>% 
  ggplot(aes(gestation))+
  geom_histogram()+
  ggtitle("Gestation")

plot2 <- mammals %>% 
  ggplot(aes(body_wt))+
  geom_histogram()+
  ggtitle("Body weight")

plot1+plot2

plot3 <- mammals %>% 
  ggplot(aes(log10(body_wt)))+
  geom_histogram()

plot4 <- mammals %>% 
  ggplot(aes(x=gestation, y=log10(body_wt)))+
  geom_point()

(plot2+p)/(plot3+plot4)

### continuing now with log10 transforming...

log10_model <- lm(log10(body_wt)~gestation, data=mammals)
summary(log10_model)

augment_log10_model <- broom::augment(log10_model, interval="confidence")

plot5 <- 
  augment_log10_model %>% 
  ggplot(aes(x = .std.resid)) +
  geom_histogram()+
  ggtitle("")

plot6 <- 
  augment_log10_model %>%
  ggplot(aes(sample = .std.resid)) +
  qqplotr::stat_qq_band()+
  geom_qq()
### from the qqplotr package, allows the addition of a confidence 
### interval to qqplot line. can make it easier to see whether minor 
### deviations are acceptable

plot7 <- 
  augment_log10_model %>%
  ggplot(aes(x=.fitted, y= .std.resid)) +
  geom_point()+
  ggtitle("")

plot8 <- 
  augment_log10_model %>%
  ggplot(aes(x = as.numeric(rownames(augment_gestation)), y = .cooksd)) +
  geom_col() +
  geom_hline(yintercept = cook_limit,
             color = "red",
             linetype = "dashed")+
  ggtitle("")

plot9 <- 
  augment_log10_model %>% 
  ggplot(aes(x=gestation, y=`log10(body_wt)`))+
  geom_line(aes(x=gestation, y=.fitted))+
  geom_line(aes(x=gestation, y=.upper), linetype="dashed")+
  geom_line(aes(x=gestation, y=.lower), linetype="dashed")+
  geom_point()+
  ggtitle("")

((plot5+plot6)/(plot7+plot8))

### a normal distribution to residuals
### removed patterns in residuals against fitted, and greatly reduced
### heteroscedasticity (variance along the fit of regression line)
### still have two influential outliers but on a much reduced scale

par(mfrow=c(2,2))
plot(log10_model)
### ^^ is an alternative approach, and is fine

plot9+ggtitle("Linear trend")
praise()
praise::praise("${exclamation}! This is just ${adjective}!")

broom::tidy(log10_model, conf.int=T)
broom::glance(log10_model)

stargazer::stargazer(log10_model, type="html", ci=TRUE)

new_df <- data.frame(gestation=c(600,45,270))

prediction <- predict(log10_model, new_df)
prediction
10^prediction

my_cor <- cor.test(log10(mammals$body_wt), mammals$gestation)
my_cor
### strong positive correlation between body weight and gestation 
### width (r = 0.77, t56 = 8.96, P < 0.001).