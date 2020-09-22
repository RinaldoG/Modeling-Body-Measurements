library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(lattice)
library(ggplot2)
df <- read_xlsx("bdims.xlsx")
knee_df <- cbind(df[,18],df[,25])
colnames(knee_df)[1]<-c("Knee_Diameter")
knee_df$sex <- ifelse(knee_df$sex==1,'M','F')
knee_df$sex <- factor(knee_df$sex,levels=c('F','M'))
levels(knee_df$sex)
head(knee_df)


knee_df %>% group_by(sex) %>% summarise(Min = min(Knee_Diameter,na.rm = TRUE),
                                        Q1 = quantile(Knee_Diameter,probs = .25,na.rm = TRUE),
                                        Median = median(Knee_Diameter, na.rm = TRUE),
                                        Q3 = quantile(Knee_Diameter,probs = .75,na.rm = TRUE),
                                        Max = max(Knee_Diameter,na.rm = TRUE),
                                        Mean = mean(Knee_Diameter, na.rm = TRUE),
                                        SD = sd(Knee_Diameter, na.rm = TRUE),
                                        IQR = quantile(Knee_Diameter,probs = .75,na.rm = TRUE)-quantile(Knee_Diameter,probs = .25,na.rm = TRUE),
                                        n = n(),
                                        Missing = sum(is.na(Knee_Diameter)))


knee_df %>% histogram(~Knee_Diameter | sex, data = .,layout=c(1,2),type = "density",
                      panel=function(x, ...){
  panel.histogram(x, ...)
  xn <- seq(min(x), max(x), length.out = 100)
  yn <- dnorm(xn, mean(x), sd(x))
  panel.lines(xn, yn, col = "red")
})
x <- seq(30, 50, length.out=100)
y <- with(knee_df, dnorm(x, mean(Knee_Diameter), sd(Knee_Diameter)))
lines(x, y, col = "red")


labels(knee_df$sex)

knee_df %>% histogram(~Knee_Diameter | sex, data = .,layout=c(1,2),type = "density",ylab = "Frequency",
                      panel=function(x, ...){
                        panel.histogram(x, ...)
                        xn <- seq(min(x), max(x), length.out = 100)
                        yn <- dnorm(xn, mean(x), sd(x))
                        panel.lines(xn, yn, col = "red")
                      })


help(dnorm)








