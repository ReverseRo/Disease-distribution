require(dplyr)
require(DescTools)
require(stringr)

#### load the data file ####
file<- str_c(getwd(), "/data9808.RData")
load(file)

# Linear trend
## Cochran-Armitage test
num_vi<- data9808[data9808$vision==1,] %>% group_by(year) %>% summarize(count=n())
num_nonvi<- data9808[data9808$vision==0,] %>% group_by(year) %>% summarize(count=n())
trend<-t(matrix(c(num_vi$count, num_nonvi$count), ncol=2, nrow=5, 
                dimnames=list(year=c(1998, 2000, 2002, 2005, 2008), 
                              outcome=c("Vision impairment", "Non vision impairment"))))
CochranArmitageTest(trend)

## Linear regression
prevalence<- num_vi$count/table(data9808$year)*100
year<- c(0, 2, 4, 7, 10)
linear<- lm(prevalence~year)
summary(linear)

log_linear<- lm(log(prevalence)~year)
summary(log_linear)

## Logistic regression
logistic<- glm(vision~year, data=data9808, family=binomial)
summary(logistic)

# Nonlinear regression
file<- str_c(getwd(), "/data9818.RData")
load(file)

## Polynomial regression
logistic_square<- glm(vision~year+I(year^2), data=data9818, family=binomial)
summary(logistic_square)
logistic_cubic<- glm(vision~year+I(year^2)+I(year^3), data=data9818, family=binomial)
summary(logistic_cubic)

## Spline regression
library(splines)
logistic_spline<-glm(vision~ns(year, 5),data=data9818, family="binomial")
summary(logistic_spline)

logistic_linear<- glm(vision~year, data=data9818, family=binomial)

#### plot ####
year<-c(0, 2, 4, 7, 10, 13, 16, 20)
plot<-data.frame(year=c(1998, 2000, 2002, 2005, 2008, 2011, 2014, 2018), 
                 pre_ob=c(31.7, 32.5, 42.7, 48.4, 49.0, 47.8, 48.3, 47.3),
                 pre_linear=predict(logistic_linear, data.frame(year), type="response")*100,
                 pre_square=predict(logistic_square, data.frame(year), type="response")*100,
                 pre_cubic=predict(logistic_cubic, data.frame(year), type="response")*100,
                 pre_spline=predict(logistic_spline, data.frame(year), type="response")*100)

tiff(filename="Predicted vision impairment prevalence trends.tiff", res=500, pointsize=12, 
     units="in", width=11.69, height=8.27)
opar <- par(no.readonly = TRUE)
par(mar = c(6, 6, 2, 2) + 0.1)
plot(x = plot$year, y = plot$pre_ob, type="b", pch=15, lwd=2,
     xlab = "", ylab = expression("Prevalence (%)"), main = "", 
     ylim= c(20, 60), 
     cex=1.5, cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
lines(x = plot$year, y = plot$pre_linear, type="b", pch=16, cex=1.5, lwd=2, col="red")
lines(x = plot$year, y = plot$pre_square, type="b", pch=17, cex=1.5, lwd=2, col="green")
lines(x = plot$year, y = plot$pre_cubic, type="b", pch=6, cex=1.5, lwd=2, col="blue")
lines(x = plot$year, y = plot$pre_spline, type="b", pch=18, cex=1.5, lwd=2, col="yellow")
legend("bottomright", inset=.05, cex=1.5,
       legend=c("Observed", "Logistic regression + linear term of year",
                "Logistic regression + square term of year",
                "Logistic regression + cubic term of year",
                "Logistic regression + spline of year"),
       lty=rep(1,5), pch=c(15, 16, 17, 6, 18), 
       col=c("black", "red", "green", "blue", "yellow"))
dev.off()

## joinpoint regression
num_vi<- data9818[data9818$vision==1,] %>% group_by(year) %>% summarize(count=n())
num_all<- data9818 %>% group_by(year) %>% summarize(count=n())
joinpoint<- data.frame(year=num_vi$year, num_vi=num_vi$count, num_all=num_all$count)
write.csv(joinpoint, file="Data for joinpoint regression.csv")
# save data and analyze in the Joinpoint software