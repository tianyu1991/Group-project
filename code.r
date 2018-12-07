ks.projects.201801 <- read.csv("C:/Users/tliu/Downloads/ks-projects-201801.csv/ks-projects-201801.csv")
projects <- ks.projects.201801

library(dplyr)
library(ggplot2)

projects2 <- filter(projects, state != "undefined" & !grepl(",",country))

## data clean and exploratory
ggplot(projects2, aes(usd_pledged_real))+xlim(c(0,20000))+ ylim(c(0,50000)) + geom_histogram()+
  labs(title = "Distribution of USD Pledged")


ggplot(projects2, aes(log(usd_pledged_real+1))) + geom_histogram()+
  labs(title = "Distribu
       tion of Log(USD Pledged+1)")

projects3 <- filter(projects2, usd_pledged_real > 1)
projects3 <- filter(projects3, backers!=0)

ggplot(projects3, aes(log(usd_pledged_real))) + geom_histogram()+
  labs(title = "Distribution of Log(USD Pledged)")

ggplot(projects3, aes(backers)) + geom_histogram()+xlim(c(0,11))


ggplot(projects3, aes(log(usd_goal_real), log(usd_pledged_real))) +geom_point(alpha=0.01)
ggplot(projects3, aes(log(backers), log(usd_pledged_real)))  +geom_point(alpha=0.01)


projects3$diff_time<-difftime(projects3$deadline,projects3$launched,units = "days")
ggplot(projects3, aes(diff_time, log(usd_pledged_real))) + geom_point(alpha=0.01) +labs(x = "Days Between Launched and Deadline")

projects3$log_pledged <-log(projects3$usd_pledged_real)
projects3$log_goal <-log(projects3$usd_goal_real)
projects3$log_backer <-log(projects3$backers)
projects3$log_diff_time <-log(as.numeric(projects3$diff_time))

##divided into train/test set
n=nrow(projects3)
t=sample(1:n,size=(0.9*n),replace=FALSE,prob=NULL)
train=projects3[t,]
test=projects3[-t,]


##model

m1 <- lm(log_pledged ~ log_goal + log_backer + main_category , data = train)




library(lattice)

xyplot(resid(m1) ~ fitted(m1), 
       xlab="Fitted Values", 
       ylab="Residuals", 
       main="Residual Diagnostic Plot", 
       panel=function(x, y, ...)
            {
                  panel.grid(h=-1, v=-1)
                  panel.abline(h = 0)
                  panel.xyplot(x, y, ...)
            }
 )

anova(m1)

## test group prediction

pre <-predict(m1, newdata = test,interval="confidence",level =0.95)
test_p <- cbind(pre, test$log_pledged)

within<- rep(0,nrow(test_p)) 
test_p <- cbind(test_p, within)

for(i in 1:nrow(test_p)){
  if(test_p[i,4] >= test_p[i,2] & test_p[i,4] <= test_p[i,3]){
    test_p[i,5] <- 1
  }
}
summary(test_p)

residual_square <- (test_p[,4]-test_p[,1])^2
RSS <- sum(residual_square)

mean_test <-mean(test_p[,1])
vari <- (test_p[,1]- mean_test)^2
TSS <-sum(vari)
