# To replicate plot.ci() functionality using ggplot2, because
# as of this writing (8/19/2013) plot.ci() is broken.

# The problem:

library(Zelig)
rm(list=ls(all=TRUE))
data(turnout)
z.out <- zelig(vote ~ race + educate + age + I(age^2) + income, model = "logit", data = turnout)
age.range <- 18:95
x.low <- setx(z.out, educate = 12, age = age.range)
x.high <- setx(z.out, educate = 16, age = age.range)
s.out <- sim(z.out, x = x.low, x1 = x.high)
xl <- "Age in Years"
yl <- "Expected Probability of Voting"
ma <- "Effect of Education and Age on Voting Behavior"
# The part that doesn't work:
# plot.ci(s.out, xlab = xl, ylab = yl, main = ma
# legend(45, 0.52, legend = c("College Education (16 years)", "High School Education (12 years)"), col = c("blue","red"), lty = c("solid"))

# For an idea of what may be inside s.out, do:
summary(s.out)

# The hack:

# 1. You do two rounds of simulations: one with 12
# years of ed as baseline, the other with 16:
getSims <- function() {
   x.12 <- setx(z.out, educate = 12, age = age.range)
   x.16 <- setx(z.out, educate = 16, age = age.range)
   s.12 <- sim(z.out,x=x.12,x1=x.16)
   s.16 <- sim(z.out,x=x.16,x1=x.12)
   # Will you need predicted or expected values? Just get both.
   # Also, ggplot will need a data frame, so you might as well 
   # package them now. First, collect them:
   collectSimValues <- function(zs) {
      rnames    <- names(summary(zs)[['stats']])
      expected  <- NULL
      predicted <- NULL
      for(i in rnames) {
         expected  <- rbind(expected,summary(zs)[['stats']][[i]][[1]])
         predicted <- rbind(predicted,summary(zs)[['stats']][[i]][[2]])
      }
      c <- cbind(expected,predicted)
      rownames(c) <- rnames
      return(c)
   }
   foo <- collectSimValues(s.12)
   bar <- collectSimValues(s.16)
   baz <- as.data.frame(rbind(foo,bar))
   baz$group <- rownames(baz)
   # Get education as a data frame column
   getEducate <- function(s) {
      return(as.numeric(sub(" educate=","",strsplit(s,split=',')[[1]][3])))
   }
   # Get age as a data frame column
   getAge <- function(s) {
      return(as.numeric(sub(" age=","",strsplit(s,split=',')[[1]][4])))
   }   
   baz$educate <- sapply(baz$group,getEducate)
   baz$age     <- sapply(baz$group,getAge)
   return(subset(baz,select=-group))
}
df <- getSims()

# Are we getting close?
library(ggplot2)
# Predicted P(Vote) graph
drawPPic <- function() {
   mydf <- subset(df,select=c('educate','age','1'))
   names(mydf) <- c('educate','age','pvote')
   mydf$educate <- factor(mydf$educate)   
   pic <- ggplot(data=mydf, aes(x=age, y=pvote, group=educate, color=educate)) + geom_line() + xlab(xl) + ylab(yl) + ggtitle(ma)
   return(pic)
}
# Expected P(Vote) graph
drawEPic <- function() {
   mydf <- subset(df,select=c('educate','age','mean'))
   mydf$educate <- factor(mydf$educate)
   pic <- ggplot(data=mydf, aes(x=age, y=mean, group=educate, color=educate)) + geom_line() + xlab(xl) + ylab(yl) + ggtitle(ma)
   return(pic)
}
ppic <- drawPPic()
epic <- drawEPic()
# Yes: we managed to plot the relationship
# between age and voting. Now, on to:

# 3. Draw CI plots.
# First, get the 95CI for the probability of voting
# by age, by education level, and make it long for
# ggplot to use (in Wickham-speak: make it tidy)
getCIData <- function() {
   p <- df[,c('2.5%','97.5%','educate','age')]
   names(p)[1:2] <- c('lower','upper')
   p$educate <- factor(p$educate)
   p.long <- reshape(p, direction="long", varying=c('lower','upper'), v.names="value", idvar=c("educate","age"), timevar="ci", times=c('lower','upper'))
   p.long.sorted <- p.long[order(p.long$educate,p.long$age,p.long$ci),]
   return(p.long.sorted)
}
# Next, draw the plot.
getCIplot <- function(df) {
   foo <- df
   foo$grouping <- df$age+as.numeric(df$educate)/10
   p <- ggplot(data=foo, aes(x=age, y=value, group=age, colour=educate)) + geom_line(aes(group = grouping)) + geom_point() + xlab(xl) + ylab(yl) + ggtitle(ma)
   # Set the colors by hand, change the legend
   p <- p + scale_color_manual(name  ="Education", 
                               breaks=c("12", "16"),
                               labels=c("High School (12 years)", "College (16 years)"),
                               values=c("red", "blue"))   
   # Position legend inside the graph
   p <- p + theme(legend.position=c(.75, .25))
   return(p)
}
plot.ci <- getCIplot(getCIData())