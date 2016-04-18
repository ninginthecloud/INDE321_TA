####################################
# Hypothesis test & Control Chart 
#     April, 18th, 2016
####################################

######################
#  hypothesis testing
######################
#test mean
# one sample t test
x <- c(0.593, 0.142, 0.329, 0.691, 0.231, 0.793, 0.519, 0.392, 0.418)
boxplot(x)
t.test(x, alternative = "greater", mu = 0.3, conf.level = 0.95)
t.test(x, alternative = "two.sided", mu = 0.3, conf.level = 0.95)

# two samples t tests
x <- c(91, 87, 99, 77, 88, 91)     # treatment group
y <- c(101, 110, 103, 93, 99, 104) # control group
boxplot(x,y)
t.test(x, y, alternative = "less", var.equal = TRUE) # variance are equal
t.test(x, y, alternative = "less", var.equal = FALSE) # variance are inequal

# paired t test
x <- c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28) # regular
y <- c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32) # premium
t.test(x,y, alternative = "less")
t.test(x, y, alternative = "less", paired = TRUE)

#test variance
# one sample chi-squre test
x <- c(0.593, 0.142, 0.329, 0.691, 0.231, 0.793, 0.519, 0.392, 0.418)


# two sample f-test
x <- c(91, 87, 99, 77, 88, 91)     # treatment group
y <- c(101, 110, 103, 93, 99, 104) # control group
var.test(x,y, alternative = "two.sided")
##################################
# Control Chart(x bar & R chart)
##################################
library(qcc)
## x bar & R chart
data(pistonrings)
x <- qcc.groups(pistonrings$diameter, pistonrings$sample)
x
qcc(x, type = "xbar")

# problem from slides
x <- c(34.5, 34.2, 31.6, 31.5, 35, 34.1, 32.6, 33.8, 34.8, 33.6, 31.9, 38.6, 35.4, 
        34, 37.1, 34.9, 33.5, 31.7, 34, 35.1, 33.7, 32.8, 33.5, 34.2)
R <- c(3, 4, 4, 4, 5, 6, 4, 3, 7, 8, 3, 9, 8, 6, 5, 7, 4, 3, 8, 4, 2, 1, 3, 2)
n <- 5
m <- 24

# for x bar chart
R_bar <- mean(R)
d2 <- 2.326 # for n = 5, 
x.sd <- R_bar/d2

# visualize
obj_x <- qcc(x, sizes = n, std.dev = x.sd, type = "xbar")
# summary
summary(obj_x)

# for R chart
D3 <- 0
D4 <- 2.114
LCL <- D3 * R_bar
UCL <- D4 * R_bar

obj_R <- qcc(R, sizes = n, type = "R", limits = c(LCL, UCL))
