


# problem 1.c
pro <- choose(99,3)*choose(360,1)*choose(359,96)/ (360**99)

# problem 1.d
pro_1 <- 1 - choose(360,99)/(360**99)

# set directory & read data from file
setwd("C:/Users/jinji/Desktop/MF793File")
array <- read.csv("FF_Research_Data_Factors_weekly.csv")

return_array <- array[array[,1] >= 20110101 & array[,1] <= 20191231,]
r_i <- return_array[2:length(return_array[,1]), c(2)]
r_i_1 <- return_array[1:length(return_array[,1])-1, c(2)]
mat_return <- cbind(r_i_1, r_i)

# problem 2.c.1
a <- sum(mat_return[,1]<0 & mat_return[,2]<0)
b <- sum(mat_return[,1]<0 & mat_return[,2]>0)
c <- sum(mat_return[,1]>0 & mat_return[,2]<0)
d <- sum(mat_return[,1]>0 & mat_return[,2]>0)

# problem 2.c.2
# The sum of all four probabilities not equal 1 because exist one data = 0 at
# 20140516 in the time domain
sum <- length(mat_return[,1])
a_1 <- a/sum
b_1 <- b/sum
c_1 <- c/sum
d_1 <- d/sum

# problem 2.c.3
# The sum of first row not equal 1 because exist one data r(i-1)<0 r(i)=0
a_2 <- a/sum(mat_return[,1]<0)
b_2 <- b/sum(mat_return[,1]<0)
c_2 <- c/sum(mat_return[,1]>0)
d_2 <- d/sum(mat_return[,1]>0)


# problem 4.a
fund_array_raw <- read.csv("1500fund-montrets.csv")
market_array_raw <- read.csv("FF_Research_Data_Factors_monthly.csv")

fund_array <- fund_array_raw[fund_array_raw[,2]>20100531 & fund_array_raw[,2]<20140101,]
market_array <- market_array_raw[market_array_raw[,1]>201005 & market_array_raw[,1]<201401,]/100

total_array <- cbind(fund_array, market_array[,2])

mean_array_raw <- total_array[,3:dim(total_array)[2]]

mean_array <- colMeans(mean_array_raw[sapply(mean_array_raw, is.numeric)])

fund_mean_array <- mean_array[1:length(mean_array)-1]*12

hist(fund_mean_array, xlim = c(-0.3,0.3) , nclass = 100 , prob = T, xlab = "mean return", ylab = "number of mutual funds", main = "Histogram of mutual fund mean returns")


average_market <- mean_array[length(mean_array)]*12
average_funds <- mean(fund_mean_array)

rect(average_market, 0, average_market + 0.6/100, 1, col = 'Black', add = TRUE)
rect(average_funds, 0, average_funds + 0.6/100, 1, col = 'Blue', add = TRUE)


percen_beat_market <- sum(fund_mean_array > average_market)/length(fund_mean_array)



# problem 4.b
fund_array_2 <- fund_array_raw[fund_array_raw[,2]>20140531 & fund_array_raw[,2]<20170101,]
market_array_2 <- market_array_raw[market_array_raw[,1]>201405 & market_array_raw[,1]<201701,]/100

total_array_2 <- cbind(fund_array_2, market_array_2[,2])

mean_array_raw_2 <- total_array_2[,3:dim(total_array_2)[2]]

mean_array_2 <- colMeans(mean_array_raw_2[sapply(mean_array_raw_2, is.numeric)])

fund_mean_array_2 <- mean_array_2[1:length(mean_array_2)-1]*12

average_market_2 <- mean_array_2[length(mean_array_2)]*12

# plot in chart
x_1 <- 1:length(fund_mean_array)
x_2 <- 1:length(fund_mean_array_2)

plot(x_1, fund_mean_array, xlab = "Fund index", ylab = "average return", title("R1"))
points(length(x_1)+1, average_market, col ='red', pch = '*')
abline(h=average_market, col = 'Blue')
abline(v=length(x_1)+1, col = 'Blue')



plot(x_2, fund_mean_array_2, xlab = "Fund index", ylab = "average return", title("R2"))
points(length(x_2)+1, average_market_2, col ='red', pch = '*')
abline(h=average_market_2, col = 'Blue')
abline(v=length(x_2)+1, col = 'Blue')

sort_fund_mean_array <- sort(fund_mean_array)
L_1 <- sort_fund_mean_array[0.2 * length(sort_fund_mean_array)]
M_1 <- sort_fund_mean_array[0.8 * length(sort_fund_mean_array)]

sort_fund_mean_array_2 <- sort(fund_mean_array_2)
L_2 <- sort_fund_mean_array_2[0.2 * length(sort_fund_mean_array_2)]
M_2 <- sort_fund_mean_array_2[0.8 * length(sort_fund_mean_array_2)]

fund_mean_both <- rbind(fund_mean_array, fund_mean_array_2)
# row 1
sum(fund_mean_both[1,]<=L_1 & fund_mean_both[2,]<=L_2)
sum(fund_mean_both[1,]<=L_1 & fund_mean_both[2,]>L_2 & fund_mean_both[2,]<=M_2)
sum(fund_mean_both[1,]<=L_1 & fund_mean_both[2,]>M_2)
# row 2
sum(fund_mean_both[1,]>L_1 & fund_mean_both[1,]<=M_1 & fund_mean_both[2,]<=L_2)
sum(fund_mean_both[1,]>L_1 & fund_mean_both[1,]<=M_1 & fund_mean_both[2,]>L_2 & fund_mean_both[2,]<=M_2)
sum(fund_mean_both[1,]>L_1 & fund_mean_both[1,]<=M_1 & fund_mean_both[2,]>M_2)
# row 3
sum(fund_mean_both[1,]>M_1 & fund_mean_both[2,]<=L_2)
sum(fund_mean_both[1,]>M_1 & fund_mean_both[2,]>L_2 & fund_mean_both[2,]<=M_2)
sum(fund_mean_both[1,]>M_1 & fund_mean_both[2,]>M_2)


# problem 4.c
fund_mean_mkt_premium <- fund_mean_array - average_market
fund_mean_mkt_premium_2 <- fund_mean_array_2 - average_market_2

fund_mean_mkt_premium_both <- rbind(fund_mean_mkt_premium, fund_mean_mkt_premium_2)
# first row
sum(fund_mean_mkt_premium_both[1,]<0 & fund_mean_mkt_premium_both[2,] <0)
sum(fund_mean_mkt_premium_both[1,]<0 & fund_mean_mkt_premium_both[2,] >0)
# second row
sum(fund_mean_mkt_premium_both[1,]>0 & fund_mean_mkt_premium_both[2,] <0)
sum(fund_mean_mkt_premium_both[1,]>0 & fund_mean_mkt_premium_both[2,] >0)










