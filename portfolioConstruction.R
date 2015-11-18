# This project assumes that the stockPortolio package is installed
# If it is not installed, please enter install.packages(stockPortfolio) into the console first

library(stockPortfolio)

ticker <- c("JPM","WFC","C","GS","ADR","CVX","XOM","VLO","VZ","T","S","TMUS","AMGN","GILD","CELG","REGN","DAL","UAL","LUV","AAL","AAPL","MSFT","YHOO","AMZN","^GSPC")

ind <- c("Money Center Banks","Money Center Banks","Money Center Banks","Money Center Banks","Oil and Gas","Oil and Gas","Oil and Gas","Oil and Gas","Telecommunications and Wireless Services","Telecommunications and Wireless Services","Telecommunications and Wireless Services","Telecommunications and Wireless Services","Biotechnology","Biotechnology","Biotechnology","Biotechnology","Airlines","Airlines","Airlines","Airlines","Software Applications and Services","Software Applications and Services","Software Applications and Services","Software Applications and Services","Market Index")

gr <- getReturns(ticker, start='2007-12-31', end='2012-12-31')
ret <- as.data.frame(gr$R)
return <- as.matrix(ret)

# PART A

# 1

markowitz_model <- stockModel(gr, model="none", drop=25, Rf=0.001)
portPossCurve(markowitz_model, riskRange = 10, xlim=c(0,0.20), ylim=c(-0.03,0.10))
portCloud(markowitz_model, add=TRUE)

op <- optimalPort(markowitz_model)
print(op)
cat("\n")

points(op$risk, op$R)

Rf <- 0.001
segments(0, Rf, op$risk, op$R)
slope <- (op$R-Rf)/op$risk
segments(0, Rf, 1.4*op$risk, Rf+slope*1.4*op$risk)
text(op$risk,op$R+0.005,"OPT")

# 2

portPossCurve(markowitz_model, riskRange = 10, effFrontier = TRUE, xlim=c(0,0.20), ylim=c(-0.03,0.10))
portCloud(markowitz_model, add=TRUE)
points(op$risk, op$R)
text(op$risk,op$R+0.005,"OPT")

# 3

ret <- return[,1:24]

EQ <- c(rep.int(1/24,24))

R_equal <- t(EQ) %*% colMeans(ret)
var_equal <- t(EQ) %*% cov(ret) %*% EQ
sd_equal <- sqrt(var_equal)

cat("Equal Allocation Portfolio Expected Return:", R_equal, "\n")
cat("Equal Allocation Portfolio Risk:", sd_equal, "\n")
cat("\n")

# 4

# a

sim <- stockModel(return, model="SIM", shortSelling="n", Rf=0.001, index=25)
simVas <- adjustBeta(sim, method='Vasicek') # adjust betas via Vasiceck technique
opVas <- optimalPort(simVas)

print(opVas)
cat("\n")

# b

alphas <- rep(0,24)
betas <- rep(0,24)

rettemp <- as.data.frame(return)

for(i in 1:24){
    alphas[i] <- lm(data=rettemp,formula=rettemp[,i] ~ rettemp[,25])$coefficients[1]
    betas[i] <- lm(data=rettemp,formula=rettemp[,i] ~ rettemp[,25])$coefficients[2]
}

betap <- t(opVas$X)%*%betas
alphap <- t(opVas$X)%*%alphas

cat("Alpha of opVas:", alphap[1], "\n")
cat("Beta of opVas:", betap[1], "\n")
cat("\n")

# c

sim2 <- stockModel(return, model="SIM", shortSelling="y", Rf=0.001, index=25)
simVas2 <- adjustBeta(sim2, method='Vasicek') # adjust betas via Vasiceck technique
opVas2 <- optimalPort(simVas2)

print(opVas2)
cat("\n")

betap2 <- t(opVas2$X)%*%betas
alphap2 <- t(opVas2$X)%*%alphas

cat("Alpha of opVas2:", alphap2[1], "\n")
cat("Beta of opVas2:", betap2[1], "\n")
cat("\n")

# 5

# a

ccm <- stockModel(gr, model="CCM", drop=25, shortSelling="n", Rf=0.001)
opCCM <- optimalPort(ccm)

print(opCCM)
cat("\n")

# b

ccm2 <- stockModel(gr, model="CCM", drop=25, shortSelling="y", Rf=0.001)
opCCM2 <- optimalPort(ccm2)

print(opCCM2)
cat("\n")

# 6

mgm <- stockModel(gr, model="MGM", drop=25, Rf=0.001, shortSelling="y", industry=ind)
opMGM <- optimalPort(mgm)

print(opMGM)
cat("\n")

# 7

portPossCurve(markowitz_model, riskRange = 10, xlim=c(0,0.20), ylim=c(-0.03,0.10))

points(opMGM$risk,opMGM$R,col="purple")
text(opMGM$risk,opMGM$R+0.005,"MGM",col="purple")

points(op$risk,op$R,col="blue")
text(op$risk,op$R+0.005,"Markowitz",col="blue")

points(sd_equal,R_equal)
text(sd_equal,R_equal+0.005,"EQ")

points(opVas$risk,opVas$R,col="red")
text(opVas$risk,opVas$R+0.005,"SIM(NSS)",col="red")

points(opVas2$risk,opVas2$R,col="red")
text(opVas2$risk,opVas2$R+0.005,"SIM(SS)",col="red")

points(opCCM$risk,opCCM$R,col="green")
text(opCCM$risk,opCCM$R+0.005,"CCM(NSS)",col="green")

points(opCCM2$risk,opCCM2$R,col="green")
text(opCCM2$risk,opCCM2$R+0.005,"CCM(SS)",col="green")

points(mgm, pch=19, add=TRUE)


# PART B

# Get data for new period
performance <- getReturns(ticker, start='2012-01-31', end='2015-03-31')

# Test the porfolios
tperfmarkowitz <- testPort(performance,op)
tperfeq <- testPort(performance$R[,-25], X=rep(1,24)/24)
tperfsimnss <- testPort(performance,opVas)
tperfsimss <- testPort(performance,opVas2)
tperfccmnss <- testPort(performance,opCCM)
tperfccmss <- testPort(performance,opCCM2)
tperfmgm <- testPort(performance,opMGM)

# 50-50 Rf and SIM_NSS split
rft = rep(0.001,38) # 38 months in portfolio
simnsspart <- as.matrix(cbind(opVas$X))
retnew <- as.matrix(cbind(performance$R[,-25]))
ret5050 <- 0.5 * (retnew %*% simnsspart) + 0.5 * rft

# Plot them
plot(tperfmarkowitz, lty=1, col="black", xlab="Time (in months)", ylab="Portfolio Return", ylim=c(-1.0,7.0))
lines(tperfeq, lty=2, col="green")
lines(tperfsimnss, lty=3, col="blue")
lines(tperfsimss, lty=4, col="paleturquoise4")
lines(tperfccmnss, lty=5, col="purple")
lines(tperfccmss, lty=6, col="red")
lines(tperfmgm, lty=7, col="orange")
lines(cumprod(1+rev(ret5050)), col="midnightblue")

# Market (S&P500) performance for the same time period:
lines(cumprod(1+rev(performance$R[,25])), col="pink")

# Add Legend
legend('topleft', lty=1:8, c('MARKOWITZ', 'EQUAL', 'SIM_NSS', 'SIM_SS', 'CCM_NSS', 'CCM_SS', 'MGM', 'RF SIM_NSS SPLIT', 'S&P 500'), col=c("black", "green", "blue", "paleturquoise4", "purple", "red", "orange", "midnightblue", "pink"))

# Compute Average Returns

newReturns <- colMeans(tperfsimnss$returns) # this is the same regardless of portfolio

markowitzret <- t(op$X) %*% newReturns
eqret <- t(c(rep.int(1/24,24))) %*% newReturns
simnssret <- t(opVas$X) %*% newReturns
simssret <- t(opVas2$X) %*% newReturns
ccmnssret <- t(opCCM$X) %*% newReturns
ccmssret <- t(opCCM2$X) %*% newReturns
mgmret <- t(opMGM$X) %*% newReturns

cat("Average Markowitz Returns:",markowitzret[1],"\n")
cat("Average Equal Allocation Returns:",eqret[1],"\n")
cat("Average SIM_NSS Returns:",simnssret[1],"\n")
cat("Average SIM_SS Returns:",simssret[1],"\n")
cat("Average CCM_NSS Returns:",ccmnssret[1],"\n")
cat("Average CCM_SS Returns:",ccmssret[1],"\n")
cat("Average MGM Returns:",mgmret[1],"\n")
cat("Average 50-50 SIM_NSS and RF Returns:",(0.5*0.001)+(0.5*simnssret[1]),"\n")




