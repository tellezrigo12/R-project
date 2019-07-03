install.packages("metafor")
###performing a meta analysis by adding .5 to each observation ###
library('metafor')
data <- read.csv("http://www.stat.usu.edu/jrstevens/biostat/data/rosiglitazone.csv")
RosiMI <- data[,3]
RosiNoMI <- data[,2] - data[,3]
CntrlMI <- data[,6]
CntrlNoMI <- data[,5] - data[,6]
results <- rma.uni(ai = RosiMI, bi = RosiNoMI, ci = CntrlMI, di = CntrlNoMI, 
                   measure = 'OR', add = .5, to = 'all', method = 'DL', 
                   slab = 1:42, data = data)
forest(results)
summary(results)
funnel(results)
qqnorm(results)
z <- results$yi/sqrt(results$vi + results$tau2)
x <- 1/sqrt(results$vi + results$tau2)
fit <- lm(z~x)
summary(fit)$coeff
###performing a meta analysis using a Peto Method##
results2 <- rma.uni(ai = RosiMI, bi = RosiNoMI, ci = CntrlMI, di = CntrlNoMI, 
                    measure = 'PETO', add = 0, to = 'all', method = 'DL', 
                    slab = 1:42, data = data)
forest(results2)
summary(results2)
funnel(results2)
qqnorm(results2)
z2 <- results2$yi/sqrt(results2$vi + results2$tau2)
x2 <- 1/sqrt(results2$vi + results2$tau2)
fit <- lm(z2~x2)
summary(fit)$coeff
###changing the data and noticing how that affects our inference ###
ids.drop <- c("49653/127","49653/128","49653/136","49653/143","49653/145",
              "49653/147","49653/162","49653/284","SB-712753/002","SB-712753/003")
data6 <- data[!is.element(data$StudyID,ids.drop),]
RosiMI6 <- data6[,3]
RosiNoMI6 <- data6[,2] - data6[,3]
CntrlMI6 <- data6[,6]
CntrlNoMI6 <- data6[,5] - data6[,6]
results3 <- rma.uni(ai = RosiMI6, bi = RosiNoMI6, ci = CntrlMI6, di = CntrlNoMI6, 
                   measure = 'PETO', add = 0, to = 'all', method = 'DL', 
                   slab = 1:32, data = data6)
summary(results3)

