#quiz 3
source("https://bioconductor.org/biocLite.R")
biocLite("snpStats")
biocLite("broom")
require(snpStats)
require(broom)

data(for.exercise)
use <- seq(1, ncol(snps.10), 10)
sub.10 <- snps.10[,use]
snpdata = sub.10@.Data
status = subject.support$cc
table(status)
#summary(snps.10)
#summary(snp.support)
#summary(subject.support)


#q1,2 -----------------------------------------------
snp3 = as.numeric(snpdata[,3])
table(snp3)
snp3[snp3==0] = NA
table(snp3)
glm1 = glm(status ~ snp3,family="binomial")
tidy(glm1)
lm1 <- lm(status ~ snp3)
lm1$coefficients

#q3 -----------------------------------------------
snp10 = as.numeric(snpdata[,10])
table(snp10)
snp10[snp10==0] = NA
table(snp10)
snp10_rec <- (snp10 == 3)

glm10 <- glm(status ~ snp10,family="binomial")
glm10.p <- predict(glm10, type="response")

glm10_rec <- glm(status ~ snp10_rec,family="binomial")
glm10_rec.p <- predict(glm10_rec, type="response")

tidy(glm10_rec)
tidy(glm10)

status2 = status[!is.na(snp10)]
table(status2, glm10.p >= .5)
table(status2, glm10_rec.p >= .5)

#q4 -----------------------------------------------
glm_all = snp.rhs.tests(status ~ 1,snp.data=sub.10)
slotNames(glm_all)
