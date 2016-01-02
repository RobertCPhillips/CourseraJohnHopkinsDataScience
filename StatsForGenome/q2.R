#quiz 2
source("https://bioconductor.org/biocLite.R")
biocLite("limma")
biocLite("sva")
require(limma)
require(sva)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/montpick_eset.RData")
load(file=con)
close(con)
mp = montpick.eset
pdata_mp = pData(mp)
edata_mp = as.data.frame(exprs(mp))
fdata_mp = fData(mp)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)
bm = bodymap.eset
edata_bm = exprs(bm)
pdata_bm = pData(bm)

#q1 -------------------------------
pc.a = prcomp(edata_mp)

edata_mp.b <- log2(edata_mp+1)
pc.b = prcomp(edata_mp.b)

edata_mp.c = edata_mp.b - rowMeans(edata_mp.b)
pc.c = prcomp(edata_mp.c)

#summary(pc.a)
#summary(pc.b)
#summary(pc.c)

pc.a.v <- pc.a$sdev[1]^2 / sum(pc.a$sdev^2)
pc.b.v <- pc.b$sdev[1]^2 / sum(pc.b$sdev^2)
pc.c.v <- pc.c$sdev[1]^2 / sum(pc.c$sdev^2)


#q2 -------------------------------
set.seed(333)
km <- kmeans(t(edata_mp.c),2)
svd1 = svd(edata_mp.c)

cor(svd1$v[,1], km$cluster)


#q3 -------------------------------
table(pdata_bm$num.tech.reps)
fit1 <- lm(edata_bm[1,] ~ as.factor(pdata_bm$num.tech.reps))
summary(fit1)
points(edata_bm[1,] ~ as.factor(pdata_bm$num.tech.reps))

#q4 -------------------------------
fit2 <- lm(edata_bm[1,] ~ pdata_bm$age + pdata_bm$gender)
summary(fit2)

#q5, q6 -------------------------------
fit3 <- lm.fit(model.matrix(~pdata_mp$population),t(edata_mp.b))

#q7, q8 -------------------------------
age.notNa <- !is.na(pdata_bm$age)
pdata_bm.age <- pdata_bm$age[age.notNa]
edata_bm.age <- edata_bm[,age.notNa]
mod4 = model.matrix(~ pdata_bm.age)

fit4 = lmFit(edata_bm.age, mod4)
fit4$coefficients[1000,]
plot(edata_bm.age[1000,])
abline(fit4$coefficients[1000,])

pdata_bm.agett <- pdata_bm$tissue.type[age.notNa]
mod5 = model.matrix(~ pdata_bm.age + pdata_bm.agett)
fit5 = lmFit(edata_bm.age, mod5)

fit5$coefficients[1000,]

#q10 ----------------------------------------
edata_bm.log <- log2(edata_bm.age + 1)
edata_bm.log = edata_bm.log[rowMeans(edata_bm.log) > 1, ]

set.seed(33353)
sva1 <- sva(edata_bm.log, mod4)
cor(sva1$sv[,1], pdata_bm.age)
cor(sva1$sv[,2], pdata_bm.age)
cor(sva1$sv[,3], pdata_bm.age)
cor(sva1$sv[,4], pdata_bm.age)
