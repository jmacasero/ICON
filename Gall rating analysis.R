nema.sum <- read.csv("nemaWS2012.csv")

nema.sum$block <- factor(nema.sum$block)
nema.sum$field <- factor(nema.sum$field)
nema.sum$field123 <- factor(nema.sum$field123)

### Using rank-based method ###

library(nlme)

# Result without ranking
lme.nema <- lme(fixed = GR ~ MP*SP, random = ~1|block, data=nema.sum)
anova(lme.nema)

# Result with ranking
rGRsum <- rank(nema.sum$GR)
lme.nema.sum <- lme(fixed = rGRsum ~ MP*SP, random = ~1|block, data=nema.sum)
summary(lme.nema.sum)
anova(lme.nema.sum)

### Ordered Logistic Regression model ###
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape)

# fitting the model
GR.factor <- factor(nema.sum$GR)

m <- polr(GR.factor ~ MP*SP, data=nema.sum, Hess=TRUE)
summary(m)

# computing the p value
# p values lower that 0.05, reject the null hypothesis
(ctable <- coef(summary(m)))

p <- pnorm(abs(ctable[, "t value"]), lower.tail=FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Confidence interval
(ci <- confint.default(m))

# Odds ratio
exp(cbind(OR= coef(m), ci))

### Using lme4 package ###
## lme4 package uses almost the same as PROC MIXED of SAS

library(lme4)

## fixed variable Water, Nfert, and block/rep
## random variable field

fit.lmer2 <- lmer(GR ~ block + MP*SP + (1|field), data=nema.sum)
summary(fit.lmer2)
anova(fit.lmer2)

## Getting the p value for each treatment effects
# Main plot effect
pf(1.2564, df1=2, df2=4, lower.tail=FALSE)

# Subplot effect
pf(0.2899, df1=1, df2=6, lower.tail=FALSE)

# Interaction effect
pf(0.2899, df1=2, df2=6, lower.tail=FALSE)

## block as random effect
fit.lmer3 <- lmer(GR ~ MP*SP + (1|block/field), data=nema.sum)
summary(fit.lmer3)
anova(fit.lmer3)


# checking with AOV
fit.aov2 <- aov(GR ~ MP*SP + block + Error(block/MP), data=nema.sum)
summary(fit.aov2)


### Ordinary Least Squares model ###

x <- as.matrix(cbind(int=1, nema.sum$MP, nema.sum$SP))
y <- as.vector(nema.sum$GR)

ols.model1 <- lm(GR ~ MP * SP, data=nema.sum)
summary(ols.model1)
anova(ols.model1)