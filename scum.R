library(tidyverse)
# SCUM scores available at https://www.killjamesbond.com/scum
scum <- tibble(
  id = 1:33,
  smarm=
    c(5,3,7,7,5,2,4,7,5,5,6,2,4,4,1,3,2,4,7,
      7,7,1,2,2,3,4,5,1,1,2,1,3,1),
  cultural_insensitivity=
    c(7,5,2,3,8,1,6,9,6,3,1,1,8,5,1,5,5,2,3,
      2,5,8,3,2,4,2,5,2,3,3,3,2,4),
  unprovoked_violence=
    c(2,2,6,2,2,1,5,5,4,6,4,4,4,1,2,4,9,6,1,
      4,5,3,5,6,1,2,4,2,4,4,2,0,0),
  misogyny=
    c(3,6,8,7,7,4,3,7,7,4,6,1,7,6,3,5,5,6,3,
      7,5,6,5,5,6,6,7,3,2,4,3,4,5)
)

movie_actor <- tibble(
  id = 1:33,
  name = c("Dr. No","From Russia with Love","Goldfinger","Thunderball",
           "You Only Live Twice","On Her Majesty's Secret Service","Diamonds are Forever",
           "Live and Let Die","The Man with the Golden Gun","The Spy who Loved Me","Moonraker",
           "For Your Eyes Only","Octopussy","Never Say Never Again","A View to a Kill",
           "The Living Daylights","License to Kill","Goldeneye","Tomorrow Never Dies",
           "The World is Not Enough","Die Another Day","Casino Royale","Casino Royale",
           "Quantum of Solace","Skyfall","Spectre","No Time to Die",
           "The Bourne Ultimatum", "The Bourne Legacy", "Jason Bourne",
           "Syriana", "Breach","Enemy of the State"),
  actor = c(rep("Sean Connery",5),"George Lazenby","Sean Connery",
            rep("Roger Moore",6), "Sean Connery","Roger Moore",
            rep("Timothy Dalton", 2), rep("Pierce Brosnan",4),"Peter Sellers et al.",
            rep("Daniel Craig",5), "Matt Damon", "Jeremy Renner", "Matt Damon",
            "George Clooney, Matt Damon, Jeffrey Wright", "Chris Cooper, Ryan Phillippe, Laura Linney",
            "Will Smith, Gene Hackman"),
  year = c(1962,1963,1964,1965,1967,1969,1971,1973,1974,1977,1979,
           1981,1983,1983,1985,1987,1989,1995,1997,1999,2002,1967,
           2006,2008,2012,2015,2021,2007,2012,2016,2005,2007,1998)
) %>% 
  mutate(actor = as_factor(actor))


library(psych)

psych::fa.parallel(scum[2:5])
fa <- fa(scum[2:5], nfactors = 1)
summary(fa)
fa


library(mirt)

mod <- mirt(scum[2:5],
            itemtype = "graded",
            model = 1, 
            SE = TRUE)

params <- coef(mod, IRTpars = TRUE, simplify = TRUE)

params

plot(mod, type = "trace", which.items = 1:4)

plot(mod, type = "infotrace", which.items = 1:4)

plot(mod, type = "infoSE")

itemfit(mod)
itemfit(mod, empirical.plot = 1)
itemfit(mod, empirical.plot = 2)
itemfit(mod, empirical.plot = 3)
itemfit(mod, empirical.plot = 4)

itemfit(mod, empirical.table = 1)


personfit <- personfit(mod, stats.only = FALSE)
hist(personfit$Zh)
abline(v=-2, lwd=2, lty=2) #lty is line type; lwd is line width
misfit <- personfit$Zh < -2
sum(misfit)
round(sum(misfit)/nrow(personfit), 3)
describe(personfit$Zh)
personfit$misfit <- personfit$Zh < -2
describeBy(personfit, group=personfit$misfit)
M2(mod, type="C2")


head(fscores(mod, full.scores=FALSE) )
thetaScores <- fscores(mod)
describe(thetaScores)
hist(thetaScores)

final <- cbind(scum,thetaScores)

final <- left_join(final, movie_actor, by = ("id" = "id"))
final %>% group_by(actor) %>% summarize(mean_score = mean(F1))

final$sum <- rowSums(final[2:5])
cor.test(final$F1,final$sum)

library(hrbrthemes)
library(viridis)
ggplot(data = final, mapping = aes(x = rank(year, ties.method = "first"), y = F1, fill = actor)) +
         geom_bar(stat="identity") +
  coord_flip() +
  geom_text(aes(y = 0,label=name), color="black", size=3.5)+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Deviations from Average SCUM-miness") +
  theme_ipsum() +
  ylab("Estimated SCUM-miness Score") + 
  xlab("Release Order")


plot.data <- final %>% filter(actor != "Peter Sellers et al." & actor != "George Lazenby")
ggplot(data = plot.data, mapping = aes(x = actor, y = F1, density = F1, group = actor, fill = actor)) +
  geom_violin(scale = "width", trim = FALSE, adjust = .5, draw_quantiles = c(.25,.5,.75),)+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("SCUM-miness of the Bonds") +
  theme_ipsum() +
  ylab("Estimated SCUM-miness Score")

# We can use this idea to plot the "reliability" of our test over values of theta. 
info <- testinfo(mod, thetaScores)
rxx <- 1 - (1/info)
plot(thetaScores, rxx)
abline(h=.75)


# An overall estimates of "reliability" can be computed. It is printed with the
# fscores function, known as the empirical reliability. 
head(fscores(mod, full.scores=FALSE))

# The empirical reliability is computed as follows using the CTT definition of
# reliability: rxx = true score variance / (true score variance + error variance)
scores <- fscores(mod, full.scores=T, scores.only=T, full.scores.SE=T)
e <- mean(scores[, 2]^2) #estimate of error variance from standard error estimates of theta
t <- var(scores[, 1]) #variance in theta scores as an estimate of true score variance (since thetas are estimates of true ability)
(t)/(t+e)
# or equivalently for rxx = 1 - (error var / obs var), in which obs var x = T+E
1-(e / (t + e))

# Estimate of marginal reliability from (deAyala, 2013)
1-e
marginal_rxx(mod)

# You can obtain extra fit statistics using EAPsum method in fscores
scores <- fscores(mod, method="EAPsum", full.scores=F)

# You can check local dependence using the model residuals.
residuals(mod)
residuals(mod, type="LDG2")


## Other models
kjbmod <- "F = smarm-misogyny
            CONSTRAIN = (smarm-misogyny, a1)"

(kjb.fit1PL <- mirt(data=scum[2:5], model=kjbmod, itemtype="graded", SE=T))
(kjb.params1PL <- coef(kjb.fit1PL, IRTpars=T, simplify=T))

# Performing a LRT comparing the GRM and 1PL GRM
anova(mod, kjb.fit1PL) #freely estimating the discrimination parameters improves model fit
itemfit(kjb.fit1PL)
itemfit(kjb.fit1PL, empirical.plot = 1)
itemfit(kjb.fit1PL, empirical.plot = 2)
itemfit(kjb.fit1PL, empirical.plot = 3)
itemfit(kjb.fit1PL, empirical.plot = 4)

itemfit(kjb.fit1PL, empirical.table = 1)


# Two other alternative polytomous IRT models include the Rasch Partial Credit
# Model and the Generalized Partial Credit Model. Below is how you would
# estimate those in mirt.

(kjb.fitPCM <- mirt(data=scum[2:5], model=1, itemtype="Rasch", SE=T)) #Partial Credit Model
(kjb.paramsPCM <- coef(kjb.fitPCM, IRTpars=T, simplify=T))

(kjb.fitGPCM <- mirt(data=scum[2:5], model=1, itemtype="gpcm", SE=T)) #Generalized Partial Credit Model
(kjb.paramsGPCM <- coef(kjb.fitGPCM, IRTpars=T, simplify=T))
anova(mod, kjb.fitGPCM)

library(tidyverse)
library(scales)

## Rescale
scum2 <- scum %>% 
  mutate(
    id = id,
    smarm = smarm,
    cultural_insensitivity = round(rescale(cultural_insensitivity, to = c(1,7)),0),
    unprovoked_violence = round(rescale(unprovoked_violence, to = c(1,7)),0),
    misogyny = round(rescale(misogyny, to = c(1,7)),0)
  )


# GRM

mod2 <- mirt(scum2[2:5],
            itemtype = "graded",
            model = 1, 
            SE = TRUE)

params2 <- coef(mod2, IRTpars = TRUE, simplify = TRUE)

params2
anova(mod,mod2)
plot(mod2, type = "trace", which.items = 1:4)

plot(mod2, type = "infotrace", which.items = 1:4)

plot(mod2, type = "infoSE")

itemfit(mod2)
itemfit(mod2, empirical.plot = 1)
itemfit(mod2, empirical.plot = 2)
itemfit(mod2, empirical.plot = 3)
itemfit(mod2, empirical.plot = 4)

itemfit(mod2, empirical.table = 1)

