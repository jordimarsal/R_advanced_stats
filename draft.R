#rm(v3)
#rm(df)

# PAC 4. R-SCRIPT. JORDI MARSAL.

oldwd = getwd() # d:/programfiles/rstudio
setwd("E:/UOC/UOC-MA01/5_ESTADISTICA/PAC4_ST")

dat <- read.csv("Wage.csv")

dim(dat)
names(dat)
head(dat)
str(dat)
#  SI hi ha nulls:
sapply(dat, function(x) (sum(is.na(x))))

library(reshape2)
library(ggplot2)

par(mfrow = c(1,1))
lapply(seq_along(dat), 
       FUN  = function(i) 
         boxplot(dat[,i] ~ dat$race, main = names(dat)[i])
)

boxplot(dat[,i] ~ dat$race, main = names(dat)[i])

bx2plot <- function(var, by, title){
  boxplot(var ~ by, main = title,xlab="", ylab = "",col = "skyblue")
}

bx2plot(dat$wage, dat$race, "Wage - Race")

hist(dat$wage, main = "wage",xlab = "", col = "skyblue")
qqnorm(dat$wage)
qqline(dat$wage)

library(moments)

agostino.test(dat$wage)

pTH <- function(res, alpha, title){
  writeLines(paste("H0:",title))
  if (res < alpha) writeLines(paste("Rebutjat H0. p-valor =",signif(res, digits = 3),"<",alpha))
  if (res >= alpha) writeLines(paste("No es rebutja H0. p-valor =",signif(res, digits = 3),">=",alpha))
}

osh = shapiro.test(dat$wage)

pTH(osh$p.value,0.05,paste("Distribució Normal. ",osh$method))

wage2 = dat$wage[dat$wage < 220]
hist(wage2, main = "wage",xlab = "", col = "skyblue")
osh = shapiro.test(wage2)
pTH(osh$p.value,0.05,paste("Distribució Normal. ",osh$method))
qqnorm(wage2)
qqline(wage2)

getSigma <- function(var){
  sigmap = sqrt(mean(var^2) - mean(var)^2)
  return(sigmap)
}
getInterval <- function(var){
  alpha2 = 0.025
  z.star = abs(qnorm(alpha2))
  writeLines(paste("z.star =",z.star))
  xm = mean(var)
  n<-length(var)
  sigmap = getSigma(var)
  imin = round(xm - (sigmap/n) * z.star,4)
  imax = round(xm + (sigmap/n) * z.star,4)
  return(paste("(",imin,",",imax,")", collapse = ""))
}

hist(dat$age, main = "Age",xlab = "", col = "skyblue")
osh = shapiro.test(dat$age)
pTH(osh$p.value,0.05,paste("Distribució Normal. ",osh$method))
qqnorm(dat$age)
qqline(dat$age)

with(beaver2, tapply(dat$age, dat$jobclass, getInterval))


w_ins = dat[c("wage")][(dat$health_ins == "1. Yes"),]
w_noins = dat$wage[dat$health_ins == "2. No"]

hist(w_ins, main = "ins",xlab = "", col = "skyblue")
hist(w_noins, main = "no_ins",xlab = "", col = "skyblue")

w_ins[1:20]
w_noins[1:20]
length(w_ins)
dat$health_ins[1:5]

### 4.1

wage_di = dat$wage
mw = mean(wage_di)

wage_di[wage_di < mw] <- 0
wage_di[wage_di >= mw] <- 1

levels(dat$jobclass)[1:3]

results = glm(wage_di ~ health_ins + jobclass + age, data = dat,family = "binomial"(link='logit'))
summary(results)

library(ggplot2)
ggplot(dat, aes(x = health_ins,
                y = age,
                color = wage_di)) + geom_point()

exp(coefficients(results))

# Exemple de predicció:
log.odds <- predict(results, data.frame(health_ins = "1. Yes",
                                        jobclass = "2. Information",
                                        age = 40))
log.odds
p = round(exp(log.odds)/(1+exp(log.odds)),3)
writeLines(paste("Probabilitat de sou superior a la mitja =",p))

rl_edu = glm(wage_di ~ health_ins + jobclass + age + education, data = dat,family = "binomial"(link='logit'))
summary(rl_edu)

exp(summary(rl_edu)$coefficients["jobclass2. Information",1] + 
      +     qnorm(c(0.025,0.5,0.975)) * summary(rl_edu)$coefficients["jobclass2. Information",2])

odds95 <- function(result, clazz){
  exp(summary(result)$coefficients[clazz,1] + 
        +     qnorm(c(0.025,0.5,0.975)) * summary(result)$coefficients[clazz,2])
  
}

exp(cbind("Odds ratio" = coef(rl_edu), confint.default(rl_edu, level = 0.95)))

# https://stats.stackexchange.com/q/304909

below = dat[dat$wage < 150,]
dim(below)[1]
below
levels(below$education)

library("ggpubr")
ggboxplot(below, x = "education", y = "wage", 
          color = "education", palette = c("#00AFBB", "#E7B800", "#FC4E07","#4B952B", "#522B95"),
          order = c("1. < HS Grad", "2. HS Grad", "3. Some College", "4. College Grad", "5. Advanced Degree"),
          x.text.angle = 45,
          font.legend = c(8, "plain", "black"),
          ylab = "Wage", xlab = "Edu")

SCT <- function(df, factor_){
  wages <- split(df$wage, factor_)
  m <- c(); l <- c();  v <- c()
  for (i in wages){
    l <- c(l,length(i))
    m <- c(m,mean(i))
    v <- c(v,var(i))
  }
  d_ = 0;  n_ = 0
  for (i in 1:length(l)){
    n_ = n_ + m[i] * l[i]
    d_ = d_ + l[i]
  }
  mean_global = n_ / d_
  scd = 0
  for (i in 1:length(l)){
    scd = scd + ((l[i]-1) * v[i])
  }
  writeLines(paste("SCD =",round(scd,0)))
  sce = 0
  for (i in 1:length(l)){
    sce = sce + (l[i] * (m[i]-mean_global)^2)
  }
  writeLines(paste("SCE =",round(sce,0)))
}

SCT(below,below$education)

res.aov <- aov(wage ~ education, data = below)
summary(res.aov)
par(mfrow = c(2,2))
plot(res.aov)

ben4 = below[below$race != "4. Other",]
levels(ben4$race)
ggboxplot(ben4, x = "race", y = "wage", 
          color = "race", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1. White", "2. Black", "3. Asian"),
          x.text.angle = 45,
          font.xtickslab = FALSE,
          font.legend = c(8, "plain", "black"),
          ylab = "Wage", xlab = "Race")

adjusted = below[c("wage","race","jobclass","education")]
ben4 = adjusted[adjusted$race != "4. Other",]

library(dplyr)

byRJE <- ben4 %>% 
    group_by(race,jobclass,education) %>% 
    summarise_at(vars(wage), funs(mean(., na.rm=TRUE)))


setattr(byRJE$race,"levels",c("1. White", "2. Black", "3. Asian"))
levels(byRJE$race)

dim(byRJE)
head(byRJE)


library(ggplot2)
ggplot(byRJE, aes(x = jobclass, y = wage, colour = race)) +
  geom_point() +
  facet_wrap( ~ race) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggplot(byRJE, aes(x = jobclass, y = wage, colour = race)) +
  geom_point() +
  facet_wrap( ~ education) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggplot(byRJE, aes(x = race, y = wage, colour = jobclass)) +
  geom_point() +
  facet_wrap( ~ education) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggplot(byRJE, aes(x = education, y = wage, colour = jobclass)) +
  geom_point() +
  facet_wrap( ~ race) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

library(expss)
install.packages("formattable")
library(formattable)

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

formattable(cro(byRJE$wage,byRJE$race))
formattable(byRJE)
formattable(byRJE$wage)

formattable(cro_cpct(byRJE$wage, list(byRJE$race, byRJE$jobclass, byRJE$education)))

byRJE %>% 
  tab_cells(wage) %>% 
  tab_cols(race, jobclass) %>% 
  tab_stat_cpct() %>% 
  tab_pivot()

formattable(t1)

library(knitr)
install.packages("kableExtra")
library(kableExtra)

byRJE %>%
  kable() %>%
  kable_styling()

t1 %>%
  kable() %>%
  kable_styling()

install.packages("arsenal")
library(arsenal)
table_one <- tableby(interaction(education,race) ~ ., data = byRJE)
summary(table_one, title = "Gapminder Data")

formattable(table_one)

table_one %>%
  kable() %>%
  kable_styling()

library(data.table)
