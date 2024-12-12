library(ggplot2)
library(data.table)
library(emmeans)
library(car)
library(dplyr)

#setwd
source('gg_interaction_function.R')

## Create the Germination Dataset
germn<-c(22,41,66,82,79,0,
         25,46,72,73,68,0,
         27,59,51,73,74,0,
         23,38,78,84,70,0,
         45,65,81,55,31,0,
         41,80,73,51,36,0,
         42,79,74,40,45,0,
         43,77,76,62,NA,0)

germ_full<-data.frame(response=germn,watrlvl=c(rep(c(1:6),8)),cvrd=rep(c(0,1),c(24,24)),smpl=rep(rep(1:4,rep(6,4)),2))

#Throw out level 6 because it was 0 for both levels of cvrd
germ_red<-germ_full[germ_full$watrlvl !=6,]
#Note, throwing out level 6 did not throw it out altogether. R thinks there's still 6 levels
str(germ_red)

#An atttemplt to the missing level 6
#germ_red<-dplyr::filter(.data = germ_full, watrlvl != 6) %>%
#  tibble::as_tibble(x = .)
#str(germ_red)

#throw away NA respon on row 40
germ_final<-germ_red[-40,]

str(germ_final)
germ_final$fac_watrlvl<-as.factor(germ_final$watrlvl)
germ_final$fac_cvrd<-as.factor(germ_final$cvrd)
germ_final$fac_smpl<-as.factor(germ_final$smpl)

## Transform the data to be binomial
#Get grouped sums for all 4 replicates all factor levels
germ_final %>%
  group_by(smpl,cvrd,watrlvl) %>%
  summarise_at(vars(response),
               list(name=sum)) -> germ_sums

#Transform response column count data into 0's and 1's for each seed
germ_raw <- data.frame(matrix(ncol=4,nrow=0))
for (i in 1:nrow(germ_sums)){
  x = data.frame(response=c(
    #Did germinate
    rep(1,germ_sums[i,4]),
    #Didn't germinate
    rep(0,(100-germ_sums[i,4]))),
    
    #Factors
    watrlvl=germ_sums[i,3],cvrd=germ_sums[i,2],smpl=germ_sums[i,1])
  germ_raw=rbind(germ_raw,x)
}

#Examine the new data set
head(germ_raw)
str(germ_raw)
nrow(germ_raw)

germ_raw$fac_watrlvl<-as.factor(germ_raw$watrlvl)
germ_raw$fac_cvrd<-as.factor(germ_raw$cvrd)
germ_raw$fac_smpl<-as.factor(germ_raw$smpl)

#Perform Logistic Regression (from Stat330)
#Logit transformation for non-linear to linear
#p(s) = 1 / 1 + e^-xBeta
#p(f) = e^-xBeta / 1 + e^-xBeta
#ln(Logit) = ln(P(s) / P(f)) = X*Beta
log_model1 <- glm(response ~ watrlvl*cvrd, data=germ_raw, family="binomial")
log_model2 <- glm(response ~ cvrd+watrlvl, data=germ_raw, family="binomial")

anova(log_model1)
summary(log_model1)

anova(log_model2)
summary(log_model2)

log_model1 <- glm(response ~ as.factor(watrlvl)*as.factor(cvrd), data=germ_raw, family="binomial")
log_model2 <- glm(response ~ as.factor(cvrd)+as.factor(watrlvl), data=germ_raw, family="binomial")

anova(log_model1)
summary(log_model1)

anova(log_model2)
summary(log_model2)


## Convert the data back to the original
#Remove na's if they exist
new_germ <- germ_raw[!is.na(germ_raw),]

raw_conv <- table(new_germ[,1],new_germ[,2],new_germ[,3])
as.data.frame(raw_conv)

gau_model1 <- glm(response ~ as.factor(watrlvl)*as.factor(cvrd), data=germ_raw, family="gaussian")
gau_model2 <- glm(response ~ as.factor(cvrd)+as.factor(watrlvl), data=germ_raw, family="gaussian")

anova(gau_model1)
summary(gau_model1)

anova(gau_model2)
summary(gau_model2)
germ_raw$fac_watrlvl<-as.factor(germ_raw$watrlvl)
germ_raw$fac_cvrd<-as.factor(germ_raw$cvrd)
str(germ_raw)



lm_model1 <- lm(response ~ fac_cvrd*fac_watrlvl,  data=germ_raw )
summary(lm_model1)
anova(lm_model1)
Anova(lm_model1, type='III')

lm_model2 <- lm(response ~ (fac_cvrd*fac_watrlvl)/fac_smpl,  data=germ_raw )
summary(lm_model2)
anova(lm_model2)
Anova(lm_model2, type='III')

