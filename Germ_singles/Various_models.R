lm_model2_sum <- lm(response ~ (fac_cvrd*fac_watrlvl)/fac_smpl,  data=germ_final )
summary(lm_model2_sum)
anova(lm_model2_sum)
Anova(lm_model2_sum, type='III')

lm_model2b_sum <- lm(response ~ (fac_cvrd*fac_watrlvl),  data=germ_final )
summary(lm_model2b_sum)
anova(lm_model2b_sum)
Anova(lm_model2b_sum, type='III')
germ_final$nogerm<-100-germ_final$response

log_model1_sum <- glm(cbind(response,nogerm) ~ fac_watrlvl*fac_cvrd, data=germ_final, family="binomial")
log_model2_sum <- glm(cbind(response,nogerm) ~ fac_cvrd+fac_watrlvl, data=germ_final, family="binomial")
log_model3_sum <- glm(cbind(response,nogerm) ~ (fac_watrlvl*fac_cvrd)/fac_smpl, data=germ_final, family="binomial")

anova(log_model1_sum)
summary(log_model1_sum)

anova(log_model2_sum)
summary(log_model2_sum)


anova(log_model3_sum)
summary(log_model3_sum)


log_model1_sum <- glm(cbind(response,nogerm) ~ fac_cvrd, data=germ_final, family="binomial")
