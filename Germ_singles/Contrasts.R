watrlvl_cp<-germ_final$watrlvl
cvrd_cp<-germ_final$cvrd

contrasts(cvrd_cp) <- contr.helmert
contrasts(watrlvl_cp) <- contr.poly
str(watrlvl_cp)

DTS<-lm(germ_final$response~watrlvl_cp*cvrd_cp)
anova(DTS)
summary(DTS)
str(watrlvl_cp)
