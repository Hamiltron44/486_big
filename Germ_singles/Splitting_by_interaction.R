dts_full<-data.frame(response=germn,watrlvl=c(rep(c(1:6),8)),cvrd=as.factor(rep(c(0,1),c(24,24))))

dts_red<-dts_full[dts_full[,2]!=6,]
dts_red$watrlvl=as.factor(c(rep(c(1:5),8)))
tens<-with(dts_red, C(watrlvl, poly, 2))

dts_c <- lm(response ~ cvrd + tens + watrlvl+ tens:cvrd + watrlvl:cvrd, data = dts_red)
anova(dts_c)

covrd<-dts_red[1:20,]
uncovrd<-dts_red[21:40,]
tens_covrd<-with(covrd, C(watrlvl, poly, 2))
tens_uncovrd<-with(uncovrd, C(watrlvl, poly, 2))

dts_covrd <- lm(response ~ tens_covrd + watrlvl, data = covrd)
anova(dts_covrd)

dts_uncovrd <- lm(response ~ tens_uncovrd + watrlvl, data = uncovrd)
anova(dts_uncovrd)
