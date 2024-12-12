germ_final_tab
margin.table(germ_final_tab,c(1,3,4))

loglin(germ_final_tab,list(c(2),c(1,3,4)),fit=T)->AL
AL$pearson
AL$lrt
AL$fit
loglin(germ_final_tab,list(c(1,2),c(2,3),c(2,4),c(1,3,4)),fit=T)->AL
AL$pearson
AL$lrt
AL$fit
germ_final_tab[1,,,]
germ_final_tab[1,2,,]
germ_final_tab[1,,1,1]
germ_final_tab[1,,1,2]
germ_final_tab[1,,2,1]

loglin(germ_final_tab,list(c(2,1,3,4)),fit=T)->AL
AL$lrt
loglin(germ_final_tab,list(c(2,1,3),c(2,1,4),c(2,3,4),c(1,3,4)),fit=T)->AL
AL$lrt
AL$pearson
AL$df
1-pchisq(AL$lrt,AL$df)
(germ_final_tab - AL$fit)/sqrt(AL$fit)
qqnorm((germ_final_tab - AL$fit)/sqrt(AL$fit))
((germ_final_tab - AL$fit)/sqrt(AL$fit))^2
((germ_final_tab - AL$fit)/sqrt(AL$fit))^2 > 3.84
sum(((germ_final_tab - AL$fit)/sqrt(AL$fit))^2 >3.84)
germ_final_tab[,,5,1]
germ_final_tab[,,5,2]
germ_final_tab[4,,5,2]
germ_final_tab[4,1,5,2]
germ_final_tab[4,1,5,2]<-.5
germ_final_tab[4,2,5,2]<-.5
germ_final_tab[4,,5,2]
loglin(germ_final_tab,list(c(2,1,3),c(2,1,4),c(2,3,4),c(1,3,4)),fit=T)->AL
AL$lrt
1-pchisq(AL$lrt,AL$df)
germ_final_tab[4,1,5,2]<-0
germ_final_tab[4,2,5,2]<-0
loglin(germ_final_tab+.5,list(c(2,3,4),c(1,3,4)),fit=T)->AL
AL$lrt
AL$pearson
loglin(germ_final_tab+.5,list(c(2),c(1,3,4)),fit=T)->AL
AL$lrt
loglin(germ_final_tab+.5,list(c(2,3),c(2,4),c(1,3,4)),fit=T)->AL
AL$lrt
loglin(((germ_final_tab)/2)+.5,list(c(2,3,4),c(1,3,4)),fit=T)->AL
AL$lrt
AL$df
loglin(((germ_final_tab)/4)+.5,list(c(2,3,4),c(1,3,4)),fit=T)->AL
AL$lrt
AL$df
loglin(((germ_final_tab)/4)+.5,list(c(2,3), c(2,4),c(1,3,4)),fit=T)->AL
AL$df
AL$lrt
