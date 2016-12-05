 q=read.csv("Data/Species data 2.csv")
 
 
 da=NULL
 name=NULL
 meancontrol=NULL
 meantreated=NULL
 fold=NULL
 for(i in 11:50){
   if(mean(as.numeric(q[,i]>0))>0.05){
   ll=lmer(q[,i]~1+
             Timepoint..weeks.+(0+Timepoint..weeks.|Patient.number)+
             treatment+
             BMI+
             Age+
             Cohort+
             (1|Patient.number)+
             (1|Ethnicity),q)
   w=anova(ll)
   if(nrow(w)==5){
     name=c(name,colnames(q)[i])
     da=rbind(da,as.matrix(w)["treatment",])
     meantreated=c(meantreated,mean(q[q[,"treatment"]==1,i]))
     meancontrol=c(meancontrol,mean(q[q[,"treatment"]==0,i]))
   } 
   }
   
 }
 rownames(da)=name
 da=data.frame(Control_group=meancontrol,Progesterone_group=meantreated,log2_of_the_ratio=-log2(meantreated/meancontrol),da,FDR=p.adjust(da[,"Pr(>F)"],method="fdr"))
 da
