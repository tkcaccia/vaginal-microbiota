 library("lmerTest")

d=read.csv("Data/Species data 1.csv") 
 d[,"Preterm.birth.outcome"]=as.numeric(d[,"Preterm.birth.outcome"])
 
 da=NULL
 name=NULL
 meanbad=NULL
 meangood=NULL
 fold=NULL
 for(i in 7:56){ 
   if(mean(as.numeric(d[,i]>0))>0.05){
     ll=lmer(Preterm.birth.outcome~1+
               Gestation.at.sample+
               BMI+
               Age+
               d[,i]+
               (1|Ethnicity),d)
     w=anova(ll)
     if(nrow(w)==4){
       name=c(name,colnames(d)[i])
       da=rbind(da,as.matrix(w)["d[, i]",])
       meangood=c(meangood,mean(d[d[,"Preterm.birth.outcome"]==2,i]))
       meanbad=c(meanbad,mean(d[d[,"Preterm.birth.outcome"]==1,i]))
       
     }
   }
 }
 rownames(da)=name
 da=data.frame(before34=meanbad,after34=meangood,log2_of_the_ratio=-log2(meanbad/meangood),da,FDR=p.adjust(da[,"Pr(>F)"],method="fdr"))
 da
