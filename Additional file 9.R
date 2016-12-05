library("lmerTest")


u=read.csv("Data/Prog longitudinal data.csv")
 
 
 u1=u
 u1[,"CST"]=as.numeric(u1[,"CST"]=="I")
 ll=lmer(CST~1+
           Timepoint..weeks.+(0+Timepoint..weeks.|Patient.number)+
           treatment+
           BMI+
           Age+
           Cohort+
           (1|Patient.number)+
           (1|Ethnicity),u1)
 a=anova(ll)
 
 u2=u
 u2[,"CST"]=as.numeric(u2[,"CST"]=="II")
 ll=lmer(CST~0+
           Timepoint..weeks.+(0+Timepoint..weeks.|Patient.number)+
           treatment+
           BMI+
           Age+
           Cohort+
           (1|Patient.number)+
           (1|Ethnicity),u2)
 b=anova(ll)
 
 u3=u
 u3[,"CST"]=as.numeric(u3[,"CST"]=="III")
 ll=lmer(CST~1+
           Timepoint..weeks.+(0+Timepoint..weeks.|Patient.number)+
           treatment+
           BMI+
           Age+
           Cohort+
           (1|Patient.number)+
           (1|Ethnicity),u3)
 c=anova(ll)
 
 u4=u
 u4[,"CST"]=as.numeric(u4[,"CST"]=="IV")
 ll=lmer(CST~1+
           Timepoint..weeks.+(0+Timepoint..weeks.|Patient.number)+
           treatment+
           BMI+
           Age+
           Cohort+
           (1|Patient.number)+
           (1|Ethnicity),u4)
 d=anova(ll)
 
 u5=u
 u5[,"CST"]=as.numeric(u5[,"CST"]=="V")
 ll=lmer(CST~1+
           Timepoint..weeks.+(0+Timepoint..weeks.|Patient.number)+
           treatment+
           BMI+
           Age+
           Cohort+
           (1|Patient.number)+
           (1|Ethnicity),u5)
 e=anova(ll)
 
 
 
 tt=rbind(a["treatment",],b["treatment",],c["treatment",],d["treatment",],e["treatment",])
 fdr=p.adjust(tt[,6])
 tt=cbind(tt,fdr)
 rownames(tt)=c("CSTI","CSTII","CSTIII","CSTIV","CSTV")
 tt
