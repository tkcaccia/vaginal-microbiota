 library("lmerTest")

 r=read.csv("Data/Prog data.csv”)
 
 r[,"Preterm.birth.outcome"]=as.numeric(r[,"Preterm.birth.outcome"])
 
 test <- glm(Preterm.birth.outcome ~ 
             Gestation.at.sample+
             Age+
             BMI+
             CST,  data = r)
 
 anova(test, test = "Chisq")
