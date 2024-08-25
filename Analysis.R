library(survival)

###construction of the effortless lifestyle score
#e.g. Tea_Score
fit <- cph(Surv(Timetodeath,Deathyesno)~rcs(n_1488_0_0_outlier,c(5))+sex+SES+age+bmi_classify+ethnic+smoking+drinking+diet+exercise,data = lazy01)#协变量的变量类型记得搞好，RCS好像不能用factor(SES)这个形式，需要转换变量类型,这里是num；phenoage是因变量；tea是自变量，其他的放协变量
anova(fit)
dd$limits$n_1488_0_0_outlier[2] <- 5.29648241 #set reference as 5 cups/day   5.29648241
fit <- update(fit)
summary(fit)
lazy01$Tea_Score[lazy01$n_1488_0_0_outlier > 6 | lazy01$n_1488_0_0_outlier < 4 ] <- '0'
lazy01$Tea_Score[lazy01$n_1488_0_0_outlier >= 4 & lazy01$n_1488_0_0_outlier <= 6 ] <- '1'
#sum
lazy01$lazyscore <- lazy01$Tea_Score+lazy01$Sleep_Score+lazy01$Sun_Score+lazy01$Sedentary_Score
lazy01$lazyscore_cat <- factor(lazy01$lazyscore)


###the associations between the effortless lifestyle score with multiple health outcomes
y_to_process<-c("Surv(TimetoDementia,Dementia_in)","Surv(TimetoCVD,CVD_in)","Surv(TimetoDM,DM_in)","Surv(Timetodeath,Deathyesno)","Surv(TimetoDepre,Depre_in)","Surv(TimetoAnxie,Anxie_in)")
variables<- c("lazyscore_cat+SES+exercise+age+sex+ethnic+smoking+drinking+diet+bmi_classify")
results1<-NULL
results2<-NULL
for (j in 1:length(y_to_process)){
    formula_str <- paste(y_to_process[j], "~",variables)
    res.cox1 <- coxph(as.formula(formula_str) , data =lazy02)
    temp<-summary(res.cox1)
    results1 <- rbind(results1, temp$coefficients[1:4,])
    results2 <- rbind(results2, temp$conf.int[1:4,3:4])
    result <- cbind(results1,results2)
}
write.csv(result, file = "result.csv")

model1 <- lm(PhenoAge~lazyscore_cat+exercise+age+sex+bmi_classify+ethnic+smoking+drinking+diet, data = lazy02)
summary(model1)


###the mediation analysis of SES 
##e.g. PhenoAge
set.seed(2023)
med.fit=lm(lazyscore~factor(SES)+exercise+age+sex+bmi_classify+ethnic+smoking+drinking+diet,data=PhenoAge_med)
out.fit=lm(PhenoAge ~ factor(SES)+lazyscore+exercise+age+sex+bmi_classify+ethnic+smoking+drinking+diet, data = PhenoAge_med)
med.out.re=mediate(med.fit,out.fit,treat="factor(SES)",mediator="lazyscore",control.value="3",treat.value="1",sims=500)
summary(med.out.re)

set.seed(2023)
med.fit=lm(lazyscore~factor(SES)+exercise+age+sex+bmi_classify+ethnic+smoking+drinking+diet,data=PhenoAge_med)
out.fit=lm(PhenoAge ~ factor(SES)+lazyscore+exercise+age+sex+bmi_classify+ethnic+smoking+drinking+diet, data = PhenoAge_med)
med.out.re=mediate(med.fit,out.fit,treat="factor(SES)",mediator="lazyscore",control.value="3",treat.value="2",sims=500)
summary(med.out.re)

model1 <- lm(PhenoAge~SES+exercise+age+sex+bmi_classify+ethnic+smoking+drinking+diet,data=PhenoAge_med)
summary(cox1)
model1 <- lm(PhenoAge~SES+exercise+age+sex+bmi_classify+ethnic+smoking+drinking+diet+lazyscore,data=PhenoAge_med)
summary(cox1)


###the mediation analysis of BMI 
##e.g. CVD
set.seed(2023)
med.fit=lm(lazyscore~bmi_classify+exercise+age+sex+SES+ethnic+smoking+drinking+diet,data=CVD_med)
out.fit=survreg(Surv(TimetoCVD,CVD_in) ~ bmi_classify+lazyscore+exercise+age+sex+SES+ethnic+smoking+drinking+diet, data = CVD_med)
med.out.re=mediate(med.fit,out.fit,treat="bmi_classify",mediator="lazyscore",control.value="1",treat.value="2",sims=500)
summary(med.out.re)

set.seed(2023)
med.fit=lm(lazyscore~bmi_classify+exercise+age+sex+SES+ethnic+smoking+drinking+diet,data=CVD_med)
out.fit=survreg(Surv(TimetoCVD,CVD_in) ~ bmi_classify+lazyscore+exercise+age+sex+SES+ethnic+smoking+drinking+diet, data = CVD_med)
med.out.re=mediate(med.fit,out.fit,treat="bmi_classify",mediator="lazyscore",control.value="1",treat.value="3",sims=500)
summary(med.out.re)

cox1 <- coxph(Surv(TimetoCVD,CVD_in) ~ bmi_classify+exercise+age+sex+SES+ethnic+smoking+drinking+diet, data = lazy03)
summary(cox1)
cox1 <- coxph(Surv(TimetoCVD,CVD_in) ~ bmi_classify+lazyscore+exercise+age+sex+SES+ethnic+smoking+drinking+diet, data =lazy03)
summary(cox1)


###the analysis of brain and peripheral markers
#e.g. peripheral markers
result_df <- data.frame(Dependent_Variable = character(),  
                        Significant_Variable = logical(),  
                        Estimated_Value = numeric(),  
                        P_value = numeric(),
                        Adjusted_p_value = numeric(),  
                        stringsAsFactors = FALSE)  

independent_vars_id <- c('id','lazyscore','SES','age','sex','bmi_classify','exercise','smoking','drinking',
                         'diet','ethnic')  
independent_vars<- c('lazyscore','SES','age','sex','bmi_classify','exercise','smoking','drinking',
                     'diet','ethnic')  
specified_var <- "lazyscore"
for (dep_var in colnames(lazy_BloodMet)) {  
  if (dep_var %in% independent_vars_id) {  
    next  
  }  
  
  formula <- as.formula(paste("scale(",dep_var, ") ~", paste(independent_vars, collapse = "+")))  
  model <- lm(formula, data = lazy_BloodMet)  
  coefficients <- summary(model)$coefficients  
  
  p_values <- summary(model)$coefficients[, "Pr(>|t|)"]  
  adjusted_p_values <- p.adjust(p_values, method = "bonferroni")  
  
  if (specified_var %in% names(adjusted_p_values) && adjusted_p_values[specified_var] < 0.05) { 
    significant_variable <- TRUE
  } else {
    significant_variable <- FALSE
  }
   
  estimated_values <- coefficients[specified_var, "Estimate"]
  p_value <- p_values[specified_var]
  adjusted_p_value <- adjusted_p_values[specified_var]  
  
  result_row <- data.frame(Dependent_Variable = dep_var,   
                           Significant_Variable = significant_variable,   
                           Estimated_Value = estimated_values, 
                           P_value = p_value,
                           Adjusted_p_value = adjusted_p_value)  #True
    
    result_df <- rbind(result_df, result_row)  
}
print(result_df) 
