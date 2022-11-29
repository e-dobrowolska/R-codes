# starter 
{
  library(lmtest)  
  library(BaylorEdPsych)
  library(car)
  library(LogisticDx)
  library(sandwich)
  library(mfx)
  library(stargazer)
  library(tseries)
  
  setwd("C:/Users/ewado/OneDrive/Dokumenty/Studia/R-mat/AE/model")
  
  source("linktest.R")
  source("marginaleffects.R")
  
  data<-read.csv2("ae_dataset.csv")
  names(data)
  
  # Rescale data for more friendly models' coefficients
  data$Restaurant<-ifelse(data$type=="Restaurant",1,0)
  data<-data[,6:32]
  summary(data)
  data$parking_500<-data$parking_500/100
  data$parking_N<-data$parking_N/100
  data$nature_500<-data$nature_500/100
  data$nature_N<-data$nature_N/100
  data$child_pct_500<-data$child_pct_500*100
  data$child_pct_N<-data$child_pct_N*100
  data$elderly_pct_500<-data$elderly_pct_500*100
  data$elderly_pct_N<-data$elderly_pct_N*100
  
  # Data table
  stargazer(data, omit.stat=c("N"), out="data.html")
  
}

# basic models
{ 
  # Basic model equation
  eq<-Restaurant~d_centre+pop_500+pop_N+f2m_500+f2m_N+child_pct_500+child_pct_N+elderly_pct_500+
    elderly_pct_N+transport_500+transport_N+metro_500+metro_N+parking_500+parking_N+
    facility_500+facility_N+hotels_500+hotels_N+schools_500+schools_N+nature_500+nature_N+
    cluster_R+cluster_CV+mall+cluster_R*cluster_CV+I(pop_500^2)
  
  lpm<-lm(eq, data=data)
  summary(lpm)
  resettest(lpm) # bad specification - p-value<0.05   
  bptest(lpm) # residuals are heteroscedastic - p-value<0.05
  jarque.bera.test(lpm$residuals) # residuals aren't normally distributed - p-value<0.05
       # LPM fails all tests but it doesn't matter since even if it passed them it would be bad (biased & inconsistent)
       # let's leave it as it is and move on to logits and probits
  
  lpmHC<-coeftest(lpm, vcov=vcovHC(lpm, type="HC")) # for stargazer table 
  
  logit<-glm(eq, data=data, family=binomial(link="logit"))
  summary(logit)
  
  probit<-glm(eq, data=data, family=binomial(link="probit"))
  summary(probit)
  
  AIC(lpm, logit, probit) # AIC favours the Logit model
  BIC(lpm, logit, probit) # BIC favours the Logit model
  
  PseudoR2(logit)
  PseudoR2(probit)
  
  null_logit<-glm(Restaurant~1, data=data, family=binomial(link="logit"))
  lrtest(null_logit, logit) # OK - all the variables jointly significant
  
  null_probit<-glm(Restaurant~1, data=data, family=binomial(link="probit"))
  lrtest(null_probit, probit) # OK - all the variables jointly significant
  
  linktest(logit) # OK - good specification
  linktest(probit) # OK - good specification
}

# GETS
{
  # AIC, BIC --> logit model
  
  logit0<-logit
  
  # Step 1 - without transport_500
  eq1<-Restaurant~d_centre+pop_500+pop_N+f2m_500+f2m_N+child_pct_500+child_pct_N+elderly_pct_500+
    elderly_pct_N+transport_N+metro_500+metro_N+parking_500+parking_N+
    facility_500+facility_N+hotels_500+hotels_N+schools_500+schools_N+nature_500+nature_N+
    cluster_R+cluster_CV+mall+cluster_R*cluster_CV+I(pop_500^2)
  
  logit1<-glm(eq1, data=data, family=binomial(link="logit"))
  summary(logit1)
  linktest(logit1) # OK - good specification
  gof(logit1)$gof # OK - passed Hosmer-Lemeshow test (although didn't pass Osius-Rojek test)
  
  lrtest(logit0, logit1)  # OK - dropped variables jointly insignificant
  
  # Step 2 - without cluster_R*cluster_CV
  eq2<-Restaurant~d_centre+pop_500+pop_N+f2m_500+f2m_N+child_pct_500+child_pct_N+elderly_pct_500+
    elderly_pct_N+transport_N+metro_500+metro_N+parking_500+parking_N+
    facility_500+facility_N+hotels_500+hotels_N+schools_500+schools_N+nature_500+nature_N+
    cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit2<-glm(eq2, data=data, family=binomial(link="logit"))
  summary(logit2)
  linktest(logit2) # OK
  gof(logit2)$gof # OK
  
  lrtest(logit0, logit2) # OK
  
  # Step 3 - without f2m_N
  eq3<-Restaurant~d_centre+pop_500+pop_N+f2m_500+child_pct_500+child_pct_N+elderly_pct_500+
    elderly_pct_N+transport_N+metro_500+metro_N+parking_500+parking_N+
    facility_500+facility_N+hotels_500+hotels_N+schools_500+schools_N+nature_500+nature_N+
    cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit3<-glm(eq3, data=data, family=binomial(link="logit"))
  summary(logit3)
  linktest(logit3) # OK
  gof(logit3)$gof # OK
  
  lrtest(logit0, logit3) # OK
  
  # Step 4 - without f2m_500
  eq4<-Restaurant~d_centre+pop_500+pop_N+child_pct_500+child_pct_N+elderly_pct_500+
    elderly_pct_N+transport_N+metro_500+metro_N+parking_500+parking_N+
    facility_500+facility_N+hotels_500+hotels_N+schools_500+schools_N+nature_500+nature_N+
    cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit4<-glm(eq4, data=data, family=binomial(link="logit"))
  summary(logit4)
  linktest(logit4) # OK
  gof(logit4)$gof # OK
  
  lrtest(logit0, logit4)  # OK
  
  # Step 5 - without transport_N
  eq5<-Restaurant~d_centre+pop_500+pop_N+child_pct_500+child_pct_N+elderly_pct_500+
    elderly_pct_N+metro_500+metro_N+parking_500+parking_N+
    facility_500+facility_N+hotels_500+hotels_N+schools_500+schools_N+nature_500+nature_N+
    cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit5<-glm(eq5, data=data, family=binomial(link="logit"))
  summary(logit5)
  linktest(logit5) # OK
  gof(logit5)$gof # OK
  
  lrtest(logit0, logit5)  # OK
  
  # Step 6 - without parking_500
  eq6<-Restaurant~d_centre+pop_500+pop_N+child_pct_500+child_pct_N+elderly_pct_500+
    elderly_pct_N+metro_500+metro_N+parking_N+
    facility_500+facility_N+hotels_500+hotels_N+schools_500+schools_N+nature_500+nature_N+
    cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit6<-glm(eq6, data=data, family=binomial(link="logit"))
  summary(logit6)
  linktest(logit6) # OK
  gof(logit6)$gof # OK
  
  lrtest(logit0, logit6)  # OK
  
  # Step 7 - without schools_500
  eq7<-Restaurant~d_centre+pop_500+pop_N+child_pct_500+child_pct_N+elderly_pct_500+
    elderly_pct_N+metro_500+metro_N+parking_N+
    facility_500+facility_N+hotels_500+hotels_N+schools_N+nature_500+nature_N+
    cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit7<-glm(eq7, data=data, family=binomial(link="logit"))
  summary(logit7)
  linktest(logit7) # OK
  gof(logit7)$gof # OK
  
  lrtest(logit0, logit7)  # OK
  
  # Step 8 - without metro_N
  eq8<-Restaurant~d_centre+pop_500+pop_N+child_pct_500+child_pct_N+elderly_pct_500+
    elderly_pct_N+metro_500+parking_N+facility_500+facility_N+hotels_500+hotels_N+schools_N+
    nature_500+nature_N+cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit8<-glm(eq8, data=data, family=binomial(link="logit"))
  summary(logit8)
  linktest(logit8) # OK
  gof(logit8)$gof # OK
  
  lrtest(logit0, logit8)  # OK
  
  # Step 9 - without elderly_pct_N
  eq9<-Restaurant~d_centre+pop_500+pop_N+child_pct_500+child_pct_N+elderly_pct_500+
    metro_500+parking_N+facility_500+facility_N+hotels_500+hotels_N+schools_N+
    nature_500+nature_N+cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit9<-glm(eq9, data=data, family=binomial(link="logit"))
  summary(logit9)
  linktest(logit9) # OK
  gof(logit9)$gof # OK
  
  lrtest(logit0, logit9)  # OK
  
  # Step 10 - without child_pct_N
  eq10<-Restaurant~d_centre+pop_500+pop_N+child_pct_500+elderly_pct_500+
    metro_500+parking_N+facility_500+facility_N+hotels_500+hotels_N+schools_N+
    nature_500+nature_N+cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit10<-glm(eq10, data=data, family=binomial(link="logit"))
  summary(logit10)
  linktest(logit10) # OK
  gof(logit10)$gof # OK
  
  lrtest(logit0, logit10)  # OK
  
  # Step 11 - without nature_N 
  eq11<-Restaurant~d_centre+pop_500+pop_N+child_pct_500+elderly_pct_500+
    metro_500+parking_N+facility_500+facility_N+hotels_500+hotels_N+schools_N+
    nature_500+cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit11<-glm(eq11, data=data, family=binomial(link="logit"))
  summary(logit11)
  linktest(logit11) # OK
  gof(logit11)$gof # OK
  
  lrtest(logit0, logit11)  # OK
  
  # Step 12 - without d_centre 
  eq12<-Restaurant~pop_500+pop_N+child_pct_500+elderly_pct_500+
    metro_500+parking_N+facility_500+facility_N+hotels_500+hotels_N+schools_N+
    nature_500+cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit12<-glm(eq12, data=data, family=binomial(link="logit"))
  summary(logit12)
  linktest(logit12) # OK
  gof(logit12)$gof # OK
  
  lrtest(logit0, logit12) # OK
  
  # Step 13 - without metro_500  
  eq13<-Restaurant~pop_500+pop_N+child_pct_500+elderly_pct_500+
    parking_N+facility_500+facility_N+hotels_500+hotels_N+schools_N+
    nature_500+cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit13<-glm(eq13, data=data, family=binomial(link="logit"))
  summary(logit13)
  linktest(logit13) # OK
  gof(logit13)$gof # OK
  
  lrtest(logit0, logit13)  # OK
  
  # Step 14 - without hotels_N  
  eq14<-Restaurant~pop_500+pop_N+child_pct_500+elderly_pct_500+parking_N+facility_500+
    facility_N+hotels_500+schools_N+nature_500+cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit14<-glm(eq14, data=data, family=binomial(link="logit"))
  summary(logit14)
  linktest(logit14) # OK
  gof(logit14)$gof # OK
  
  lrtest(logit0, logit14)  # OK
  
  # Step 15 - without hotels_500  
  eq15<-Restaurant~pop_500+pop_N+child_pct_500+elderly_pct_500+parking_N+facility_500+
    facility_N+schools_N+nature_500+cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit15<-glm(eq15, data=data, family=binomial(link="logit"))
  summary(logit15)
  linktest(logit15) # OK
  gof(logit15)$gof # OK
  
  lrtest(logit0, logit15)  # OK
  
  # Step 16 - without pop_500  
  eq16<-Restaurant~pop_N+child_pct_500+elderly_pct_500+parking_N+facility_500+
    facility_N+schools_N+nature_500+cluster_R+cluster_CV+mall+I(pop_500^2)
  
  logit16<-glm(eq16, data=data, family=binomial(link="logit"))
  summary(logit16)
  linktest(logit16) # OK
  gof(logit16)$gof # OK
  
  lrtest(logit0, logit16)  # OK
  
  # Step 17 - without I(pop_500^2)  
  eq17<-Restaurant~pop_N+child_pct_500+elderly_pct_500+parking_N+facility_500+
    facility_N+schools_N+nature_500+cluster_R+cluster_CV+mall
  
  logit17<-glm(eq17, data=data, family=binomial(link="logit"))
  summary(logit17)
  linktest(logit17) # OK
  gof(logit17)$gof # OK
  
  lrtest(logit0, logit17) # OK
  
  
      ### The end of GETS approach - all the variables are statistically significant ###
  
  logit_final<-logit17
  
}

# Final model interpretation
{
  linktest(logit_final) # OK - the specification of the model is fine
  gof(logit_final)$gof # fairly OK - H-L test passed, O-R test failed 
  
  PseudoR2(logit_final) # R2 McKelvey.Zavoina, R2 Count and R2 Adj.Count - described in the paper
  
  # Quality publication table:
  
  r2logit<-PseudoR2(logit)
  r2probit<-PseudoR2(probit)
  r2log17<-PseudoR2(logit17)

  lr.logit<-lrtest(null_logit, logit) # OK
  lr.probit<-lrtest(null_probit, probit) # OK
  lr.logit17<-lrtest(null_logit, logit17) # OK

  gof.logit<-gof(logit)$gof
  gof.probit<-gof(probit)$gof
  gof.logit17<-gof(logit17)$gof
  
  # Additional lines
  bic<-c("Bayesian Inf. Crit.",  "", round(BIC(logit), 3), round(BIC(probit),3), round(BIC(logit17),3))
  hltest<-c("Hosmer-Lemeshow test", "", paste0(round(gof.logit$val[1],3), " "), paste0(round(gof.probit$val[1],3), " "), 
            paste0(round(gof.logit17$val[1],3), " *"))
  orstat<-c("Osius-Rojek test",  "", paste0(round(gof.logit$val[3],3), " ***"), paste0(round(gof.probit$val[3],3), " ***"), 
            paste0(round(gof.logit17$val[3],3), "**"))
  R2<-c("R2",  round(summary(lpm)$r.squared,3), "", "", "")
  adjR2<-c("Adjusted R2",  round(summary(lpm)$adj.r.squared,3), "", "", "")
  R2mkz<-c("R2 McKelvey Zavoina",  "", round(r2logit["McKelvey.Zavoina"],3), round(r2probit["McKelvey.Zavoina"],3), round(r2log17["McKelvey.Zavoina"],3))
  R2count<-c("R2 Count", "", round(r2logit["Count"],3), round(r2probit["Count"],3), round(r2log17["Count"],3))
  R2adjcount<-c("R2 Adjusted Count",  "", round(r2logit["Adj.Count"],3), round(r2probit["Adj.Count"],3), round(r2log17["Adj.Count"],3))
  LR<-c("LR test",  "", paste0(round(lr.logit$Chisq[2],3), " ***"), paste0(round(lr.probit$Chisq[2], 3)," ***"), 
        paste0(round(lr.logit17$Chisq[2],3), " ***"))
  
  stargazer(lpmHC, logit, probit, logit17, out="table.html",
            column.labels=c("LPM", "Logit", "Probit", "Final"),
            model.names=F, add.lines=list(hltest, orstat, R2, adjR2, R2mkz, R2count, R2adjcount, LR, bic))
  
  #Linktest result
  stargazer(linktest(logit17), out="linktest.html")
  
  #*p<0.1; **p<0.05; ***p<0.01
}

# Marginal effects, odds ratios
{
  # Marginal effects - described in the paper
  mef<-logitmfx(logit_final, data = data, atmean=TRUE)
  mef<-mef$mfxest
  mefdf<-as.data.frame(mef)
  
  # Marginal effects - table for the paper
  stargazer(mefdf, out="mf.html", summary=F, title="Marginal effects", align=T)

  # Odds ratios - described in the paper
  odds_ratios<-exp(coef(logit_final))
  
  # Odds ratios table for the paper
  stargazer(odds_ratios, out="oddsratio.html", summary=F, title="Odds ratios", align=T)

}
