#Starter
setwd("C:/Users/ewado/OneDrive/Dokumenty/Studia/R-mat/Ekonometria")

 library(rgdal)
 library(AER)
 library(skedastic)
 library(tseries)
 library(stargazer)


grid<-readOGR(".", "model_drogi")
grid.df<-as.data.frame(grid)
grid<-spTransform(grid, CRS("+proj=longlat +datum=NAD83"))
colnames(grid.df)<-c("ID","Population","dist_center","district","river","bus_tram_stops","greeness", "dist_metro", "is_airport", "greeness_pct",
                     "Bemowo", "Bialoleka", "Bielany", "Mokotow", "Outskirts", "Ochota", "Praga_Polnoc",
                     "Praga_Poludnie", "Rembertow", "Targowek", "Ursus", "Ursynow", "Wawer", "Wesola", "Wilanow",
                     "Wlochy", "Wola", "Zoliborz", "roads")

y<-grid.df[grid.df$Population>0,]
y$is_river<-as.numeric(y$river)
y$dist_center<-y$dist_center/1000 #metry na kilometry
y$dist_metro<-y$dist_metro/1000
y$roads<-y$roads/1000

# General model 
model<-lm(Population~dist_center+river+bus_tram_stops+roads+dist_metro+is_airport+greeness_pct+Bemowo+
            Bialoleka+Bielany+Mokotow+Outskirts+Ochota+Praga_Polnoc+Praga_Poludnie+Rembertow+Targowek+
            Ursus+Ursynow+Wawer+Wesola+Wilanow+Wlochy+Wola+Zoliborz+dist_center*bus_tram_stops+dist_center*dist_metro+dist_center*roads, data=y)



# Better model
y$log_pop<-log(y$Population)
y$bus_tram_stops_2<-I(y$bus_tram_stops^2)
y$bus_tram_stops_3<-I(y$bus_tram_stops^3)
y$dist_centerXroads<-y$dist_center*y$roads
y$dist_centerXdist_metro<-y$dist_center*y$dist_metro
y$dist_centerXbus_tram_stops<-y$dist_center*y$bus_tram_stops

ols.better<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+roads+dist_metro+
                is_airport+Bemowo+greeness+Bialoleka+Bielany+Mokotow+Outskirts+Ochota+Praga_Polnoc+Praga_Poludnie+
                Rembertow+Targowek+Ursus+Ursynow+Wawer+Wesola+Wilanow+Wlochy+Wola+Zoliborz+dist_centerXbus_tram_stops+
                dist_centerXdist_metro+dist_centerXroads, data=y)

resettest(ols.better)
bptest(ols.better)
white_lm(ols.better)
jarque.bera.test(ols.better$residuals) #tego nie zdaje ale jest du¿o obserwacji
summary(ols.better)
vif(ols.better)

model1<-ols.better

# Are districts jointly significant?
linearHypothesis(model = ols.better, c("Bielany=0", "Bialoleka=0","Praga_Polnoc=0", "Praga_Poludnie=0", "Rembertow=0", "Zoliborz=0",
                                      "Targowek=0", "Ursus=0", "Ursynow=0", "Wawer=0", "Wesola=0", "Wola=0", "Wilanow=0",
                                      "Wlochy=0", "Bemowo=0", "Mokotow=0", "Outskirts=0", "Ochota=0"))
# Yes

# Remove Wesola - highest p-value
olsd0<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+roads+dist_metro+
                 is_airport+Bemowo+greeness+Bialoleka+Bielany+Mokotow+Outskirts+Ochota+Praga_Polnoc+Praga_Poludnie+
                 Rembertow+Targowek+Ursus+Ursynow+Wawer+Wilanow+Wlochy+Wola+Zoliborz+dist_centerXbus_tram_stops+
                 dist_centerXdist_metro+dist_centerXroads, data=y)

resettest(olsd0)
bptest(olsd0)
summary(olsd0) 

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0")) # We can remove Bemowo

olsd1<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+roads+dist_metro+
            is_airport+greeness+Bialoleka+Bielany+Mokotow+Outskirts+Ochota+Praga_Polnoc+Praga_Poludnie+
            Rembertow+Targowek+Ursus+Ursynow+Wawer+Wilanow+Wlochy+Wola+Zoliborz+dist_centerXbus_tram_stops+
            dist_centerXdist_metro+dist_centerXroads, data=y)

resettest(olsd1)
bptest(olsd1)
summary(olsd1) 

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0")) # We can remove interaction

olsd2<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+roads+dist_metro+
            is_airport+greeness+Bialoleka+Bielany+Mokotow+Outskirts+Ochota+Praga_Polnoc+Praga_Poludnie+
            Rembertow+Targowek+Ursus+Ursynow+Wawer+Wilanow+Wlochy+Wola+Zoliborz+
            dist_centerXdist_metro+dist_centerXroads, data=y)


resettest(olsd2)
bptest(olsd2)
summary(olsd2) 

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0")) # We can remove Praga_Polnoc

olsd3<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+roads+dist_metro+
            is_airport+greeness+Bialoleka+Bielany+Mokotow+Outskirts+Ochota+Praga_Poludnie+
            Rembertow+Targowek+Ursus+Ursynow+Wawer+Wilanow+Wlochy+Wola+Zoliborz+
            dist_centerXdist_metro+dist_centerXroads, data=y)

resettest(olsd3)
bptest(olsd3)
summary(olsd3) 

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0")) # We can remove roads

olsd4<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+
            is_airport+greeness+Bialoleka+Bielany+Mokotow+Outskirts+Ochota+Praga_Poludnie+
            Rembertow+Targowek+Ursus+Ursynow+Wawer+Wilanow+Wlochy+Wola+Zoliborz+
            dist_centerXdist_metro+dist_centerXroads, data=y)

resettest(olsd4)
bptest(olsd4)
summary(olsd4)

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                      "Ochota=0")) # We can remove Ochota

olsd5<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+
            is_airport+greeness+Bialoleka+Bielany+Mokotow+Outskirts+Praga_Poludnie+
            Rembertow+Targowek+Ursus+Ursynow+Wawer+Wilanow+Wlochy+Wola+Zoliborz+
            dist_centerXdist_metro+dist_centerXroads, data=y)


resettest(olsd5)
bptest(olsd5)
summary(olsd5)

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                      "Ochota=0", "Ursus=0")) # We can remove Ursus

olsd6<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+is_airport+
            greeness+Bialoleka+Bielany+Mokotow+Outskirts+Praga_Poludnie+Rembertow+Targowek+Ursynow+Wawer+Wilanow+
            Wlochy+Wola+Zoliborz+dist_centerXdist_metro+dist_centerXroads, data=y)

resettest(olsd6)
bptest(olsd6)
summary(olsd6)

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                      "Ochota=0", "Ursus=0", "Rembertow=0")) # We can remove Rembertow

olsd7<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+is_airport+
            greeness+Bialoleka+Bielany+Mokotow+Outskirts+Praga_Poludnie+Targowek+Ursynow+Wawer+Wilanow+
            Wlochy+Wola+Zoliborz+dist_centerXdist_metro+dist_centerXroads, data=y)

resettest(olsd7)
bptest(olsd7)
summary(olsd7)

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                      "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0")) # We can remove Zoliborz

olsd8<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+is_airport+
            greeness+Bialoleka+Bielany+Mokotow+Outskirts+Praga_Poludnie+Targowek+Ursynow+Wawer+Wilanow+
            Wlochy+Wola+dist_centerXdist_metro+dist_centerXroads, data=y)

resettest(olsd8)
bptest(olsd8)
summary(olsd8)

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                      "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0", "Outskirts=0")) # We can remove Outskirts

olsd9<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+is_airport+
            greeness+Bialoleka+Bielany+Mokotow+Praga_Poludnie+Targowek+Ursynow+Wawer+Wilanow+
            Wlochy+Wola+dist_centerXdist_metro+dist_centerXroads, data=y)

resettest(olsd9)
bptest(olsd9)
summary(olsd9)

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                      "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0", "Outskirts=0", "Bielany=0")) # We can remove Bielany

olsd10<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+is_airport+
             greeness+Bialoleka+Mokotow+Praga_Poludnie+Targowek+Ursynow+Wawer+Wilanow+
             Wlochy+Wola+dist_centerXdist_metro+dist_centerXroads, data=y)


resettest(olsd10)
bptest(olsd10)
summary(olsd10)

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                       "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0", "Outskirts=0", "Bielany=0", "Mokotow=0")) # We can remove Mokotow

olsd11<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+is_airport+
             greeness+Bialoleka+Praga_Poludnie+Targowek+Ursynow+Wawer+Wilanow+Wlochy+Wola+dist_centerXdist_metro+
             dist_centerXroads, data=y)

resettest(olsd11) 
bptest(olsd11) 
summary(olsd11) 

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                       "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0", "Outskirts=0", "Bielany=0", "Mokotow=0",
                                      "Wawer=0")) # We can remove Wawer

olsd12<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+is_airport+
             greeness+Bialoleka+Praga_Poludnie+Targowek+Ursynow+Wilanow+Wlochy+Wola+dist_centerXdist_metro+
             dist_centerXroads, data=y)

resettest(olsd12) 
bptest(olsd12) 
summary(olsd12) 

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                       "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0", "Outskirts=0", "Bielany=0", "Mokotow=0",
                                       "Wawer=0", "Wola=0")) # We can remove Wole

olsd13<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+is_airport+
             greeness+Bialoleka+Praga_Poludnie+Targowek+Ursynow+Wilanow+Wlochy+dist_centerXdist_metro+
             dist_centerXroads, data=y)



resettest(olsd13) 
bptest(olsd13) 
summary(olsd13)

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                       "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0", "Outskirts=0", "Bielany=0", "Mokotow=0",
                                       "Wawer=0", "Wola=0", "Targowek=0")) # We can remove Targowek

olsd14<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+is_airport+
             greeness+Bialoleka+Praga_Poludnie+Ursynow+Wilanow+Wlochy+dist_centerXdist_metro+dist_centerXroads, data=y)


resettest(olsd14) 
bptest(olsd14) # test failed - it's better not to remove Targowek
summary(olsd14) 

# Back to modelu 13 & try to remove something else

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                       "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0", "Outskirts=0", "Bielany=0", "Mokotow=0",
                                       "Wawer=0", "Wola=0", "Ursynow=0")) # We can remove Ursynow

olsd15<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+is_airport+
             greeness+Bialoleka+Praga_Poludnie+Targowek+Wilanow+Wlochy+dist_centerXdist_metro+
             dist_centerXroads, data=y)


resettest(olsd15) 
bptest(olsd15) 
summary(olsd15) 

linearHypothesis(model = ols.better, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                       "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0", "Outskirts=0", "Bielany=0", "Mokotow=0",
                                       "Wawer=0", "Wola=0", "Ursynow=0", "is_airport=0")) # We can remove is_airport

olsd16<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+
             greeness+Bialoleka+Praga_Poludnie+Targowek+Wilanow+Wlochy+dist_centerXdist_metro+
             dist_centerXroads, data=y)

resettest(olsd16) # passed 
bptest(olsd16) # passed
summary(olsd16) # insignificant Targowek but let's keep it - removing didn't work

model2<-olsd16

vif(olsd16) #ok
#plot(olsd16) # remove observation 219

z<-y[-219,]

ols.out<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+
              greeness+Bialoleka+Praga_Poludnie+Targowek+Wilanow+Wlochy+dist_centerXdist_metro+
              dist_centerXroads, data=z)

resettest(ols.out) # passed 
bptest(ols.out) # passed
summary(ols.out)

ols.better.out<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+roads+dist_metro+
                     is_airport+Bemowo+greeness+Bialoleka+Bielany+Mokotow+Outskirts+Ochota+Praga_Polnoc+Praga_Poludnie+
                     Rembertow+Targowek+Ursus+Ursynow+Wawer+Wesola+Wilanow+Wlochy+Wola+Zoliborz+dist_centerXbus_tram_stops+
                     dist_centerXdist_metro+dist_centerXroads, data=z)

linearHypothesis(model = ols.better.out, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                           "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0", "Outskirts=0", "Bielany=0", "Mokotow=0",
                                           "Wawer=0", "Wola=0", "Ursynow=0", "is_airport=0", "Targowek=0")) # We can remove Targowek

ols.out1<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+greeness+
            Bialoleka+Praga_Poludnie+Wilanow+Wlochy+dist_centerXdist_metro+dist_centerXroads, data=z)

resettest(ols.out1) # passed 
bptest(ols.out1) # passed
summary(ols.out1) # better!

linearHypothesis(model = ols.better.out, c("Wesola=0", "Bemowo=0", "dist_centerXbus_tram_stops=0", "Praga_Polnoc=0", "roads=0",
                                           "Ochota=0", "Ursus=0", "Rembertow=0", "Zoliborz=0", "Outskirts=0", "Bielany=0", "Mokotow=0",
                                           "Wawer=0", "Wola=0", "Ursynow=0", "is_airport=0", "Targowek=0", "greeness=0")) # We can remove greeness

ols.out2<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+
               Bialoleka+Praga_Poludnie+Wilanow+Wlochy+dist_centerXdist_metro+dist_centerXroads, data=z)

resettest(ols.out2) # passed 
bptest(ols.out2) # failed. GETS ends on the model ols.out1
summary(ols.out2) 

model3<-ols.out1

modelHC0<-coeftest(ols.out1, vcov=vcovHC(ols.out1, type="HC0")) # model with a robust matrix

# Weighted OLS

modelWLS<-lm(log_pop~dist_center+is_river+bus_tram_stops+bus_tram_stops_2+bus_tram_stops_3+dist_metro+greeness+
                  Bialoleka+Praga_Poludnie+Wilanow+Wlochy+dist_centerXdist_metro+dist_centerXroads, data=z, weights=1/dist_center)

bptest(modelWLS)
bptest(modelWLS) # nothing changed - distance to the city center doesn't affect heteroscedasticity
summary(modelWLS)

#FGLS

e <- model3$residuals
e2 <- e^2
ln_e2 <- log(e2)
regression_supp <- lm(ln_e2~dist_center+dist_metro+bus_tram_stops+greeness, data=z)
ln_e2_hat <- fitted(regression_supp)
e2_hat <- exp(ln_e2_hat)

log_pop <- log(z$Population)/sqrt(e2_hat)
dist_center <- z$dist_center/sqrt(e2_hat)
dist_metro <- z$dist_metro/sqrt(e2_hat)
bus_tram_stops <- z$bus_tram_stops/sqrt(e2_hat)
bus_tram_stops_2 <- I(z$bus_tram_stops^2)/sqrt(e2_hat)
bus_tram_stops_3 <- I(z$bus_tram_stops^3)/sqrt(e2_hat)
intercept <- 1/sqrt(e2_hat)
greeness <- z$greeness/sqrt(e2_hat)
river <- as.numeric(z$river)/sqrt(e2_hat)
Bialoleka <- z$Bialoleka/sqrt(e2_hat)
Praga_Poludnie <- z$Praga_Poludnie/sqrt(e2_hat)
Wilanow <- z$Wilanow/sqrt(e2_hat)
Wlochy <- z$Wlochy/sqrt(e2_hat)
roads<-z$roads/sqrt(e2_hat)
dist_centerXdist_metro <- z$dist_center/sqrt(e2_hat)*z$dist_metro/sqrt(e2_hat)
dist_centerXroads <- z$dist_center/sqrt(e2_hat)*z$roads/sqrt(e2_hat)


modelFGLS <- lm(log_pop~intercept+dist_center+river+bus_tram_stops+bus_tram_stops+bus_tram_stops_3+dist_metro+greeness+
                  Bialoleka+Praga_Poludnie+Wilanow+Wlochy+dist_centerXdist_metro+dist_centerXroads-1)
summary(modelFGLS)

# model1 - first model to GETS
# model2 - first model from GETS
# model3 - second model from GETS, after removing observation 219
# modelHC0 - model 3 with a robust matrix
# modelWLS - Weighted OLS model
# modelFGLS - feasible generalized least squares model

stargazer(model1, model2, model3, modelWLS, modelFGLS, modelHC0, type="text", 
          df=FALSE, column.labels=c("model1", "model2", "model3", "modelWLS", "modelFGLS", "modelHC0"))

# The final model - model 3: diagnostics

resettest(model3) # passed, p-value > 0.05, good fit
bptest(model3) # passed, p-value > 0.05, homoscedastic residuals
white_lm(model3) # passed, p-value > 0.05, homoscedastic residuals
jarque.bera.test(model3$residuals) # failed, p-value < 0.05, the distribution of residuals isn't normal

# Chow test for parameter stability

# We split observations into two groups based on the distance from the center

group1<-z[z$dist_center<5,]
group2<-z[z$dist_center>=5,]

# Two regressions - one for each group
regression1<-lm(log_pop ~ dist_center + is_river + bus_tram_stops + 
                  bus_tram_stops_2 + bus_tram_stops_3 + dist_metro + greeness + 
                  Bialoleka + Praga_Poludnie + Targowek + Wilanow + Wlochy + 
                  dist_centerXdist_metro + dist_centerXroads, data=group1)

regression2<-lm(log_pop ~ dist_center + is_river + bus_tram_stops + 
                  bus_tram_stops_2 + bus_tram_stops_3 + dist_metro + greeness + 
                  Bialoleka + Praga_Poludnie + Targowek + Wilanow + Wlochy + 
                  dist_centerXdist_metro + dist_centerXroads, data=group2)

# Sum of the residuals' squares 
sum1<-deviance(regression1) #deviance calculates RSS
sum2<-deviance(regression2)

together<-sum1+sum2

sum<-deviance(model3) # RSS for the whole sample

nominator<-(sum-together)/(14*1)   # two subsamples => m=2 => m-1=1. K=14 (parameters)
denominator<-together/(554-2*14)    # N=554 (number of observations)
Ftest<-nominator/denominator       # Ftest = 0.4425
qf(0.95, 14, 526)              # qf = 1.7106 
p.value<-1-pf(q=Ftest, df1=14, df2=526)

# Ftest < qf, the test statistic does not fall into the critical region, we cannot reject the null hypothesis
# about the parameters' stability. Confirmed by p-value = 1 > 0.05.




