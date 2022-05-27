#### Coxi regressioon ####

library(survival)
library(essurvey)
library(weights)
library(dplyr)
library(descr)
library(survminer)

# Kaplan-Meieri hinnangufunktsiooniga elukestuskõverate koostamine meestele ja naistele

fit_gndr <- survfit(Surv(fcldbthage3, bthcld01) ~ gndr, data = ee9, weights = pspwght)
summary(fit_gndr)

ggsurvplot(fit_gndr, data = ee9, conf.int = TRUE, 
           censor = F, 
           legend.labs = c("Mehed", "Naised"), 
           surv.median.line = "hv", 
           ggtheme = theme_minimal())

# Teeme sooga Coxi regressioonimudeli

ee9 <- ee9 %>% 
  mutate(naine = ifelse(gndr == 2, 1, 0) %>% as.factor())

coxmod1 <- coxph(Surv(fcldbthage3, bthcld01) ~ naine, data = ee9, weights = pspwght)
summary(coxmod1)

# lisame mudelisse haridustee pikkuse ja sünniaasta

coxmod2 <- coxph(Surv(fcldbthage3, bthcld01) ~ naine + eduyrs + yrbrn, data = ee9, weights = pspwght)
summary(coxmod2)

# vaatame mudeli alusel prognoositavat elukestuskõverat

ggsurvplot(survfit(coxmod2), data = ee9, 
           censor = F, 
           surv.median.line = "hv", 
           ggtheme = theme_minimal())

# kuidas erinevad mudeli alusel prognoositavad meeste ja naiste elukestuskõverad?

gndr01_df <- with(ee9,
                  data.frame(naine = as.factor(c(0, 1)),
                             eduyrs = rep(weighted.mean(eduyrs, w = pspwght, na.rm = TRUE), 2),
                             yrbrn = rep(weighted.mean(yrbrn, w = pspwght, na.rm = TRUE), 2)
                  )
)
gndr01_df

fit <- survfit(coxmod2, newdata = gndr01_df)
ggsurvplot(fit, data = gndr01_df, conf.int = TRUE, 
           censor = F, 
           legend.labs = c("Mehed", "Naised"), 
           surv.median.line = "hv", 
           ggtheme = theme_minimal())

# leidke iseseisvalt, kuidas erinevad mudeli alusel prognoositavad elukestuskõverad eri pikkusega haridusteega ja eri aastatel sündinud inimestel. Haridustee puhul võiks võrrelda nt 8, 12 ja 17 aasta pikkuse haridusteega inimeste prognoose ning sünniaasta puhul 1950., 1970. ja 1990. aastal sündinute omasid.

#### Riskide võrdelisuse eeldus ####

# kontrollime tagantjärele ka riskide võrdelisuse eeldust Schönfeldi jääkide alusel
# jäägid ei tohiks ajas varieeruda, sest Coxi mudel eeldab, et sõltumatute tunnuste väärtused ajas ei varieeru

test_ph <- cox.zph(coxmod2)
test_ph

# mida väiksemad jäägid, sega täpsem on mudeli alusel saadav prognoos. Riskide võrdelisuse osas on tähtis, et jäägid peaksid varieeruma sõltumatu tunnuse regressioonikordaja ümber, varieeruvus ei tohiks ajas muutuda

ggcoxzph(test_ph, var = "naine")
ggcoxzph(test_ph, var = "eduyrs")
ggcoxzph(test_ph, var = "yrbrn")




