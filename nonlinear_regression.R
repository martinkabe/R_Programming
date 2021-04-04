
# install.packages("robustbase")
# install.packages("drc")
library(robustbase)
library(drc)
library(tidyverse)

DNase1 <- DNase[ DNase$Run == 1, ]

DN2 <- DNase1
DN2[5,"density"] <- 1.5*DN2[5,"density"]

RmN1  <- nlrob(density ~ Asym/(1 + exp(( xmid - log(conc) )/scal ) ),
                data = DN2,
                start = list( Asym = 3, xmid = 0, scal = 1 ))

rm2DNase1 <- nlrob(density ~ 1/(1 + exp(( xmid - log(conc) )/scal ) ),
                   data = DN2, start = c( xmid = 0, scal = 1 ),
                   alg = "plinear")

algae.m1 <- drc::drm(DN2$density~DN2$conc, data=DN2, fct=LL.4())

bestModel = data.frame(nlrob_model=RmN1$residuals,
                          nlrob_condLinearity_model=rm2DNase1$residuals,
                          drm_model=algae.m1$predres[,2]) %>% 
  mutate(
    Best=names(.)[apply(., 1, function(x) which.min(abs(x)))]
  ) %>% pull(Best) %>% table() %>% as.data.frame() %>% `colnames<-`(c("Model", "Frequency")) %>%
  arrange(desc(Frequency))

pred_lines = data.frame(x=seq(0, max(DN2$conc), by=0.01),
                        nlrob_preds=predict(RmN1, newdata = data.frame(conc=seq(0, max(DN2$conc), by=0.01))),
                        nlrob_condLinearity_preds=predict(rm2DNase1, newdata = data.frame(conc=seq(0, max(DN2$conc), by=0.01))),
                        drm_preds=predict(algae.m1, newdata = data.frame(conc=seq(0, max(DN2$conc), by=0.01)))) %>%
  gather(., "Algo", "Value", c(-x))

ggplot(DN2, mapping=aes(x=conc, y=density)) +
  geom_point() +
  geom_line(pred_lines, mapping=aes(x=x, y=Value, col=Algo)) +
  ggtitle("nlrob with outlier") +
  scale_color_manual(values=c("red", "blue", "black"))


# Identify Outliers -------------------------------------------------------
library(car)

robust_model <- data.frame(x=1:length(algae.m1$residuals),
                           drm_model=algae.m1$predres[,2],
                           nlrob_model=RmN1$residuals)
car::outlierTest(MASS::rlm(drm_model~x, data=robust_model))
car::outlierTest(MASS::rlm(nlrob_model~x, data=robust_model))


car::outlierTest(lm(drm_model~x, data=robust_model))
car::outlierTest(lm(nlrob_model~x, data=robust_model))

