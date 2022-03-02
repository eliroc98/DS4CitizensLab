library(dplyr)
#importiamo i dati
data <- read.csv("/Users/lizzy/Desktop/Universita/tirocinio/github/DS4CitizensLab/dataset.csv")
summary(data)

# data <- data %>%
#   dplyr::mutate(ridurre.uso.sn = factor(ridurre.uso.sn)) %>%
#   dplyr::filter(eta<100) %>%
#   dplyr::mutate_at(vars(matches("perche.uso.sn")), factor) %>%
#   dplyr::mutate_at(vars(matches("tipo.contenuto.visto.sn")), factor) %>%
#   dplyr::mutate_at(vars(matches("stato.maritale")), factor) %>%
#   dplyr::mutate_at(vars(matches("abitudine.al.fumo")), factor) %>%
#   dplyr::mutate_at(vars(matches("religione")), factor) %>%
#   dplyr::mutate_at(vars(matches("vivere.con.famiglia")), factor) %>%
#   dplyr::mutate_at(vars(matches("professione")), factor) %>%
#   dplyr::mutate_at(vars(matches("genere")), factor) %>%
#   dplyr::mutate_at(vars(matches("pensare.meglio.senza.sn")), factor) %>%
#   dplyr::mutate_at(vars(matches("credo.a.quello.che.vedo")), factor) %>%
#   dplyr::mutate_at(vars(matches("pressione.da.compagni")), factor) %>%
#   dplyr::mutate_at(vars(matches("area.di.residenza")), factor) %>%
#   dplyr::mutate_at(vars(matches("tipo.social.network")), factor)
# 
# summary(data)

data_reg <- data %>% 
  dplyr::filter(eta<100) %>%
  dplyr::select(-c(X, tipo.social.network, perche.uso.sn, tipo.contenuto.visto.sn, stato.maritale, abitudine.al.fumo, religione,
                                      vivere.con.famiglia, professione, genere, pensare.meglio.senza.sn, credo.a.quello.che.vedo, pressione.da.compagni,
                                      area.di.residenza))

summary(data)

#train test split
library(caret)
set.seed(42)
train_indices <- createDataPartition(data_reg$malessere,p=0.7,list=FALSE)
test_indices <- (-train_indices)

library(lmtest)
library('leaps')
library(modelsummary)
regfit_forward_selection <- regsubsets(malessere~. , data = data_reg[train_indices,], nvmax = 100, method = 'forward')
reg_summary <- summary(regfit_forward_selection)
reg_summary

#plots
par(mfrow=c(2,2))
p <- plot(reg_summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
points(which.min(reg_summary$rss), reg_summary$rss[which.min(reg_summary$rss)], col="red",cex=2,pch=20)
plot(reg_summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
points(which.max(reg_summary$adjr2), reg_summary$adjr2[which.max(reg_summary$adjr2)], col="red",cex=2,pch=20)
plot(reg_summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
points(which.min(reg_summary$cp),reg_summary$cp[which.min(reg_summary$cp)],col="red",cex=2,pch=20)
plot(reg_summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
points(which.min(reg_summary$bic),reg_summary$bic[which.min(reg_summary$bic)],col="red",cex=2,pch=20)

#variables selected
# quanto.tempo.al.giorno.sn, livello.educazione, tipo.contenuto.visto.sn_Get.pleasure.from.funny.posts..memes, 
# pressione.da.compagni_Yes, pensare.meglio.senza.sn_No, genere_Female, abitudine.al.fumo_Smoker, 
# professione_teacher, professione_unemployedincludingstudents, pressione.sn

lm.fit <- lm(malessere ~ quanto.tempo.al.giorno.sn + 
               livello.educazione +
               tipo.contenuto.visto.sn_Get.pleasure.from.funny.posts..memes + 
               pressione.da.compagni_Yes +
               pensare.meglio.senza.sn_No +
               genere_Female +
               abitudine.al.fumo_Smoker +
               professione_teacher +
               professione_unemployedincludingstudents +
               pressione.sn, data = data[train_indices,])

d <- data %>%
  dplyr::select(names(coef(lm.fit))[-1])
correlations <- cor(d)
par(mfrow = c(1,1))
corrplot(correlations, type = "upper",tl.col = "black", tl.srt = 45, method = "number", tl.cex = 0.8, number.cex = 0.7)

library(car)
vif(lm.fit)

#corr female smoker

lm.fit <- lm(malessere ~ quanto.tempo.al.giorno.sn + 
               livello.educazione +
               tipo.contenuto.visto.sn_Get.pleasure.from.funny.posts..memes + 
               pressione.da.compagni_Yes +
               pensare.meglio.senza.sn_No +
               genere_Female +
               professione_teacher +
               professione_unemployedincludingstudents +
               pressione.sn, data = data[train_indices,])

d <- data %>%
  dplyr::select(names(coef(lm.fit))[-1])
correlations <- cor(d)
par(mfrow = c(1,1))
corrplot(correlations, type = "upper",tl.col = "black", tl.srt = 45, method = "number", tl.cex = 0.8, number.cex = 0.7)

library(car)
vif(lm.fit)

# null hypothesis of the Breusch-Pagan test is homoscedasticity
bptest(lm.fit)

#we have to solve this problem: transforming Y into something else using Box Cox transformation
distBCMod <- caret::BoxCoxTrans(data_reg$malessere)
print(distBCMod)

data_reg_mod <- data_reg %>%
  mutate(malessere = predict(distBCMod , malessere))

lm.fit_mod <- lm(malessere ~ quanto.tempo.al.giorno.sn + 
                   livello.educazione +
                   tipo.contenuto.visto.sn_Get.pleasure.from.funny.posts..memes + 
                   pressione.da.compagni_Yes +
                   pensare.meglio.senza.sn_No +
                   genere_Female +
                   professione_teacher +
                   professione_unemployedincludingstudents +
                   pressione.sn, data = data_reg_mod[train_indices,])
summary(lm.fit_mod)
bptest(lm.fit_mod)

resid <- (resid(lm.fit_mod))
#The null hypothesis of Shapiro's test is that the population is distributed normally
shapiro.test(resid) 
par(mfrow=c(2,2))
plot(lm.fit_mod)

#removing outliers and high leverage points
library(MASS)
train_indices_no_out_no_leverage <- train_indices
while(length(train_indices_no_out_no_leverage) > 200){
  model <- lm(malessere ~ quanto.tempo.al.giorno.sn + 
                livello.educazione +
                tipo.contenuto.visto.sn_Get.pleasure.from.funny.posts..memes + 
                pressione.da.compagni_Yes +
                pensare.meglio.senza.sn_No +
                genere_Female +
                professione_teacher +
                professione_unemployedincludingstudents +
                pressione.sn, data = data_reg_mod[train_indices_no_out_no_leverage,])
  #let's see outliers: we need studentized residuals
  s_res <- studres(model)
  out <- which(abs(s_res) > 3)
  #leverage points: observations with high leverage high leverage have an unusual value for x
  #calculate leverage for each observation in the model
  cd <- cooks.distance(model)
  h_lev <- which(cd > 4/(nrow(data_reg_mod[train_indices_no_out_no_leverage,]) - ncol(data_reg_mod[train_indices_no_out_no_leverage,]) - 1 - 1))
  if(length(union(out, h_lev)) > 0){
    train_indices_no_out_no_leverage <- train_indices_no_out_no_leverage[-union(out, h_lev)]
  }
  else{
    break
  }
  
}

lm.fit_no_cor_robust_no_out_no_leverage <- lm(malessere ~ quanto.tempo.al.giorno.sn + 
                                                livello.educazione +
                                                tipo.contenuto.visto.sn_Get.pleasure.from.funny.posts..memes + 
                                                pressione.da.compagni_Yes +
                                                pensare.meglio.senza.sn_No +
                                                genere_Female +
                                                professione_teacher +
                                                professione_unemployedincludingstudents +
                                                pressione.sn, data = data_reg_mod[train_indices_no_out_no_leverage,])

#last check
s_resids <- studres(lm.fit_no_cor_robust_no_out_no_leverage)
outliers <- which(abs(s_resids) > 3)
cooksd <- cooks.distance(lm.fit_no_cor_robust_no_out_no_leverage)
h_leverage_points <- which(cooksd > 4/(nrow(data_reg_mod[train_indices_no_out_no_leverage,]) - ncol(data_reg_mod[train_indices_no_out_no_leverage,]) - 1 - 1))

#heteroskedasticity test
bptest(lm.fit_no_cor_robust_no_out_no_leverage)

resid <- (resid(lm.fit_no_cor_robust_no_out_no_leverage))
shapiro.test(resid) 
par(mfrow=c(2,2))
plot(lm.fit_no_cor_robust_no_out_no_leverage)

summary(lm.fit_no_cor_robust_no_out_no_leverage)
