library(dplyr)
#importiamo i dati
data <- read.csv("/Users/lizzy/Desktop/Universita/tirocinio/github/DS4CitizensLab/dataset.csv")

#visualizziamo i dati
View(data)

#diamo un'occhiata veloce alle statistiche principali
summary(data)
print(paste0("Numero di colonne: ", ncol(data)))
print(paste0("Numero di righe: ", nrow(data)))

#rappresentazione grafica di qualche variabile con ggplot
library(ggplot2)
data <- data[data$eta < 100,]
ggplot(data = data, aes(x=eta)) +
  geom_density()

ggplot(data = data, aes(x=eta)) +
  geom_boxplot()

ggplot(data = data, aes(x=da.quando.utilizzo.sn)) +
  geom_density()

data$livello.educazione <- as.factor(data$livello.educazione)
ggplot(data = data, aes(x=livello.educazione)) +
  geom_bar()

n <- lapply(colnames(data)[12:16], function(o) strsplit(o, "_")[[1]][[2]])
data$perche.uso.sn_cat <- unlist(apply(data[12:16], 1, function(o) n[which(o==1)]))
data$perche.uso.sn_cat <- as.factor(data$perche.uso.sn_cat)
ggplot(data = data, aes(x=perche.uso.sn_cat)) +
  geom_bar()

pie(table(factor(data$perche.uso.sn_cat)))




