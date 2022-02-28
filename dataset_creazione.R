#https://www.sciencedirect.com/science/article/pii/S2352340921008684
library(dplyr)
library(readxl)
dataset <- read.csv("/Users/lizzy/Desktop/Universita/tirocinio/github/DS4CitizensLab/dataset_originale.csv", sep = ";")
View(dataset)

#X2..Which.social.media.account.do.you.use.usually.
#tipo sn
dataset$X2..Which.social.media.account.do.you.use.usually.[which(dataset$X2..Which.social.media.account.do.you.use.usually. %in% c("Facebook", 
                                                                                                                                   "Instagram", 
                                                                                                                                   "Snapchat", 
                                                                                                                                   "Tiktok ", 
                                                                                                                                   "Twitter"))] <- "Non messaggistica"

dataset$X2..Which.social.media.account.do.you.use.usually.[which(dataset$X2..Which.social.media.account.do.you.use.usually. %in% c("imo", 
                                                                                                                                   "Imo", 
                                                                                                                                   "WeChat", 
                                                                                                                                   "WhatsApp"))] <- "Messaggistica"
#X5..How.long.have.you.been.using.a.social.media.account.
#anni
dataset$X5..How.long.have.you.been.using.a.social.media.account.[which(dataset$X5..How.long.have.you.been.using.a.social.media.account. == "2-5 years")] <- trunc(runif(1, 2.0, 5.0) * 365, 0)
dataset$X5..How.long.have.you.been.using.a.social.media.account.[which(dataset$X5..How.long.have.you.been.using.a.social.media.account. == "5-10 years")] <- trunc(runif(1, 5.0, 10.0) * 365, 0)
distrib <- rnorm(100, mean = 10.0, sd = 5)
value <- distrib[which(distrib >= 10 & distrib < 20)][1]
dataset$X5..How.long.have.you.been.using.a.social.media.account.[which(dataset$X5..How.long.have.you.been.using.a.social.media.account. == "More than 10 years")] <- trunc(value * 365, 0)
distrib <- rnorm(100, mean = 2.0, sd = 1.5)
value <- distrib[which(distrib > 0  & distrib < 2)][1]
dataset$X5..How.long.have.you.been.using.a.social.media.account.[which(dataset$X5..How.long.have.you.been.using.a.social.media.account. == "Less than 2-year")] <- trunc(value * 365, 0)
dataset$X5..How.long.have.you.been.using.a.social.media.account. <- as.numeric(dataset$X5..How.long.have.you.been.using.a.social.media.account.)

#X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media.
#volte a settimana
dataset$X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media.[which(dataset$X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media. == "Less than 1 per day")] <- sum(sample(0:1, 7, TRUE))
dataset$X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media.[which(dataset$X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media. == "1-2 per day")] <- sum(sample(1:2, 7, TRUE))
dataset$X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media.[which(dataset$X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media. == "3-5 per day")] <- sum(sample(3:5, 7, TRUE))
dataset$X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media.[which(dataset$X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media. == "More than 5 per day")] <- sum(sample(6:10, 7, TRUE))
dataset$X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media. <- as.numeric(dataset$X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media.)

#X7..How.much.time.do.you.spend.daily.in.social.media.
#minuti al giorno
dataset$X7..How.much.time.do.you.spend.daily.in.social.media.[which(dataset$X7..How.much.time.do.you.spend.daily.in.social.media. == "Less than 1 hour")] <- trunc(runif(1, 0.0, 0.9) * 60, 0)
dataset$X7..How.much.time.do.you.spend.daily.in.social.media.[which(dataset$X7..How.much.time.do.you.spend.daily.in.social.media. == "1-3 hours")] <- trunc(runif(1, 1.0, 2.9) * 60, 0)
dataset$X7..How.much.time.do.you.spend.daily.in.social.media.[which(dataset$X7..How.much.time.do.you.spend.daily.in.social.media. == "3-5 hours")] <- trunc(runif(1, 3.0, 4.9) * 60, 0)
dataset$X7..How.much.time.do.you.spend.daily.in.social.media.[which(dataset$X7..How.much.time.do.you.spend.daily.in.social.media. == "More than 5 hours")] <- trunc(runif(1, 5.0, 7.9) * 60, 0)
dataset$X7..How.much.time.do.you.spend.daily.in.social.media. <- as.numeric(dataset$X7..How.much.time.do.you.spend.daily.in.social.media.)

#X9..How.many.friends.do.you.have.on.social.media.
#amici
distrib <- rnorm(100, mean = 500, sd = 250)
value <- distrib[which(distrib > 0  & distrib < 500)][1]
value
dataset$X9..How.many.friends.do.you.have.on.social.media.[which(dataset$X9..How.many.friends.do.you.have.on.social.media. == "Less than 500")] <- trunc(value, 0)
dataset$X9..How.many.friends.do.you.have.on.social.media.[which(dataset$X9..How.many.friends.do.you.have.on.social.media. == "500-2000")] <- trunc(runif(1, 500, 1999), 0)
dataset$X9..How.many.friends.do.you.have.on.social.media.[which(dataset$X9..How.many.friends.do.you.have.on.social.media. == "2000-4000")] <- trunc(runif(1, 2000, 3999), 0)
distrib <- rnorm(100, mean = 4000, sd = 1000)
value <- distrib[which(distrib >= 4000)][1]
dataset$X9..How.many.friends.do.you.have.on.social.media.[which(dataset$X9..How.many.friends.do.you.have.on.social.media. == "More than 4000")] <- trunc(value, 0)
dataset$X9..How.many.friends.do.you.have.on.social.media. <- as.numeric(dataset$X9..How.many.friends.do.you.have.on.social.media.)

#amici. percentuale
dataset$X9..How.many.friends.do.you.have.on.social.media.[which(dataset$X10..How.many.friends.do.you.know.personally.in.social.media. == "Few of them")] <- dataset$X9..How.many.friends.do.you.have.on.social.media.[which(dataset$X10..How.many.friends.do.you.know.personally.in.social.media. == "Few of them")] * 0.25
dataset$X9..How.many.friends.do.you.have.on.social.media.[which(dataset$X10..How.many.friends.do.you.know.personally.in.social.media. == "Many of them")] <- dataset$X9..How.many.friends.do.you.have.on.social.media.[which(dataset$X10..How.many.friends.do.you.know.personally.in.social.media. == "Many of them")] * 0.50
dataset$X9..How.many.friends.do.you.have.on.social.media.[which(dataset$X10..How.many.friends.do.you.know.personally.in.social.media. == "Most of them")]<- dataset$X9..How.many.friends.do.you.have.on.social.media.[which(dataset$X10..How.many.friends.do.you.know.personally.in.social.media. == "Most of them")] * 0.75

#X17..Does.your.emotion.get.influenced.by.other.s.posts..success..failure..loss..
dataset$X17..Does.your.emotion.get.influenced.by.other.s.posts..success..failure..loss..[which(dataset$X17..Does.your.emotion.get.influenced.by.other.s.posts..success..failure..loss.. == "Always")] <- 2
dataset$X17..Does.your.emotion.get.influenced.by.other.s.posts..success..failure..loss..[which(dataset$X17..Does.your.emotion.get.influenced.by.other.s.posts..success..failure..loss.. == "Sometimes")] <- 1
dataset$X17..Does.your.emotion.get.influenced.by.other.s.posts..success..failure..loss..[which(dataset$X17..Does.your.emotion.get.influenced.by.other.s.posts..success..failure..loss.. == "Not at all")] <- 0
dataset$X17..Does.your.emotion.get.influenced.by.other.s.posts..success..failure..loss.. <- as.numeric(dataset$X17..Does.your.emotion.get.influenced.by.other.s.posts..success..failure..loss..)

#X18..Have.you.ever.compared.yourself.with.other.s.success.or.luxurious.life.
dataset$X18..Have.you.ever.compared.yourself.with.other.s.success.or.luxurious.life.[which(dataset$X18..Have.you.ever.compared.yourself.with.other.s.success.or.luxurious.life.== "All the times")] <- 2
dataset$X18..Have.you.ever.compared.yourself.with.other.s.success.or.luxurious.life.[which(dataset$X18..Have.you.ever.compared.yourself.with.other.s.success.or.luxurious.life. == "Most of the times")] <- 1
dataset$X18..Have.you.ever.compared.yourself.with.other.s.success.or.luxurious.life.[which(dataset$X18..Have.you.ever.compared.yourself.with.other.s.success.or.luxurious.life. == "Never")] <- 0
dataset$X18..Have.you.ever.compared.yourself.with.other.s.success.or.luxurious.life.<- as.numeric(dataset$X18..Have.you.ever.compared.yourself.with.other.s.success.or.luxurious.life.)

#X20..If.answer.is.yes..are.you.trying.to.control.that.thing.and.trying.to.reduce.the.use.of.social.media.
dataset$X20..If.answer.is.yes..are.you.trying.to.control.that.thing.and.trying.to.reduce.the.use.of.social.media.[which(dataset$X20..If.answer.is.yes..are.you.trying.to.control.that.thing.and.trying.to.reduce.the.use.of.social.media. %in% c("No, I am not trying", "Not trying" , "Yes, I am trying but can’t"))] <- 0
dataset$X20..If.answer.is.yes..are.you.trying.to.control.that.thing.and.trying.to.reduce.the.use.of.social.media.[which(dataset$X20..If.answer.is.yes..are.you.trying.to.control.that.thing.and.trying.to.reduce.the.use.of.social.media. %in% c("Trying to reduce the use", "Trying to stop the use" , "Yes, I am trying and I have reduced using it"))] <- 1
dataset$X20..If.answer.is.yes..are.you.trying.to.control.that.thing.and.trying.to.reduce.the.use.of.social.media.<- as.numeric(dataset$X20..If.answer.is.yes..are.you.trying.to.control.that.thing.and.trying.to.reduce.the.use.of.social.media.)

#X25..Education
dataset$X25..Education[which(dataset$X25..Education == "")] <- NA
dataset$X25..Education[which(dataset$X25..Education == "Primary level")] <- 0
dataset$X25..Education[which(dataset$X25..Education == "Secondary level")] <- 1
dataset$X25..Education[which(dataset$X25..Education == "Higher secondary level")] <- 2
dataset$X25..Education[which(dataset$X25..Education == "Graduate Level")] <- 3
dataset$X25..Education[which(dataset$X25..Education == "Graduate or above")] <- 4
dataset$X25..Education[which(dataset$X25..Education == "Masters or above")] <- 5
dataset$X25..Education<- as.numeric(dataset$X25..Education)

#X27..Monthly.income
dataset$X27..Monthly.income[which(dataset$X27..Monthly.income == "No income")] <- 0
dataset$X27..Monthly.income[which(dataset$X27..Monthly.income == "10,000-40,000 Tk")] <- 1
dataset$X27..Monthly.income[which(dataset$X27..Monthly.income == "40,000-70,000 Tk")] <- 2
dataset$X27..Monthly.income[which(dataset$X27..Monthly.income == "Above 70,000 Tk")] <- 3
dataset$X27..Monthly.income<- as.numeric(dataset$X27..Monthly.income)

lista_variabili <- c("X1..In.the.past.30.days..do.you.feel.lack.of.companionship.", 
                     "X2..In.the.past.30.days..there.is.no.one.I.can.turn.to", 
                     "X3..In.the.past.30.days..I.feel.left.out.",
                     "X4..In.the.last.30.days..I.feel.isolated.from.others.",
                     "X5..In.the.last.30.days..I.am.unhappy.being.so.withdrawn.",
                     "X6..In.the.last.30.days..people.are.around.me.but.not.with.me.",
                     "X7..In.the.last.30.days..I.am.an.outgoing.person.",
                     "X8..In.the.last.30.days..I.can.find.companionship.when.I.want.it.")

for(variabile in lista_variabili){
  print(variabile)
  dataset[which(dataset[, variabile] == "Never"), variabile] <- 0
  dataset[which(dataset[, variabile] == "Rarely"), variabile] <- 1
  dataset[which(dataset[, variabile] == "Sometimes"), variabile] <- 2
  dataset[which(dataset[, variabile] == "Often"), variabile] <- 3
  dataset[, variabile]<- as.numeric(dataset[, variabile])
}

for(i in c(42:57)){
  dataset[which(dataset[, i] == "Not at all"), i] <- 0
  dataset[which(dataset[, i] == "Several days"), i] <- 1
  dataset[which(dataset[, i] %in% c("Half of days", "Half days")), i] <- 2
  dataset[which(dataset[, i] == "Nearly everyday"), i] <- 3
  dataset[i]<- as.numeric(dataset[,i])
}

#X1..When.I.am.usually.gone.to.bed.
dataset$X1..When.I.am.usually.gone.to.bed.[which(dataset$X1..When.I.am.usually.gone.to.bed. == "10.01 PM to 12.00 AM")] <- 1
dataset$X1..When.I.am.usually.gone.to.bed.[which(dataset$X1..When.I.am.usually.gone.to.bed. == "12.01 AM to 2.00 AM")] <- 2
dataset$X1..When.I.am.usually.gone.to.bed.[which(dataset$X1..When.I.am.usually.gone.to.bed. == "2.01 AM to 5.00 AM")] <- 3
dataset$X1..When.I.am.usually.gone.to.bed.[which(dataset$X1..When.I.am.usually.gone.to.bed. == "8.00 PM to 10.00 PM")] <- 0
dataset$X1..When.I.am.usually.gone.to.bed.<- as.numeric(dataset$X1..When.I.am.usually.gone.to.bed.)

#X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night.
dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night.[which(dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night. == "Within 15 minutes")] <- 15
dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night.[which(dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night. == "16-30")] <- 30
dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night.[which(dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night. == "31-60")] <- 60
for(l in levels(factor(dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night.))){
  if(! l %in% c("Within 15 minutes", "16-30", "31-60")){
    t <- strsplit(l, " ")[[1]][3]
    dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night.[which(dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night. == l)] <- t
  }
}
dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night.<- as.numeric(dataset$X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night.)

#X3..When.have.you.usually.gotten.up.in.the.morning.
dataset$X3..When.have.you.usually.gotten.up.in.the.morning.[which(dataset$X3..When.have.you.usually.gotten.up.in.the.morning. == "Within 5.00 AM")] <- 0
dataset$X3..When.have.you.usually.gotten.up.in.the.morning.[which(dataset$X3..When.have.you.usually.gotten.up.in.the.morning. == "5.01 AM to 7.00 AM")] <- 1
dataset$X3..When.have.you.usually.gotten.up.in.the.morning.[which(dataset$X3..When.have.you.usually.gotten.up.in.the.morning. == "7.01 AM to 9.00 AM")] <- 2
dataset$X3..When.have.you.usually.gotten.up.in.the.morning.[which(dataset$X3..When.have.you.usually.gotten.up.in.the.morning. == "After 9.00 AM")] <- 3
dataset$X3..When.have.you.usually.gotten.up.in.the.morning.<- as.numeric(dataset$X3..When.have.you.usually.gotten.up.in.the.morning.)

#X4..How.many.hours.of.actual.sleep.do.you.get.at.night.
dataset$X4..How.many.hours.of.actual.sleep.do.you.get.at.night.[which(dataset$X4..How.many.hours.of.actual.sleep.do.you.get.at.night. == "Less than 4 hours")] <- 0
dataset$X4..How.many.hours.of.actual.sleep.do.you.get.at.night.[which(dataset$X4..How.many.hours.of.actual.sleep.do.you.get.at.night. == "4-6 hours")] <- 1
dataset$X4..How.many.hours.of.actual.sleep.do.you.get.at.night.[which(dataset$X4..How.many.hours.of.actual.sleep.do.you.get.at.night. == "7-8 hours")] <- 2
dataset$X4..How.many.hours.of.actual.sleep.do.you.get.at.night.[which(dataset$X4..How.many.hours.of.actual.sleep.do.you.get.at.night. == "More than 8 hours")] <- 3
dataset$X4..How.many.hours.of.actual.sleep.do.you.get.at.night.<- as.numeric(dataset$X4..How.many.hours.of.actual.sleep.do.you.get.at.night.)

#X5...How.many.hours.were.you.in.bed.
dataset$X5...How.many.hours.were.you.in.bed.[which(dataset$X5...How.many.hours.were.you.in.bed. == "Less than 5 hours")] <- 0
dataset$X5...How.many.hours.were.you.in.bed.[which(dataset$X5...How.many.hours.were.you.in.bed. == "5-7 hours")] <- 1
dataset$X5...How.many.hours.were.you.in.bed.[which(dataset$X5...How.many.hours.were.you.in.bed. == "8-10 hours")] <- 2
dataset$X5...How.many.hours.were.you.in.bed.[which(dataset$X5...How.many.hours.were.you.in.bed. == "More than 10 hours")] <- 3
dataset$X5...How.many.hours.were.you.in.bed.<- as.numeric(dataset$X5...How.many.hours.were.you.in.bed.)

for(i in c(63:75)){
  dataset[which(dataset[, i] == "Not during last month"), i] <- 0
  dataset[which(dataset[, i] == "Less then once a week"), i] <- 1
  dataset[which(dataset[, i] %in% c("Once or twice a week")), i] <- 2
  dataset[which(dataset[, i] == "Three or more in week"), i] <- 3
  dataset[i]<- as.numeric(dataset[,i])
}

#X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall.
dataset$X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall.[which(dataset$X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall. == "Very bad")] <- 0
dataset$X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall.[which(dataset$X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall. == "Farely bad")] <- 1
dataset$X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall.[which(dataset$X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall. == "Farely good")] <- 2
dataset$X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall.[which(dataset$X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall. == "Very good")] <- 3
dataset$X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall.<- as.numeric(dataset$X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall.)

dataset <- dataset %>%
  dplyr::select(-c(Timestamp, 
                   X1..Do.you.have.a.social.media.account...e.g...Facebook..Twitter..etc.., 
                   X3..Which.device.do.you.usually.use.to.connect.social.media.,
                   X4..Which.type.of.internet.connection.do.you.use.,
                   X8..When.do.you.usually.use.social.media.,
                   X11..How.many.groups.you.are.tagged.in.social.media.,
                   X14.Do.you.believe.social.media.is.a.good.thing.,
                   X10..How.many.friends.do.you.know.personally.in.social.media.))

trasformo_stringa<-function(stringa){
  s <- str_replace_all(tolower(stringa), " ", "")
  
  return(s)
}
library(stringr)
dataset <- dataset %>% 
  dplyr::rename("tipo.social.network" = X2..Which.social.media.account.do.you.use.usually.) %>% 
  dplyr::rename("da.quando.utilizzo.sn" = X5..How.long.have.you.been.using.a.social.media.account.) %>% 
  dplyr::rename("frequenza.creazione.contenuto.sn" =  X6..How.frequently.do.you.post..upload.status.or.add.photos.videos..on.social.media.) %>% 
  dplyr::rename("quanto.tempo.al.giorno.sn" =  X7..How.much.time.do.you.spend.daily.in.social.media.) %>% 
  dplyr::rename("quanti.amici.sn.si.conoscono" = X9..How.many.friends.do.you.have.on.social.media.) %>%
  dplyr::rename("perche.uso.sn" = X12..What.is.your.main.purpose.for.using.social.media..e.g..Facebook..) %>% 
  dplyr::rename("tipo.contenuto.visto.sn" = X13..What.contents.do.you.mainly.look.for.in.your.social.media.news.feed.) %>% 
  dplyr::rename("credo.a.quello.che.vedo" = X15..When.you.see.something.in.social.media..do.you.instantly.believe.it.) %>% 
  dplyr::rename("pressione.da.compagni" = X16..Have.you.ever.experienced.peer.pressure.due.to.social.media.) %>% 
  dplyr::rename("influenza.sulle.emozioni" = X17..Does.your.emotion.get.influenced.by.other.s.posts..success..failure..loss..) %>% 
  dplyr::rename("compararsi.con.vite.lussuose" = X18..Have.you.ever.compared.yourself.with.other.s.success.or.luxurious.life.) %>% 
  dplyr::rename("pensare.meglio.senza.sn" = X19..Do.you.think..your.mental.wellbeing.would.be.better.if.you.do.not.use.social.media.) %>% 
  dplyr::rename("ridurre.uso.sn" =  X20..If.answer.is.yes..are.you.trying.to.control.that.thing.and.trying.to.reduce.the.use.of.social.media.) %>% 
  dplyr::rename("eta" =  X21..Please.write.your.age.in.years..number..) %>% 
  dplyr::rename("genere" =  X22..Gender) %>% 
  dplyr::rename("stato.maritale" =  X23..Marital.Status) %>% 
  dplyr::rename("religione" =  X24..Religion) %>%
  dplyr::rename("livello.educazione" =  X25..Education) %>%
  dplyr::rename("professione" =  X26..Profession) %>%
  dplyr::rename("reddito" =  X27..Monthly.income) %>%
  dplyr::rename("area.di.residenza" =  X28..Area.of.residence) %>%
  dplyr::rename("vivere.con.famiglia" =  X29..Living.with.) %>%
  dplyr::rename("peso" = X30..Body.weight..Kg.) %>%
  dplyr::rename("altezza" = X31..Height..m.) %>%
  dplyr::rename("abitudine.al.fumo" = X32..Smoking.habit) %>%
  dplyr::rename("mancanza.compagnia" = X1..In.the.past.30.days..do.you.feel.lack.of.companionship.) %>%
  dplyr::rename("mancanza.confidente" = X2..In.the.past.30.days..there.is.no.one.I.can.turn.to) %>%
  dplyr::rename("sentirsi.escluso" = X3..In.the.past.30.days..I.feel.left.out.) %>%
  dplyr::rename("sentirsi.isolato" = X4..In.the.last.30.days..I.feel.isolated.from.others.) %>%
  dplyr::rename("triste.per.essere.scartato" = X5..In.the.last.30.days..I.am.unhappy.being.so.withdrawn.) %>%
  dplyr::rename("gente.attorno.ma.non.con.me" = X6..In.the.last.30.days..people.are.around.me.but.not.with.me.) %>%
  dplyr::rename("sono.estroverso" = X7..In.the.last.30.days..I.am.an.outgoing.person.) %>%
  dplyr::rename("ho.compagnia.quando.voglio" = X8..In.the.last.30.days..I.can.find.companionship.when.I.want.it.) %>%
  dplyr::rename("poco.interesse.nel.fare.cose" = X1.In.the.last.30.days..little.interest.or.pleasure.in.doing.things.) %>%
  dplyr::rename("sentirsi.depresso" = X2..In.the.last.30.days..feeling.down..depressed.or.hopeless.) %>%
  dplyr::rename("avere.troppo.sonno" = X3..In.the.last.30.days..trouble.falling.or.staying.asleep..sleeping.too.much) %>%
  dplyr::rename("sentirsi.stanco" = X4..In.the.last.30.days..Feeling.tired.or.having.little.energy.) %>%
  dplyr::rename("non.avere.fame.o.troppa" = X5..In.the.last.30.days..poor.appetite.or.over.eating.) %>%
  dplyr::rename("sentirsi.falliti" = X6..In.the.last.30.days..feeling.bad.about.yourself.or.that.you.are.a.failure.or.have.let.yourself.or.your.family.down.) %>%
  dplyr::rename("non.riuscire.a.concentrarsi" = X7..In.the.last.30.days..trouble.concentrating.on.things..such.as.reading.the.newspaper.or.watching.television.) %>%
  dplyr::rename("muoversi.troppo.o.troppo.poco" = X8..In.the.last.30.days..moving.or.speaking.so.slowly.or.the.opposite.moving.around.a.lot.more.than.usual.) %>%
  dplyr::rename("pensieri.suicidio" = X9..In.the.last.30.days..thoughts.that.you.would.be.better.off.dead..or.of.hurting.yourself.) %>%
  dplyr::rename("ansia" = X1..In.the.last.30.days..I.am.feeling.nervous..anxious..or.on.edge) %>%
  dplyr::rename("non.riuscire.a.non.sentirsi.preoccupato" = X2..In.the.last.30.days..I.an.not.being.able.to.stop.or.control.worrying) %>%
  dplyr::rename("preoccupato.troppo.per.diverse.cose" = X3..In.the.last.30.days..I.am.worrying.too.much.about.different.things.) %>%
  dplyr::rename("non.riuscire.a.rilassarsi" = X4..In.the.last.30.days..I.felt.trouble.in.relaxing.) %>%
  dplyr::rename("non.riuscire.a.stare.fermi.dallo.stress" = X5..In.the.last.30.days..I.am.being.so.restless.that.it.s.hard.to.sit.still) %>%
  dplyr::rename("essere.molto.irritabile" = X6..In.the.last.30.days..I.becoming.easily.annoyed.or.irritable.) %>%
  dplyr::rename("sentire.che.qualcosa.va.male" = X7..In.the.last.30.days..I.am.feeling.afraid.as.if.something.awful.might.happen.) %>%
  dplyr::rename("quando.vado.a.letto" = X1..When.I.am.usually.gone.to.bed.) %>%
  dplyr::rename("quanto.ci.metto.ad.addormentarmi" = X2..How.long..in.minutes..has.it.taken.you.to.fall.asleep.each.night.) %>%
  dplyr::rename("quando.mi.alzo.mattina" = X3..When.have.you.usually.gotten.up.in.the.morning.) %>%
  dplyr::rename("quante.ore.dormo" = X4..How.many.hours.of.actual.sleep.do.you.get.at.night.) %>%
  dplyr::rename("quante.ore.sto.a.letto" = X5...How.many.hours.were.you.in.bed.) %>%
  dplyr::rename("non.mi.addormento.in.30.minuti" = X6..In.last.30.days..How.many.times..I.cannot.get.to.sleep.within.30.minutes.) %>%
  dplyr::rename("mi.sveglio.nel.mezzo.della.notte" = X7..In.last.30.days..How.many.times..I.wake.up.in.the.middle.of.the.night.or.early.morning.) %>%
  dplyr::rename("alzarsi.notte.per.bagno" = X8..In.last.30.days..How.many.times..I.had.to.get.up.to.use.the.bathroom.) %>%
  dplyr::rename("respiro.affannato" = X9..In.last.30.days..How.many.times..I.cannot.breathe.comfortably.) %>%
  dplyr::rename("tossire.forte" = X10..In.last.30.days..How.many.times..I.cough.or.snore.loudly.) %>%
  dplyr::rename("sentire.troppo.freddo" = X11..In.last.30.days..How.many.times..I.feel.too.cold.) %>%
  dplyr::rename("sentire.troppo.caldo" = X12..In.last.30.days..How.many.times..I.feel.too.hot.) %>%
  dplyr::rename("incubi" = X13..In.last.30.days..How.many.times..I.saw.bad.dreams.) %>%
  dplyr::rename("dolore.mentre.dormo" = X14..In.last.30.days..How.many.times..I.have.pain.during.sleep.) %>%
  dplyr::rename("non.riuscire.dormire.per.altre.ragioni" = X15..In.last.30.days..How.many.times..I.having.trouble.sleeping.for.any.other.reason.) %>%
  dplyr::rename("sonniferi" = X16..In.last.30.days..In.last.month..have.you.take.medicines.for.sleep.) %>%
  dplyr::rename("non.dormire.per.percorsi.cause.importanti" = X17..In.last.month..how.many.times.you.cannot.sleep.due.to.any.program.or.other.important.case.) %>%
  dplyr::rename("problemi.frequentare.percorsi.cause.importanti" = X18..In.last.month..how.many.times.you.face.problems.to.maintain.program.or.other.important.case.) %>%
  dplyr::rename("qualita.sonno" = X19..During.the.past.month..how.would.you.rate.your.sleep.quality.overall.) %>%
  dplyr::filter(!is.na(livello.educazione)) %>%
  dplyr::mutate(professione = trasformo_stringa(professione))
  
  

library(fastDummies)

lista <- c("perche.uso.sn", 
           "tipo.contenuto.visto.sn", 
           "credo.a.quello.che.vedo", 
           "pressione.da.compagni",
           "pensare.meglio.senza.sn",
           "genere",
           "stato.maritale",
           "religione",
           "area.di.residenza",
           "vivere.con.famiglia",
           "abitudine.al.fumo",
           "professione",
           "tipo.social.network")
dataset <- dummy_cols(dataset, select_columns = lista, remove_selected_columns = FALSE)

dataset$`tipo.contenuto.visto.sn_Beautiful girls Lady`[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1)] <- 1
dataset$tipo.contenuto.visto.sn_Business[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1)] <- 1
dataset$tipo.contenuto.visto.sn_Ducks[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1)] <- 1
dataset$`tipo.contenuto.visto.sn_Entertainment `[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 11)] <- 1
dataset$`tipo.contenuto.visto.sn_For higher studies`[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1)] <- 1
dataset$`tipo.contenuto.visto.sn_For random contents not specified `[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1 | dataset$`tipo.contenuto.visto.sn_Nothing specific ` == 1)] <- 1
dataset$`tipo.contenuto.visto.sn_Get pleasure from funny posts/ memes`[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1 | dataset$`tipo.contenuto.visto.sn_To Stay Connected, get updates from people and get pleasure from memes` == 1)] <- 1
dataset$`tipo.contenuto.visto.sn_Islamic content`[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1)] <- 1
dataset$`tipo.contenuto.visto.sn_Job circular`[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1 | dataset$`tipo.contenuto.visto.sn_To get updates of job circular, to learn from job related post/experience` == 1)] <- 1
dataset$`tipo.contenuto.visto.sn_Motivational/ informative contents`[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1)] <- 1
dataset$`tipo.contenuto.visto.sn_To buy products from various online pages/groups `[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1)] <- 1
dataset$`tipo.contenuto.visto.sn_To stay connected with people`[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1 | dataset$`tipo.contenuto.visto.sn_Both update of current affairs and stay connected with people` == 1 | dataset$`tipo.contenuto.visto.sn_To Stay Connected, get updates from people and get pleasure from memes` == 1)] <- 1
dataset$`tipo.contenuto.visto.sn_Updates of current affairs/news`[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1 | dataset$`tipo.contenuto.visto.sn_Both update of current affairs and stay connected with people` == 1)] <- 1
dataset$`tipo.contenuto.visto.sn_Updates of people`[which(dataset$tipo.contenuto.visto.sn_All.of.the.above == 1 | dataset$tipo.contenuto.visto.sn_All.of.them == 1 | dataset$`tipo.contenuto.visto.sn_To Stay Connected, get updates from people and get pleasure from memes` == 1)] <- 1

dataset$professione_housewife <- dataset$professione_housewife + dataset$professione_hhousewife
dataset$professione_doctor <- dataset$professione_doctor + dataset$professione_interndoctor
dataset$professione_dentist <- dataset$professione_dentist + dataset$professione_dentalsurgeon
dataset$professione_privatejob <- dataset$professione_privatejob + dataset$professione_privatejobandselftaughtmakeupartist
dataset$religione_Christian <- dataset$religione_Christian + dataset$religione_Christan + dataset$religione_Christianity
dataset$religione_Agnostic <- dataset$religione_Agnostic + dataset$`religione_Agnostic `
dataset$religione_None <- dataset$religione_None + dataset$`religione_Not religious `
dataset <- dataset %>% dplyr::select(-c(professione_hhousewife, 
                                        professione_interndoctor, 
                                        `religione_Not religious `, 
                                        religione_Christan, 
                                        religione_Christianity, 
                                        `religione_Agnostic `,
                                        professione_privatejobandselftaughtmakeupartist,
                                        professione_dentalsurgeon,
                                        pensare.meglio.senza.sn_Yes,
                                        genere_Male,
                                        `area.di.residenza_Rural Area`,
                                        `vivere.con.famiglia_Without family`,
                                        `abitudine.al.fumo_Non-smoker`,
                                        tipo.social.network_Messaggistica,
                                        pressione.da.compagni_No,
                                        credo.a.quello.che.vedo_No,
                                        `tipo.contenuto.visto.sn_Both update of current affairs and stay connected with people`,
                                        `tipo.contenuto.visto.sn_Nothing specific `,
                                        `tipo.contenuto.visto.sn_To get updates of job circular, to learn from job related post/experience`,
                                        `tipo.contenuto.visto.sn_To Stay Connected, get updates from people and get pleasure from memes`,
                                        `tipo.contenuto.visto.sn_All of the above`,
                                        `tipo.contenuto.visto.sn_All of them`))

dataset["malessere"] <- dataset$mancanza.compagnia + 
  dataset$mancanza.confidente + 
  dataset$sentirsi.escluso + 
  dataset$sentirsi.isolato + 
  dataset$triste.per.essere.scartato + 
  dataset$gente.attorno.ma.non.con.me -
  dataset$sono.estroverso -
  dataset$ho.compagnia.quando.voglio +
  dataset$poco.interesse.nel.fare.cose + 
  dataset$sentirsi.depresso + 
  dataset$avere.troppo.sonno + 
  dataset$sentirsi.stanco + 
  dataset$non.avere.fame.o.troppa + 
  dataset$sentirsi.falliti + 
  dataset$non.riuscire.a.concentrarsi + 
  dataset$muoversi.troppo.o.troppo.poco + 
  dataset$pensieri.suicidio + 
  dataset$ansia + 
  dataset$non.riuscire.a.non.sentirsi.preoccupato +
  dataset$preoccupato.troppo.per.diverse.cose +
  dataset$non.riuscire.a.rilassarsi +
  dataset$non.riuscire.a.stare.fermi.dallo.stress +
  dataset$essere.molto.irritabile + 
  dataset$sentire.che.qualcosa.va.male +
  dataset$non.mi.addormento.in.30.minuti + 
  dataset$mi.sveglio.nel.mezzo.della.notte +
  dataset$alzarsi.notte.per.bagno +
  dataset$respiro.affannato + 
  dataset$tossire.forte +
  dataset$sentire.troppo.freddo + 
  dataset$sentire.troppo.caldo +
  dataset$incubi +
  dataset$dolore.mentre.dormo +
  dataset$non.riuscire.dormire.per.altre.ragioni -
  dataset$qualita.sonno +
  dataset$sonniferi + 
  dataset$non.dormire.per.percorsi.cause.importanti +
  dataset$problemi.frequentare.percorsi.cause.importanti

dataset["pressione.sn"] <- dataset$influenza.sulle.emozioni + dataset$compararsi.con.vite.lussuose

dataset <- dataset %>% dplyr::select(-c(mancanza.compagnia, 
                                mancanza.confidente,
                                sentirsi.escluso, 
                                sentirsi.isolato,
                                triste.per.essere.scartato,
                                gente.attorno.ma.non.con.me,
                                sono.estroverso,
                                ho.compagnia.quando.voglio,
                                poco.interesse.nel.fare.cose ,
                                sentirsi.depresso , 
                                avere.troppo.sonno , 
                                sentirsi.stanco ,
                                non.avere.fame.o.troppa ,
                                sentirsi.falliti ,
                                non.riuscire.a.concentrarsi ,
                                muoversi.troppo.o.troppo.poco ,
                                pensieri.suicidio ,
                                ansia ,
                                non.riuscire.a.non.sentirsi.preoccupato ,
                                preoccupato.troppo.per.diverse.cose ,
                                non.riuscire.a.rilassarsi ,
                                non.riuscire.a.stare.fermi.dallo.stress ,
                                essere.molto.irritabile , 
                                sentire.che.qualcosa.va.male ,
                                non.mi.addormento.in.30.minuti ,
                                mi.sveglio.nel.mezzo.della.notte,
                                alzarsi.notte.per.bagno ,
                                respiro.affannato , 
                                tossire.forte ,
                                sentire.troppo.freddo ,
                                sentire.troppo.caldo,
                                incubi ,
                                dolore.mentre.dormo ,
                                non.riuscire.dormire.per.altre.ragioni ,
                                qualita.sonno,
                                influenza.sulle.emozioni,
                                compararsi.con.vite.lussuose,
                                quando.vado.a.letto,
                                quanto.ci.metto.ad.addormentarmi,
                                quando.mi.alzo.mattina,
                                quante.ore.dormo,
                                quante.ore.sto.a.letto,
                                sonniferi,
                                non.dormire.per.percorsi.cause.importanti,
                                problemi.frequentare.percorsi.cause.importanti))
dataset$altezza <- as.numeric(str_replace_all(dataset$altezza, ",", "."))
dataset$peso <- as.numeric(str_replace_all(dataset$peso, ",", "."))
dataset$eta <- as.numeric(str_replace_all(dataset$eta, ",", "."))

write.csv(dataset, "/Users/lizzy/Desktop/Universita/tirocinio/github/DS4CitizensLab/dataset.csv")
