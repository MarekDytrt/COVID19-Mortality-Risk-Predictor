       ########## INSTALACE POTŘEBNÝCH BALÍČKŮ ##########

library(readr) 
library(dplyr)
library(margins)
library(caret)
library(ROCR)
library(pROC)
library(caret)
library(car)
library(tidyverse)
library(scales)


         ########## NACTENI A UPRAVA DAT PRO LOGIT ##########

#odkaz na data: https://www.kaggle.com/datasets/meirnizri/covid19-dataset/data               
                                           
# načtení zdrojových dat z csv formátu
df <- read_csv("CovidData.csv")

# zobrazení proměnných a jejich několika prvních hodnot
glimpse(df)

# Zdrojová data jsou mají 21 proměnných a 1 048 575 pozorování. 
# Proměnné nabývající binárních hodnot mají značení 2 pro NE a 1 pro ANO.
# U binárních proměnných se mohou vyskytovat také hodnoty se speciální významem:
#   97 - „Not applicable“ / Neaplikovatelné
#   98 – „Unknown“ / Neznámé
#   99 – „Not specified“ / Neuvedeno
#
#     -USMER...hodnoty 1, 2 nebo 3 indikující úroveň zdravotnického zařízení
#     -MEDICAL_UNIT...celočíslené hondoty <1,13>; typ instituce, kde byl pacient léčen
#     -SEX...1 pokud je pacient žena, 2 pokud je muž
#     -PATIENT_TYPE...1 pokud byl se pacient léčil doma, 2 pokud byl hospitalizován
#     -DATE_DIED...datum smrti pacinta; 999-99-99 pokud nezemřel
#     -INTUBED...1 pokud byl pacient napojen na ventilátor; 2 pokud nebyl
#     -AGE...vek pacienta
#     -PREGNANT...1 pokud je pacient(ka) těhotná, 2 pokud ne
#     -PNEUMONIA, DIABETES, COPD, ASTHMA, INMSUPR, HIPERTENSION, OTHER_DIESEASE
#     CARDIOVASCULAR, OBESITY, RENAL_CHRONIC, TOBACCO...1 pokud pacient má danou nemoc, 2 pokud ne
#     -ICU...1 pokud byl pacient lecen na JIP, 2 pokud nebyl
#     -CLASIFFICATION_FINAL...celočíslné hodnoty v rozsahu <1,7>. 
#                             Hodnoty 1 až 3 označují pacietny s pozitivním COVID testem, vyšší hodnoty
#                             znamenají negativní nebo nerozhodný test
     

# vytvoření nového datového souboru s upravenými daty
df_final <- df %>%

  # Protože pro tuto analýzu jsou potřební pouze COVID-pozitivní 
  # pacienti, pozorování s hodnotou větší než 3 smažeme
  filter(CLASIFFICATION_FINAL < 4) %>%
  
  # přejmenovaní SEX na FEMALE pro jednoznačnou interpretaci významu proměnné
  rename(FEMALE = SEX) %>%
  
  # přepsání hodnot větších než 97 na NA u nemocí
  mutate(across(c(FEMALE, PNEUMONIA, DIABETES, COPD, ASTHMA, INMSUPR, 
                  HIPERTENSION, OTHER_DISEASE, OBESITY, RENAL_CHRONIC, 
                  TOBACCO, CARDIOVASCULAR),
                ~ ifelse(. >= 97, NA, .))) %>%

 
  # převod 1/2 na 1/0 (aby 1=Ano, 0=Ne)
  mutate(across(c(FEMALE, PNEUMONIA, DIABETES, COPD, ASTHMA, INMSUPR, 
                  HIPERTENSION, OTHER_DISEASE, OBESITY, RENAL_CHRONIC, 
                  TOBACCO, CARDIOVASCULAR),
                ~ ifelse(. == 1, 1, 0))) %>%
  
  # nova umělá proměnná pro zemřelé pacienty
  mutate(DEAD = ifelse(DATE_DIED == "9999-99-99", 0, 1)) %>%

  # smazání dále nepoužívaných proměnných
  select(-USMER, -PREGNANT, -MEDICAL_UNIT, -DATE_DIED,-INTUBED, -ICU,
         -CLASIFFICATION_FINAL, -PATIENT_TYPE)

glimpse(df_final)
# s těmito daty budeme dále pracovat, po úpravách máme 
# 391 979 pozorování a 14 proměnných 


# protože řádků, kde je alespoň jednou NA, je v modelu malé množství 
# vzhledem k velikosti dat, všechny řádky s NA smažeme. Usnadní to
# pozdější práci s daty.
print(paste("Počet řádků s NA:", sum(!complete.cases(df_final))))
df_final <- na.omit(df_final)



        ########## GRAFY A POPISNÉ STATISTIKY ##########

# sloupcový graf
diseases_mortality <- df_final %>%
  select(DEAD, PNEUMONIA, DIABETES, COPD, ASTHMA, INMSUPR,
         HIPERTENSION, CARDIOVASCULAR, OBESITY, RENAL_CHRONIC, 
         TOBACCO, OTHER_DISEASE) %>%
  
  pivot_longer(
    cols = -DEAD,        
    names_to = "Disease",
    values_to = "Has_Disease"
  ) %>%
  
  filter(Has_Disease == 1) %>%
  mutate(Status = ifelse(DEAD == 1, "zemřel", "přežil"))

ggplot(diseases_mortality, aes(x = Disease, fill = Status)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("zemřel" = "#e41a1c", "přežil" = "#377eb8")) + 
  labs(
    title = "Počet pacientů s konkrétními nemocemi",
    x = NULL,
    y = NULL,
    fill = NULL  
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
# průměr a median poctu nemoci u pacientů

diseases_list <- c("PNEUMONIA", "DIABETES", "COPD", "ASTHMA", "INMSUPR",
                    "HIPERTENSION", "CARDIOVASCULAR", "OBESITY", 
                    "RENAL_CHRONIC", "TOBACCO", "OTHER_DISEASE")

diseases_count <- df_final %>%
  mutate(Pocet_nemoci = rowSums(select(., all_of(diseases_list)) == 1)) %>%
  group_by(DEAD) %>%
  summarise(
    Prumer = mean(Pocet_nemoci),
    Median = median(Pocet_nemoci), 
    Pocet_pacientu = n() 
  )
print(diseases_count)


# průměr a medán věku zemřelých a přeživších
age_stats <- df_final %>%
  group_by(DEAD) %>%
  summarise(
    Prumer_vek = mean(AGE),  
    Median_vek = median(AGE)
  )
print(age_stats)

# smrtnost podle pohlavi
gender_stats <- df_final %>%
  group_by(FEMALE) %>%
  summarise(
    Celkem_pacientu = n(),
    Zemrelo = sum(DEAD == 1)
  )

print(gender_stats)


# rozdělení na trénovací(90%) a testovací data
set.seed(123)

trainIndex <- createDataPartition(df_final$DEAD, p = 0.9, list = FALSE, times = 1)

data_Train <- df_final[ trainIndex,]
data_Test  <- df_final[-trainIndex,]

print(paste("Počet pozorování pro trénink:", nrow(data_Train)))
print(paste("Počet pozorování pro testování:", nrow(data_Test)))


          ########## ODHAD MODELU ##########

# logit s proměnnou PNEUMONIA
logit1 <- glm(DEAD ~ AGE + PNEUMONIA + FEMALE + DIABETES + COPD + ASTHMA 
                 + INMSUPR + HIPERTENSION + CARDIOVASCULAR + RENAL_CHRONIC
                 + OBESITY + TOBACCO + OTHER_DISEASE,
                 data = data_Train,
                 family = binomial(link="logit"))

summary(logit1)
odds1 <- exp(coefficients(logit1))
print(odds1[-1])
summary(margins(logit1))
vif(logit1)

# logit bez proměnné PNEUMONIA
logit2 <- glm(DEAD ~ AGE + FEMALE + DIABETES + COPD + ASTHMA 
                + INMSUPR + HIPERTENSION + CARDIOVASCULAR + RENAL_CHRONIC
                + OBESITY + TOBACCO + OTHER_DISEASE,
               data = data_Train,
               family = binomial(link="logit"))
summary(logit2)

odds2 <- exp(coefficients(logit2))
print(odds2[-1])
summary(margins(logit2))
vif(logit2)



    ########## VLASTNOSTI MODELU NA TRÉNOVACÍCH DATECH#########

pom_model <- logit2
pom_y <- data_Train$DEAD
cutoff <- 0.5
actual <- pom_y==1
predicted <- fitted(pom_model) > cutoff
tabulka <- table(actual, predicted)
print(tabulka)

TP <- tabulka[2,2] 
TN <- tabulka[1,1] 
FP <- tabulka[1,2] 
FN <- tabulka[2,1]

print(paste("Přesnost s 0.5 cutoff:", (TP + TN) / sum(tabulka)))
print(paste("Sensitivita s 0.5 cutoff:", TP / (TP + FN)))

pom_P1 <- fitted(pom_model)
rocobj <- roc(pom_y, pom_P1)
rocobj <- roc(pom_y, pom_P1)

auc <- auc(pom_y, pom_P1)
gini <- 2 * auc - 1

print(paste("AUC na trénovacích datech:", as.numeric(auc)))
print(paste("Gini na trénovacích datech:", gini))



pred <- ROCR::prediction(fitted(pom_model),pom_y)
par(mfrow = c(1, 3))

# vykresleni predikcni presnosti v zavislosti na cutoff hranici
plot(performance(pred,"acc"), main = "Přesnost vs cutoff",)

# vykresleni ROC krivky  
plot(performance(pred,"tpr","fpr"), main = "ROC křivka (Trénink)")
abline(0,1,lty=2) 


# určení optimální cutoff hranice z grafu
cutoff_optimized <- 0.2

# Určení přesnosti a senzistivity s novým cutoff
pom_model2 <- logit2
pom_y <- data_Train$DEAD
actual <- pom_y==1
predicted <- fitted(pom_model) > cutoff_optimized
tabulka <- table(actual, predicted)
print(tabulka)

TP <- tabulka[2,2] 
TN <- tabulka[1,1] 
FP <- tabulka[1,2] 
FN <- tabulka[2,1]

print(paste("Přesnost s 0.2 cutoff:", (TP + TN) / sum(tabulka)))
print(paste("Sensitivita s 0.2 cutoff:", TP / (TP + FN)))


    ########## VLASTNOSTI MODELU NA TESTOVACÍCH DATECH #########


pred_probs_test <- predict(logit2, newdata = data_Test, type = "response")
pred_class_test <- pred_probs_test > cutoff_optimized

# porovnáváme predikci vs skutečnost v testovacích datech
tabulka <- table(Actual = data_Test$DEAD, Predicted = pred_class_test)
print(tabulka)

TP <- tabulka[2,2] 
TN <- tabulka[1,1] 
FP <- tabulka[1,2] 
FN <- tabulka[2,1]

print(paste("Přesnost v testu:", (TP + TN) / sum(tabulka)))
print(paste("Sensitivita v testu:", TP / (TP + FN)))


pom_P1_test <- predict(logit2, newdata = data_Test, type = "response")
pom_y_test <- data_Test$DEAD
rocobj_test <- roc(pom_y_test, pom_P1_test)

auc_test <- auc(rocobj_test)
print(paste("AUC na testovacích datech:", auc_test))

gini_test <- 2 * auc_test - 1
print(paste("Gini na testovacích datech:", gini_test))

# ROC křivka pro testovací data
pred_obj <- ROCR::prediction(pom_P1_test, pom_y_test)
plot(performance(pred_obj, "tpr", "fpr"), main = "ROC křivka (Test)")
abline(0, 1, lty = 2) 


  
  
