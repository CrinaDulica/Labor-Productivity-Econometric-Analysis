
# Pachete necesare
library(readxl)
library(lmtest)
library(tseries)
library(car)

# =========================================
# IMPORT DATE
# =========================================

date <- read_excel(file.choose())

head(date)
str(date)
summary(date)

# =========================================
# 3. MODEL UNIFACTORIAL
# PIB_ora ~ internet
# =========================================

model_uni <- lm(pib_ora ~ internet, data = date)

summary(model_uni)

# =========================================
# Scatterplot
# =========================================

plot(date$internet,
     date$pib_ora,
     main = "Diagrama de imprastiere: PIB per ora lucrata si utilizarea internetului",
     xlab = "Utilizarea internetului (%)",
     ylab = "PIB per ora lucrata",
     col = "blue",
     pch = 16)

abline(model_uni,
       col = "red",
       lwd = 2)

# =========================================
# 3c - Validitatea modelului
# =========================================

summary(model_uni)

# =========================================
# 3d - Test t + interval de incredere
# =========================================

summary(model_uni)

confint(model_uni,
        level = 0.95)

# =========================================
# 3e - Test unilateral dreapta
# =========================================

coef(summary(model_uni))

t_calculat <- coef(summary(model_uni))[2,3]

grade_libertate <- model_uni$df.residual

p_unilateral <- pt(t_calculat,
                   df = grade_libertate,
                   lower.tail = FALSE)

p_unilateral

t_critic <- qt(0.95,
               df = grade_libertate)

t_critic

# =========================================
# 3f1 - HOMOSCEDASTICITATE
# =========================================

# Test Breusch-Pagan
bptest(model_uni)

# Test White
bptest(model_uni,
       ~ internet + I(internet^2),
       data = date)

# Test Glejser
glejser_model <- lm(abs(residuals(model_uni)) ~ internet,
                    data = date)

summary(glejser_model)

# =========================================
# 3f2 - AUTOCORELARE
# =========================================

# Durbin Watson
dwtest(model_uni)

# Breusch Godfrey
bgtest(model_uni,
       order = 2)

# =========================================
# 3f3 - NORMALITATE
# =========================================

# Jarque Bera
jarque.bera.test(residuals(model_uni))

# Shapiro Wilk
shapiro.test(residuals(model_uni))

# QQ Plot
qqnorm(residuals(model_uni))
qqline(residuals(model_uni),
       col = "red")

# =========================================
# 3g - CORECTAREA MODELULUI
# =========================================

# Transformare logaritmica

date$log_pib <- log(date$pib_ora)
date$log_internet <- log(date$internet)

model_log <- lm(log_pib ~ log_internet,
                data = date)

summary(model_log)

# =========================================
# Eliminare outliere
# =========================================

boxplot(date$pib_ora,
        main = "Boxplot PIB per ora lucrata")

date_clean <- date[date$pib_ora < 200000, ]

date_clean$log_pib <- log(date_clean$pib_ora)
date_clean$log_internet <- log(date_clean$internet)

model_log2 <- lm(log_pib ~ log_internet,
                 data = date_clean)

summary(model_log2)

# =========================================
# Reverificare ipoteze
# =========================================

jarque.bera.test(residuals(model_log2))

shapiro.test(residuals(model_log2))

dwtest(model_log2)

bgtest(model_log2,
       order = 2)

qqnorm(residuals(model_log2))
qqline(residuals(model_log2),
       col = "red")

# =========================================
# 3h - PREVIZIUNE
# =========================================

ultima_val <- tail(date_clean$internet, 1)

ultima_val

val_noua <- ultima_val * 1.10

val_noua

val_noua_log <- log(val_noua)

# Interval predictie
predict(model_log2,
        newdata = data.frame(log_internet = val_noua_log),
        interval = "prediction",
        level = 0.95)

# Interval incredere
predict(model_log2,
        newdata = data.frame(log_internet = val_noua_log),
        interval = "confidence",
        level = 0.95)

# =========================================
# 4. MODEL MULTIFACTORIAL
# =========================================

model_multi <- lm(pib_ora ~ rd + internet + educatie,
                  data = date)

summary(model_multi)

# =========================================
# 4b - MULTICOLINIARITATE
# =========================================

# Matrice corelatii

cor(date[, c("rd",
             "internet",
             "educatie")],
    use = "complete.obs")

# VIF
vif(model_multi)

# =========================================
# 4c - MODEL REDUS
# =========================================

model_redus <- lm(pib_ora ~ rd + educatie,
                  data = date)

summary(model_redus)

# =========================================
# 4d - IPOTEZE MODEL MULTIFACTORIAL
# =========================================

# =========================================
# HOMOSCEDASTICITATE
# =========================================

# Breusch Pagan
bptest(model_redus)

# White
bptest(model_redus,
       ~ rd + educatie +
         I(rd^2) +
         I(educatie^2),
       data = date)

# =========================================
# AUTOCORELARE
# =========================================

dwtest(model_redus)

bgtest(model_redus,
       order = 2)

# =========================================
# NORMALITATE
# =========================================

jarque.bera.test(residuals(model_redus))

shapiro.test(residuals(model_redus))

qqnorm(residuals(model_redus))
qqline(residuals(model_redus),
       col = "red")

# =========================================
# CORECTARE MODEL
# =========================================

date$log_rd <- log(date$rd + 0.01)
date$log_educatie <- log(date$educatie)

model_multi_log <- lm(log_pib ~ log_rd + log_educatie,
                      data = date_clean)

summary(model_multi_log)

jarque.bera.test(residuals(model_multi_log))

shapiro.test(residuals(model_multi_log))

qqnorm(residuals(model_multi_log))
qqline(residuals(model_multi_log),
       col = "red")

# =========================================
# 4e - TERMEN DE INTERACTIUNE
# =========================================

model_interactie <- lm(pib_ora ~ rd*educatie,
                       data = date)

summary(model_interactie)

# =========================================
# INTERVAL INCREDERE COEFICIENTI
# =========================================

confint(model_interactie,
        level = 0.95)


install.packages("lmtest")
library(lmtest)
install.packages("readxl")
install.packages("lmtest")
install.packages("tseries")
install.packages("car")

library(readxl)
library(lmtest)
library(tseries)
library(car)
library(readxl)
library(lmtest)
library(tseries)
library(car)
bptest(model_redus)

date_clean <- date[date$pib_ora < 200000, ]
date_clean$log_pib <- log(date_clean$pib_ora)

date_clean$log_internet <- log(date_clean$internet)
model_log2 <- lm(log_pib ~ log_internet,
                 data = date_clean)
