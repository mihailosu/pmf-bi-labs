
# Generisemo nasumicni uzorak iz normalne distribucije

# Postavljamo pocetnu vrednost za generator
# nasumicnih brojeva
set.seed(42)

sample_normal <- rnorm(1000, mean = 10, sd = 2)

# Crtamo histogram prethodno odabranih 1000 vrednosti
hist(sample_normal, breaks = 30, freq = FALSE, main = "Sample from Normal Distribution",
     xlab = "Value", ylab = "Density", col = "lightblue")

# Preko histograma crtamo funkciju gustine verovatnoce
# normalne distribucije
curve(dnorm(x, mean = 10, sd = 2), add = TRUE, col = "darkblue", lwd = 2)


# Generisemo nasumicne vrednosti iz eksponencijalne distribucije
set.seed(42)
sample_exponential <- rexp(1000, rate = 0.1)

# Iscrtamo histogram vrednosti
hist(sample_exponential, breaks = 30, freq = FALSE, main = "Sample from Exponential Distribution",
     xlab = "Value", ylab = "Density", col = "lightgreen")

# Preko histograma crtamo krivu gustine verovatnoce
curve(dexp(x, rate = 0.1), add = TRUE, col = "darkgreen", lwd = 2)


# Generisemo uniformne vrednosti
sample_uniform <- runif(1000, min = 0, max = 1)

# Crtamo histogram...
hist(sample_uniform, breaks = 30, freq = FALSE, main = "Sample from Uniform Distribution",
     xlab = "Value", ylab = "Density", col = "lightcoral")

# ...i gustinu verovatnoce
curve(dunif(x, min = 0, max = 1), add = TRUE, col = "darkred", lwd = 2)


# Prelazimo na statisticke testove

# Korelacija

# Ucitamo mtcars skup
data(mtcars)

View(mtcars)

# Racunamo Pearson-ovu korelaciju izmedju mpg (miles per gallon) i wt (weight)
correlation <- cor.test(mtcars$mpg, mtcars$wt, method = "pearson")

# Ispisujemo rezultat testa na ekran
print(correlation)

if (correlation$p.value < 0.05) {
  cat("There is a significant correlation between mpg and wt (p < 0.05).\n")
} else {
  cat("There is no significant correlation between mpg and wt (p ≥ 0.05).\n")
}

# Upareni t-test

# Ucitamo skup podataka
data(PlantGrowth)

# ...prikazemo ga
View(PlantGrowth)

group1 <- PlantGrowth$weight[PlantGrowth$group == "ctrl"]
group2 <- PlantGrowth$weight[PlantGrowth$group == "trt2"]

# Uporedimo 2 grupe t-testom
paired_t_test <- t.test(group1, group2, paired = TRUE)

print(paired_t_test)

if (paired_t_test$p.value < 0.05) {
  cat("There is a significant difference in weights before and after treatment (p < 0.05).\n")
} else {
  cat("There is no significant difference in weights before and after treatment (p ≥ 0.05).\n")
}

# hi-kvadrat test

# Ucitavamo swiss skup podataka
data(swiss)

View(swiss)

# Od numerickih kolona dobijemo kategoricke
swiss$Fertility_Category <- cut(swiss$Fertility, breaks = 3, labels = c("low", "medium", "high"))
swiss$Catholic_Category <- cut(swiss$Catholic, breaks = 3, labels = c("low", "medium", "high"))

cont_table = table(swiss$Fertility_Category, swiss$Catholic_Category)

# Prosledimo matricu u hi-kvadrat test
chi_squared_test <- chisq.test(cont_table)

print(chi_squared_test)

if (chi_squared_test$p.value < 0.05) {
  cat("There is a significant association between fertility rate and Catholic percentage (p < 0.05).\n")
} else {
  cat("There is no significant association between fertility rate and Catholic percentage (p ≥ 0.05).\n")
}

# Fisher-ov test

# Load mtcars dataset
data(mtcars)

View(mtcars)

# Pretvaramo kolone u boolean vrednosti (da/ne)
mtcars$HighMPG <- ifelse(mtcars$mpg >= median(mtcars$mpg), TRUE, FALSE)
mtcars$ManualTransmission <- ifelse(mtcars$am == 1, TRUE, FALSE)

# Pokrecemo Fisher-ov test 
fisher_test <- fisher.test(table(mtcars$HighMPG, mtcars$ManualTransmission))

print(fisher_test)

if (fisher_test$p.value < 0.05) {
  cat("There is a significant association between high MPG and manual transmission (p < 0.05).\n")
} else {
  cat("There is no significant association between high MPG and manual transmission (p ≥ 0.05).\n")
}

