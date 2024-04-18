
# Pregled standardnih skupova podataka, pogodnih za eksperimentisanje sa
# funkcijama za deskriptivnu i inferencijsku statistiku
library(help="datasets")

# Ukljucimo datasets paket u projekat
library(datasets)

# Brz pregled "iris" skupa podataka
head(iris)

summary(iris)

# Brz pregled skupa podataka "volcano", koji predstavlja topografsku kartu
# vulkana na Novom Zelandu

print(volcano)

# Isctavanje 2D topografske karte vulkana

z <- volcano # Z-osa, tj. visine
x <- 1:nrow(z) # X-osa predstavlja broj redova u skupu podataka
y <- 1:ncol(z) # Y-osa je broj kolona u skupu

image(x,y,z,main="Mt Eden") # Iscrtavanje "heat map" grafikona

contour(x,y,z,add=T) # Dodavanje kontura

# Iscrtavanje 3D topografske karte istog vulkana

library(lattice)

wireframe(volcano, shade=TRUE, light.source=c(10, 0, 10))

# Ucitamo skup podataka koji opisuje odnos rasta biljaka (tezina)
# u odnosu na razlicite tretmane

data(PlantGrowth)

print(PlantGrowth)

# Crtamo tzv. "Violin plot"

ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of Plant Weights by Fertilizer Treatment",
       x = "Fertilizer Treatment", y = "Plant Weight") +
  theme_minimal()


# Ukrstanje kolona Titanic skupa podataka

class(iris)
mode(iris)

# Pregled skupa

data(iris)

# Izvlacimo numericke kolone iz skupa
iris_numeric <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]

# Kreiramo tackase grafikone
pairs(iris_numeric, 
      main = "Scatterplot Matrix - Iris Dataset", 
      pch = 16, 
      col = iris$Species)

# Box plot-ovi numerickih kolona

# Kreiranje box plot-ova svih numerickih varijabli, u odnosu na vrstu cveta
par(mfrow = c(2, 2)) # Podesimo matricu 2x2 gde smestamo grafikone
boxplot(Sepal.Length ~ Species, 
        data = iris, main = "Sepal Length by Species", xlab = "Species", ylab = "Sepal Length")
boxplot(Sepal.Width ~ Species, 
        data = iris, main = "Sepal Width by Species", xlab = "Species", ylab = "Sepal Width")
boxplot(Petal.Length ~ Species, 
        data = iris, main = "Petal Length by Species", xlab = "Species", ylab = "Petal Length")
boxplot(Petal.Width ~ Species, 
        data = iris, main = "Petal Width by Species", xlab = "Species", ylab = "Petal Width")

# Iscrtavanje tackastog grafikona u prostoru uz pomoc
# dodatne biblioteke
library(scatterplot3d)

scatterplot3d(x = iris$Sepal.Length, y = iris$Sepal.Width, z = iris$Petal.Length, 
              color = as.numeric(iris$Species),
              main = "3D Scatter Plot - Iris Dataset",
              xlab = "Sepal Length", ylab = "Sepal Width", zlab = "Petal Length",
              pch = 16, angle = 30)
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 16, bty = "n", title = "Species")


# Pravimo heatmap matrice korelacije svih numerickih kolona

# Ponovo izvlacimo numericke vrednosti iz skupa podataka
iris_numeric <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]

# Racunamo matricu korelacije
correlation_matrix <- cor(iris_numeric)

print(correlation_matrix)

# Ukljucujemo pakete 
library(ggplot2)  # Biblioteka za napredno crtanje
library(reshape2) # Dodatne funkcije za rad sa data.frame

# Preslozimo data.frame korelacije u oblik pogodan za ggplot
correlation_melted <- melt(correlation_matrix)

print(correlation_melted)

# Crtanje heat map-a
ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = 
          element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()


# Prelazimo na analizu mtcars skupa podataka

data(mtcars)

# Izvlacimo mpg varijablu - miles per galon
mpg <- mtcars$mpg

# Crtamo histogram MPG varijable
hist(mpg, 
     freq = FALSE, 
     main = "Histogram of MPG", 
     xlab = "MPG", 
     col = "lightblue", 
     border = "white")


# Hocemo sada da iscrtamo grafik funkcije normalne distribucije
# Prvo pravimo sekvencu od 1000 elemenata izmedju -4 i 4
x <- seq(-4, 4, length.out = 1000)

# Crtamo standardni grafik normalne distibucije
plot(x, dnorm(x), type = "l", col = "blue", lwd = 2, 
     main = "Standard Normal Distribution", 
     xlab = "Standard Deviations from Mean", 
     ylab = "Density")


# Ponovo crtamo isti histogram
hist(mpg, 
     freq = FALSE, 
     main = "Histogram of MPG", 
     xlab = "MPG", 
     col = "lightblue", 
     border = "white")

# Crtamo normalnu distribuciju preko histograma
srednja_vrednost <- mean(mpg)
standardna_devijacija <- sd(mpg)
curve(dnorm(x, mean = srednja_vrednost, sd = standardna_devijacija), 
      col = "darkblue", lwd = 2, add = TRUE)


# Kreiramo QQ (kvantil-kvantil) plot
qqnorm(mpg, main = "QQ Plot - MPG")
qqline(mpg, col = "red")

# Racunamo korelacije kolona iz mtcars skupa
correlation_matrix <- cor(mtcars)

# Preslozimo korelacije za ggplot
correlation_melted <- melt(correlation_matrix)

# Crtamo heat map
ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()


# Racunamo korelacije kolona iz mtcars skupa
correlation_matrix <- cor(mtcars)

# Kako bismo prikazali vise informacija, mozemo da grupisemo (klasterizujemo)
# promenljive koje imaju slicne korelacije sa ostalim promenljivima
hc <- hclust(as.dist(1 - correlation_matrix))

# Sortiramo matricu korelacije kako bismo dobili adekvatan prikaz
ordered_correlation_matrix <- correlation_matrix[hc$order, hc$order]

# Crtamo heat map s hijerarhijskim klasteringom
heatmap(ordered_correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), # (100) - Koliko nijansi boje zelimo 
        main = "Correlation Heatmap with Hierarchical Clustering",
        xlab = "Variables", ylab = "Variables", 
        margins = c(5, 5), cexRow = 1.2, cexCol = 1.2)

