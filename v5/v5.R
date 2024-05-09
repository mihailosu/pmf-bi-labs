# Data Mining tehnike

#################
# Klasterizovanje
#################

# Ucitamo iris skup podataka
data(iris)

# Pregledamo prvih par redova
head(iris)

# Odredimo kolone na osnovu kojih klasterizujemo
# (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) 
features <- iris[, 1:4]

# Koristimo "lakat metodu" (elbow method) da odredimo optimalan broj klastera
# Za ocenu klastera koristimo:
# withinss	- Vector of within-cluster sum of squares, one component per cluster
wss <- numeric(10)
for (i in 1:10) {
  wss[i] <- sum(kmeans(features, centers = i)$withinss)
}
# Prikazemo ocene klasterizovanja
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")

# Cini se da je najbolje koristiti 3 klastera

# Klasterizujemo pomocu kmeans sa 3 centra
set.seed(123)
kmeans_result <- kmeans(features, centers = 3)

# Pogledajmo centre koje smo dobili
kmeans_result$centers

# Dodajemo labele klastera skupu podataka
iris$cluster <- kmeans_result$cluster

# Pravimo matricu konfuzije, tj ukrstamo prave klase sa dobijenim klasterima
cluster_labels <- as.factor(kmeans_result$cluster)
actual_labels <- as.factor(iris$Species)

# Pravimo matrice
conf_matrix <- table(cluster_labels, actual_labels)
print(conf_matrix)

# Ucitajmo graficku biblioteku
library(ggplot2)

# Iscrtamo data frame kao heatmap radi vizuelizacije
ggplot(data = as.data.frame(conf_matrix), aes(x = actual_labels, y = cluster_labels, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(x = "Actual Species", y = "Cluster", fill = "Frequency") +
  ggtitle("Confusion Matrix for K-means Clustering on Iris Dataset")

#################
# Klasterizovanje - Agglomerativni klastering
#################

# Ucitamo stats paket
library(stats)

# Ovaj put klasterizujemo samo po 2 kolone (radi lakse vizuelizacije)
variables <- iris[, c("Sepal.Length", "Petal.Length")]

# Vrsimo aglomerativni klastering
agglo_result <- hclust(dist(variables), method = "complete")

# Presecamo dendrogram na 3 klastera
num_clusters <- 3  
clusters <- cutree(agglo_result, k = num_clusters)

# Odredjujemo boje klasterima
cluster_colors <- c("red", "green", "cyan")

# Vizuelizujemo dobijene klastere
plot(variables, col = cluster_colors[clusters], pch = 19, 
     main = "Agglomerative Clustering on Iris Dataset (Sepal.Length vs. Petal.Length)",
     xlab = "Sepal Length", ylab = "Petal Length")

# Preko toga oznacimo originalne klastere da vidimo preklapanje
points(variables[iris$Species == "setosa", ], pch = 1)
points(variables[iris$Species == "versicolor", ], pch = 2)
points(variables[iris$Species == "virginica", ],pch = 3)

# Dodamo legendu
legend("bottomright", 
       legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Setosa", "Versicolor", "Virginica"), 
       col = c(1, 2, 3, "red", "blue", "green"), pch = c(19, 19, 19, 1, 2, 3), title = "Group")

# Prikazimo kako izgleda rezultujuci dendrogram
plot(agglo_result, main = "Dendrogram", xlab = "", sub = "", cex = 0.6)

#################
# Rule Mining
#################

# Ucitamo potreban paket
library(arules)

# Ucitamo skup podataka
data("Groceries")

# Pregled prvih nekoliko korpi
inspect(head(Groceries))

# Koristimo apriori algoritam za izvlacenja pravila
rules <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.5))

# Pogledajmo prvih 5 pravila
inspect(head(rules))

# Sortiramo pravila po "Lift" oceni
rules_sorted <- sort(rules, by = "lift")

# Pogledajmo najboljih 10 pravila po ovoj osnovi
inspect(head(rules_sorted, n = 10))


#################
# Decision Tree
#################

# Ucitamo potrebne pakete
library(rpart)
library(rpart.plot)

# Ucitamo skup podataka
data(iris)

# Podelimo skup podataka na onaj za trening i 
# za testiranje
set.seed(42)
train_indices <- sample(1:nrow(iris), 0.8 * nrow(iris)) 
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Kreiramo stablo odlucivanja i istreniramo ga
iris_tree <- rpart(Species ~ ., data = train_data, method = "class")

# Vizuelizujmo stablo s pravilima
rpart.plot(iris_tree, main = "Decision Tree for Iris Species Classification")

# Iskoristimo ga da napravimo predikcije nad test skupom
predictions <- predict(iris_tree, newdata = test_data, type = "class")

# Ocenimo accuracy klasifikatora
accuracy <- sum(predictions == test_data$Species) / nrow(test_data)
cat("Accuracy:", accuracy)
