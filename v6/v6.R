
superstore_data <- read.csv("/Users/mihailoilic/PMF/PI - Poslovna Inteligencija/pmf-bi-labs/superstore.csv")

# Prikazimo prvih 5 redova
head(superstore_data, 5)

# ...i poslednjih 5 redova
tail(superstore_data, 5)

View(superstore_data)

summary(superstore_data)

# Zeleli bismo da vidimo odnose broja artikala po porudzbini
# u odnosu na region

table(superstore_data$Region, superstore_data$Quantity)

# Kako da prikazemo procentualno, po redovima iste informacije?
# Pomoc: Paket "proportions"

tabela <- table(superstore_data$Region, superstore_data$Quantity)

tabela_proc <- prop.table(tabela, 1) * 100

round(tabela_proc, 2)


# Da li imamo nedostajuce podatke?

missing_data = superstore_data[!complete.cases(superstore_data), ]


# Prikazujemo profit po regionima

library(ggplot2)

ggplot(
  superstore_data,
  aes(x = Region, fill = Profit)
) + geom_bar(width = 0.5)

# Interesuje nas zarada po kategoriji proizvoda, po regionu

ggplot(
  superstore_data,
  aes(x = Region, fill = Profit)
) + 
  geom_bar(width = 0.5) + 
  facet_wrap(~Category) + 
  ggtitle("Category") + 
  xlab("Region") + ylab("Profit")


# Koja je raspodela ukupne prodaje svih proizvoda?

qqnorm(superstore_data$Sales)
qqline(superstore_data$Sales)

# Korelacije izmedju numerickih kolona

numerical_columns <- c("Sales", "Quantity", "Discount", "Profit")

# Racunamo matricu korelacije
correlation_matrix <- cor(superstore_data[numerical_columns])

print(correlation_matrix)

# Crtamo matricu korelacije

library(reshape2)
melted_correlation_matrix <- melt(correlation_matrix)

ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

# hi-kvadarat test za odnos regiona i nacina slanja paketa

contingency_table <- table(superstore_data$Region, superstore_data$Ship.Mode)

print(contingency_table)

chi_squared_test <- chisq.test(contingency_table)

print(chi_squared_test$expected)

print(chi_squared_test$observed)

print(chi_squared_test)


# Stablo odlucivanja

library(rpart)
library(rpart.plot)

superstore_data$Segment <- as.factor(superstore_data$Segment)
superstore_data$Ship.Mode <- as.factor(superstore_data$Ship.Mode)
superstore_data$Region <- as.factor(superstore_data$Region)
superstore_data$Category <- as.factor(superstore_data$Category)
superstore_data$Sub.Category <- as.factor(superstore_data$Sub.Category)

tree_input = superstore_data[, c("Segment", "Region", "Ship.Mode", "Category", "Quantity", "Profit", "Sub.Category")]



decision_tree_model <- rpart(Profit ~ ., 
                             data = tree_input, 
                             method = "anova")

print(summary(decision_tree_model))

rpart.plot(decision_tree_model, type = 3, extra = 101, fallen.leaves = TRUE)

