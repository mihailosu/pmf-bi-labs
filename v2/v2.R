# Ukljucimo biblioteku za rad sa Excel fajlovima
library(readxl)

# Ucitamo podatke
# Napomena: potrebno je podesiti korektnu putanju do fajla
data <- read_excel("/Users/mihailoilic/PMF/PI - Poslovna Inteligencija/pmf-bi-labs/apartments_for_sale.xlsx")

# Podsetimo se, koja je struktura podataka i kojim klasama pripada ucitana tabela
mode(data)
class(data)

# Prikazemo prvih 5 redova
head(data)

# Generalni prikaz podataka
summary(data)

# Pregled kolona koje imaju nedostajuce podatke
missing_values <- colSums(is.na(data))

print(missing_values)

# Uklanjanje redova s nedostajucim podacima

data <- na.omit(data)

# Provera da li smo uklonili sve redove gde podaci nedostaju
missing_values_2 <- colSums(is.na(data))

print(missing_values_2)

# Proverimo koliko podataka nam je ostalo
nrow(data)



# Uklanjamo duple redove na osnovu ID kolone

data <- data[!duplicated(data$ID),]

# Konverzija u faktore

summary(data) # Kolona neighborhood je klase character

data$neighborhood <- as.factor(data$neighborhood)
data$city <- as.factor(data$city)
data$flooring_type <- as.factor(data$flooring_type)

class(data$neighborhood)
summary(data) # Kolona neighborhood je sada klase factor i korektno prikazuje kategorije delova grada


# Osnovne desktiptive funkcije, mean, median, max, min ...
mean(data$price)

median(data$price)

var(data$price)

sd(data$price)

sqrt(var(data$price))

min(data$price)

max(data$price)

range(data$price)

cor(data$price, data$square_feet, method = "pearson")

# Brz pregled i ukrstanje kolona moze se izvesti pozivom komande table

table(data$balcony)

# Moguce je ukrstiti 2 kolone

table(data$balcony, data$neighborhood)

# Procentualni prikaz

table(data$balcony) / length(data$balcony)

t1 = table(data$balcony, data$neighborhood)

prop.table(t1, 2)

# Osnovno crtanje

hist(data$price)

# Histogram spratova, navodeci "korpe" rucno

hist(data$floor, breaks = seq(1, max(data$floor)))

# Graficka tabela

t2 = table(data$balcony, data$neighborhood)
plot(t2)


# Box plot cena i dela grada

boxplot(data$price ~ data$neighborhood, data, xlab="neighborhood", ylab="price")

# Pie chart po gradovima

pie(table(data$neighborhood))


# Ukljucimo ggplot2

library(ggplot2)

# Odnos tipa poda i lokacije u gradu

ggplot(data, aes(x=neighborhood, fill=flooring_type)) + geom_bar(width = 0.5)

ggplot(data, aes(x=neighborhood, fill=flooring_type)) 
      + geom_bar(width = 0.5) 
      + facet_wrap(~balcony)


