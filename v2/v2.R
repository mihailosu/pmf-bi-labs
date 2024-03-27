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



# Brz pregled i ukrstanje kolona moze se izvesti pozivom komande table

table(data$balcony)

# Moguce je ukrstiti 2 kolone

table(data$balcony, data$)