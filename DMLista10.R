##### PCA #####

miasta <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/Miasta.txt")

#korzystamy z wbudowanej funkcji princomp

# a)
#wykonaj analizę składowych głównych dla wszystkich zmiennych
pc1 <- princomp(~., data = miasta, cor = TRUE)
names(pc1)

#można zobaczyć jak wyglądają te składowe główne
#każda z nowych kolumn to kombinacje liniowe starych zmiennych
pc1$scores

#jak wyglądają nowe zmienne?
#wektory kierunkowe
pc1$loadings
#z1 = 0.485 * Work - 0.618 * Price - 0.619 * Salary
#z2 = 0.875 * Work + 0.348 * Proce + 0.338 * Salary
#z3 = -0.705 * Price + 0.709 * Salary

# b)
#jaki jest procent wariancji tłumaczony przez poszczególne składowe? Czy możemy dokonać redukcji wymiaru danych?
plot(pc1)
#pierwsza zmienna tłumaczy już całkiem dużo 

# c) 
#obliczyć kierunki główne i składowe głowne
#kierunki - to te wektory

# d)
#Narysuj dwuwykres dla dwóch pierwszych składowych głównych
biplot(pc1)

# e)
#miasto o największej pierwszej składowej głównej
which.max(pc1$scores[,1])
#Manila - stolica Filipin
#z1 jest duże gdy się dużo pracuje, ale mało zarabia i mało wydaje
#rzeczywiście tak jest
miasta["Manila",]


# Jak wykorzystać PCA do klasyfikacji?
#Jeśli mamy tylko dane ilościowe - wybieramy pierwszycyh kilka składowych głównych
#Jeśli mamy ilościowe i jakościowe - musimy zamienic jakościowe na binarne (0-1 dla kazego levela)