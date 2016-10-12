WINE = read.csv("wine.data",header=FALSE)
colnames(WINE)=c("ClassIdentifier","Alcohol","MalicAcid","Ash","AlcalinityOfAsh","Magnesium","TotalPhenols","Flavanoids","NonflavanoidPhenols","Proanthocyanins","ColorInternsity","Hue","OD280/OD315OfDilutedWines","Proline")
WINE$ClassIdentifier = NULL
 WINE_COMPONENTS = c("Alcohol", "Malic Acid","Ash", "Alcalinity of ash","Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
WINE_COR_MAT=cor(WINE)
MAT_MAX = matrix(0,13,13)
MIN_MAX = matrix(0,13,13)
for(i in 1:nrow(WINE_COR_MAT))  # for each row
{
  for(j in 1:ncol(WINE_COR_MAT)) # for each column
  {
    	if(i==j)
	{
		MAT_MAX[i,j]=-50
		MIN_MAX[i,j]=50
	}
	else
	{
		MAT_MAX[i,j] = WINE_COR_MAT[i,j]     # assign values based on position: product of two indexes
		MIN_MAX[i,j] = WINE_COR_MAT[i,j]
	}
  }

}
i=0
j=0
MAX_CORELATED =matrix(0,4,2)
for (i in 1:nrow(MAX_CORELATED))
	{
		MAX_TEMP = c(which(MAT_MAX == max(MAT_MAX), arr.ind = TRUE))
		j=0
		for(j in 1:ncol(MAX_CORELATED))
		{
			MAX_CORELATED[i,j]=WINE_COMPONENTS[MAX_TEMP[j]]
		}
		MAT_MAX[MAX_TEMP[1],MAX_TEMP[2]]=-50
		MAT_MAX[MAX_TEMP[2],MAX_TEMP[1]]=-50
	}
		
i=0
j=0
MIN_CORELATED =matrix(0,4,2)
for (i in 1:nrow(MIN_CORELATED))
	{
		MIN_TEMP = c(which(MIN_MAX == min(MIN_MAX), arr.ind = TRUE))
		j=0
		for(j in 1:ncol(MIN_CORELATED))
		{
			MIN_CORELATED[i,j]=WINE_COMPONENTS[MIN_TEMP[j]]
		}
		MIN_MAX[MIN_TEMP[1],MIN_TEMP[2]]=50
		MIN_MAX[MIN_TEMP[2],MIN_TEMP[1]]=50
	}

plot(WINE$Flavanoids, WINE$TotalPhenols ,xlab="Flavanoids" ,ylab="TotalPhenols" ,col = c("blue","red"))
plot(WINE$Flavanoids, WINE$TotalPhenols ,xlab="Flavanoids" ,ylab="TotalPhenols", main = "Flavanoids V/S TotalPhenols",col = c("blue","red"))
plot(WINE$"OD280/OD315OfDilutedWines", WINE$Flavanoids ,xlab="OD280/OD315 of diluted wines" ,ylab="Flavanoids", main ="OD280/OD315OfDilutedWines V/S Flavanoids",col=c("blue","red"))
plot(WINE$"OD280/OD315OfDilutedWines", WINE$TotalPhenols ,xlab="OD280/OD315 of diluted wines" ,ylab="Total Phenols", main="OD280/OD315OfDilutedWines V/S Total Phenols", col=c("blue","red"))
plot(WINE$"Proanthocyanins", WINE$Flavanoids ,xlab="Proanthocyanins" ,ylab="Flavanoids" , main="Proanthocyanins V/S Flavanoids " ,col = c("blue","red"))
plot(WINE$"Proanthocyanins", WINE$Flavanoids ,xlab="Proanthocyanins" ,ylab="Flavanoids" , main="Proanthocyanins V/S Flavanoids" ,col = c("blue","red"))
plot(WINE$"Hue", WINE$MalicAcid ,xlab="Hue" ,ylab="Malic Acid" ,main="Hue V/S Malic Acid" , col = c("blue","red"))
plot(WINE$"NonflavanoidPhenols", WINE$Flavanoids ,xlab="Nonflavanoid Phenols" ,ylab="Flavanoids",main="Nonflavanoid Phenols V/S Flavanoids" , col = c("blue","red"))
plot(WINE$Hue, WINE$"Color Intensity" ,xlab="Hue" ,ylab="Color Intensity",main="Hue V/S Color Intensity" , col = c("blue","red"))
plot(WINE$"OD280/OD315OfDilutedWines", WINE$"NonflavanoidPhenols" ,xlab="OD280/OD315OfDilutedWines" ,ylab="NonflavanoidPhenols",main="OD280/OD315OfDilutedWines V/S Non Flavanoid Phenols" , col = c("blue","red"))
 
