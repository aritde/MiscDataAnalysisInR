#One time Execution
WINE = read.csv("wine.data",header=FALSE)
CLASSES = c(WINE$ClassIdentifier)
colnames(WINE)=c("ClassIdentifier","Alcohol","MalicAcid","Ash","AlcalinityOfAsh","Magnesium","TotalPhenols","Flavanoids","NonflavanoidPhenols","Proanthocyanins","ColorInternsity","Hue","OD280/OD315OfDilutedWines","Proline")
WINE_MAT= read.csv("wine.data",header=FALSE)
colnames(WINE_MAT)=c("ClassIdentifier","Alcohol","MalicAcid","Ash","AlcalinityOfAsh","Magnesium","TotalPhenols","Flavanoids","NonflavanoidPhenols","Proanthocyanins","ColorInternsity","Hue","OD280/OD315OfDilutedWines","Proline")
WINE_MAT$ClassIdentifier = NULL

#Euclidean Matrix Calculation
EUCLIDEAN_MAT = matrix(0,178,178)
m=177
col=13
final_dist=0
for(i in 1:m)  # for each row
{
  for(j in 2:(m+1)) # for each column
  {
      istance=0
	for(k in 1:col)
	{
			istance=istance + ((WINE_MAT[j,k]-WINE_MAT[i,k])*(WINE_MAT[j,k]-WINE_MAT[i,k]))
	}
	final_dist= sqrt(istance)
	EUCLIDEAN_MAT[i,j]=final_dist
	EUCLIDEAN_MAT[j,i]=final_dist
  }
}
#One Time Execution Ends
PCTAGE=0
i=1
j=1
m=177
c=0
c1=0
c2=0
c3=0
for(i in 1:(m+1))
{
	min = 999999
	index=999999
	for(j in 1:(m+1))
	{
			if( i !=j)
			{
			if(EUCLIDEAN_MAT[i,j]< min)
			{
				min = EUCLIDEAN_MAT[i,j]
				index = j
			}
			}
	}
	if(CLASSES[i] == CLASSES[index])
	{
		c=c+1
	}
	if(CLASSES[i]==CLASSES[index] && CLASSES[i]==1)
	{	
		c1=c1+1
	}
	if(CLASSES[i]==CLASSES[index] && CLASSES[i]==2)
	{	
		c2=c2+1
	}
	if(CLASSES[i]==CLASSES[index] && CLASSES[i]==3)
	{	
		c3=c3+1
	}
}
OVERALLPCTAGE = (c/178)*100	
PCTAGE1=(c1/59)*100
PCTAGE2=(c2/71)*100
PCTAGE3=(c3/48)*100		
