ZSCORE_NORMAL_MAT = matrix(0,178,13)
m=178
col=13

for(i in 1:col)
{
	sum =0.0
	sum_sd = 0.0
	mean_value =0.0
	stand_dev =0.0
	for(j in 1:m)
	{
		sum = sum + WINE_MAT[j,i]
	}
	mean_value = sum/178.0
	for(j in 1:m)
	{
		sum_sd = sum_sd + ((WINE_MAT[j,i] - mean_value)^2)
	}
	stand_dev = sqrt(sum_sd/178.0)
	for (j in 1:m)
	{
			ZSCORE_NORMAL_MAT[j,i]=((WINE_MAT[j,i] - mean_value)/stand_dev)
	}
}
ZSCORE_EUCLIDEAN_MAT = matrix(0,178,178)
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
			istance=istance + ((ZSCORE_NORMAL_MAT[j,k]-ZSCORE_NORMAL_MAT[i,k])*(ZSCORE_NORMAL_MAT[j,k]-ZSCORE_NORMAL_MAT[i,k]))
	}
	final_dist= sqrt(istance)
	ZSCORE_EUCLIDEAN_MAT[i,j]=final_dist
	ZSCORE_EUCLIDEAN_MAT[j,i]=final_dist
  }
}
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
			if(ZSCORE_EUCLIDEAN_MAT[i,j]< min)
			{
				min = ZSCORE_EUCLIDEAN_MAT[i,j]
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