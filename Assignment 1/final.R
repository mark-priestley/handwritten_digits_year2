## Applying optimum blurring and the best sized box


proc.time()
## Read in d
d=readRDS("d.RDS")
findnns=readRDS("findnns.RDS")
classify=readRDS("classify.RDS")
d=t(d)

## Find p
c=var(d)
e=eigen(c)
plot(e$values)
p=d %*% e$vectors[,1:20]
p=t(p)

#this is the code to decide how much the images are blurred
width = 11
ii = c(0:15)
ii[9:16] = 17 - c(9:16)
ii = ii^2
ex = exp(-ii/width)
gau = ex %*% t(ex)

#this is the code that does the blurring
#the blurred images are called x2
x2=d
x=d
for(i in c(1:10000))
{
  z = matrix(as.numeric(x[i,]),16,16)
  ft=fft(z)
  ft = ft *gau
  z2=fft(ft,inverse=T)
  x2[i,]=matrix(abs(z2),1,256)
}

#this sets the mean of the blurred data to zero
mx2=apply(x2,2,mean)
x2=sweep(x2,2,mx2)
x2=256*x2/max(x2)

#this calculates the eigenvectors of the blurred data
c=var(x2)
e=eigen(c)
pblr=x2 %*% e$vectors[,1:11]

#this calculates the nearest neighbours within a box determined by the blurred data
pblr=t(pblr)
nbrsblrbox = matrix(0,40,10000)
for(i in c(1:10000))
{
  
  box=which(abs(pblr[1,]-pblr[1,i]) < 400 & abs(pblr[2,]-pblr[2,i]) < 400)
  
  #we use the unblurred eigenvectors to find the neighbours
  nnb=findnns(p[1:20,box],p[1:20,i])
  nbrsblrbox[,i]=box[nnb]
}

labels = c(rep("0",1000),rep("1",1000),rep("2",1000),rep("3",1000),rep("4",1000),rep("5",1000),rep("6",1000),rep("7",1000),rep("8",1000),rep("9",1000))


results = apply(nbrsblrbox,2,function(x) classify(x,4))
t=table(results,labels)
print(t)
print(paste("Accuracy:",sum(diag(t))))
proc.time()

# I received an accuacy of 97.43%
# The total time it took for all the code above on my laptop was
#  user  system elapsed 
# 65.53    2.84  308.64