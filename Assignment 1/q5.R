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
width = 1
ii = c(0:15)
ii[9:16] = 17 - c(9:16)
ii = ii^2
ex = exp(-ii/width)
gau = ex %*% t(ex)
image(c(1:16),c(1:16),256*gau,col=gray(c(0:256)/256))


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
  if(i==10)
  image(c(1:16),c(1:16),abs(z2),col=gray(c(0:256)/256))
  x2[i,]=matrix(abs(z2),1,256)
}

#this sets the mean of the blurred data to zero
mx2=apply(x2,2,mean)
x2=sweep(x2,2,mx2)
x2=256*x2/max(x2)

#this calculates the eigenvectors of the blurred data
c=var(x2)
e=eigen(c)
plot(e$values)
pblr=x2 %*% e$vectors[,1:3]

#this calculates the nearest neighbours within a box determined by the blurred data
pblr=t(pblr)
nbrsblrbox = matrix(0,40,10000)
for(i in c(1:10000))
{
  
  box=which(abs(pblr[1,]-pblr[1,i]) < 250 & abs(pblr[2,]-pblr[2,i]) < 250)
  
  if(length(box) < 100)
    box=which(abs(pblr[1,]-pblr[1,i]) < 500 & abs(pblr[2,]-pblr[2,i]) < 500)
  
  #we use the unblurred eigenvectors to find the neighbours
  nnb=findnns(p[1:20,box],p[1:20,i])
  nbrsblrbox[,i]=box[nnb]
}

#Again we can find the accuracy using the same code as for 'nbrs256' 
#except we substiute 'nbrsblrbox'

pacc=0
labels = c(rep("0",1000),rep("1",1000),rep("2",1000),rep("3",1000),rep("4",1000),rep("5",1000),rep("6",1000),rep("7",1000),rep("8",1000),rep("9",1000))
for(k in c(2:40))
{
  results = apply(nbrsblrbox,2,function(x) classify(x,k))
  t=table(results,labels)
  pacc[k]=sum(diag(t))
}
points(c(2:40),pacc[2:40],col='magenta')


results = apply(nbrsblrbox,2,function(x) classify(x,4))
t=table(results,labels)
t
sum(diag(t))







## Try to find optimum blurring
acc=0
for (j in 1:20)
{
width = j
ii = c(0:15)
ii[9:16] = 17 - c(9:16)
ii = ii^2
ex = exp(-ii/width)
gau = ex %*% t(ex)

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
pblr=x2 %*% e$vectors[,1:3]

#this calculates the nearest neighbours within a box determined by the blurred data
pblr=t(pblr)
nbrsblrbox = matrix(0,40,10000)
for(i in c(1:10000))
{
  
  box=which(abs(pblr[1,]-pblr[1,i]) < 250 & abs(pblr[2,]-pblr[2,i]) < 250)
  
  if(length(box) < 100)
    box=which(abs(pblr[1,]-pblr[1,i]) < 500 & abs(pblr[2,]-pblr[2,i]) < 500)
  
  #we use the unblurred eigenvectors to find the neighbours
  nnb=findnns(p[1:20,box],p[1:20,i])
  nbrsblrbox[,i]=box[nnb]
}

results = apply(nbrsblrbox,2,function(x) classify(x,4))
t=table(results,labels)
acc[j]=sum(diag(t))

print(paste("The accuracy of",j,"is",sum(diag(t))))
}
plot(1:20,acc)

# Appears to level off at about 10/11





## Trying to find the optimum box size, when blurring is set to 11

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
image(c(1:16),c(1:16),256*gau,col=gray(c(0:256)/256))

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
  if(i==10)
    image(c(1:16),c(1:16),abs(z2),col=gray(c(0:256)/256))
  x2[i,]=matrix(abs(z2),1,256)
}

#this sets the mean of the blurred data to zero
mx2=apply(x2,2,mean)
x2=sweep(x2,2,mx2)
x2=256*x2/max(x2)

#this calculates the eigenvectors of the blurred data
c=var(x2)
e=eigen(c)
plot(e$values)
pblr=x2 %*% e$vectors[,1:3]
pblr=t(pblr)

acc=0
avg_box=0
k=1
for(j in seq(250,600,50))
{

  #this calculates the nearest neighbours within a box determined by the blurred data
  nbrsblrbox = matrix(0,40,10000)
  box_length=0
  for(i in c(1:10000))
  {
    
    box=which(abs(pblr[1,]-pblr[1,i]) < j & abs(pblr[2,]-pblr[2,i]) < j)
    box_length[i]=length(box)
    
    #we use the unblurred eigenvectors to find the neighbours
    nnb=findnns(p[1:20,box],p[1:20,i])
    nbrsblrbox[,i]=box[nnb]
  }
  results = apply(nbrsblrbox,2,function(x) classify(x,4))
  t=table(results,labels)
  print(paste("Size of box:",length(box)))
  print(paste("Accuracy:",sum(diag(t))))
  avg_box[k]=sum(box_length)/length(box_length)
  acc[k]=sum(diag(t))
  k = k + 1
}
plot(avg_box,acc)

# Appears to level off when j is about 400

## Applying both optimum blurring and finding the best sized box

