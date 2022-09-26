## Gets matrix d, nearest neighbour matrix, findnns & classify functions
nbrs256 = readRDS("nbrs256.rds")
d = readRDS("d.rds")
classify = readRDS("classify.rds")
findnns = readRDS("findnns.rds")

## 


labels = c(rep("0",1000),rep("1",1000),rep("2",1000),rep("3",1000),rep("4",1000),rep("5",1000),rep("6",1000),rep("7",1000),rep("8",1000),rep("9",1000))
k = 2
predicted = apply(nbrs256,2, function(x) classify(x,k))
t = table(predicted,labels)
View(t)

par(mfrow=c(4,2))
group = which(labels == "8" & predicted == "3")
j = 1
for(i in group)
{
  z=matrix(d[,i],16,16)
  image(c(1:16),c(1:16),z[,c(16:1)],col=gray(c(0:255)/255))
  z=matrix(d[,nbrs256[2,i]],16,16)
  image(c(1:16),c(1:16),z[,c(16:1)],col=gray(c(0:255)/255))
  j=j+1
  if (j > 4)
  {
    break
  }
}

## Question 2

d_2=t(d)
c=var(d2)
e=eigen(c)
plot(e$values)
p=d_2 %*% e$vectors[,1:20]

p=t(p)
nbrsp20 = matrix(0,40,10000)
proc.time()
for(i in c(1:10000))nbrsp20[,i]=findnns(p[1:20,],p[1:20,i])
proc.time()






