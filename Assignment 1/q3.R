d = readRDS("d.rds")
classify=readRDS("classify.RDS")
findnns=readRDS("findnns.RDS")

dim(d)

d=t(d)
c=var(d)
e=eigen(c)
plot(e$values)
p=d %*% e$vectors[,1:210]


p=t(p)
labels=c(rep("0",1000),rep("1",1000),rep("2",1000),rep("3",1000),rep("4",1000),rep("5",1000),rep("6",1000),rep("7",1000),rep("8",1000),rep("9",1000))
acc=c()

for(n in c(2,5,10,20,40,60,80,120,160,200))
{

  proc.time()
  nbrsp20 = matrix(0,40,10000)
  for(i in c(1:10000))nbrsp20[,i]=findnns(p[1:n,],p[1:n,i])
  proc.time()
  results = apply(nbrsp20,2,function(x) classify(x,4)) ## with k = 4
  t1=table(results,labels)
  print(t1)
  acc=append(acc,sum(diag(t1)))
}
plot(c(2,5,10,20,40,60,80,120,160,200),acc/100,xlab="Eigenvectors",ylab="Accuracy (%)")
## Accuracy grows very quickly at first, and then levels off when you use about 20 eigenvectors

nbrsp20 = matrix(0,40,10000)
proc.time()
for(i in c(1:10000))nbrsp20[,i]=findnns(p[1:20,],p[1:20,i])
proc.time()

#to calculate the accuracy we use the same code as above but we replace 'nbrs256' with 'nbrsp20'
labels = c(rep("0",1000),rep("1",1000),rep("2",1000),rep("3",1000),rep("4",1000),rep("5",1000),rep("6",1000),rep("7",1000),rep("8",1000),rep("9",1000))
pacc=rep(0,40)
for(k in c(2:40))
{
  results = apply(nbrsp20,2,function(x) classify(x,k))
  t=table(results,labels)
  pacc[k]=sum(diag(t))
}
plot(c(2:40),pacc[2:40])

#this code calculates the nearest neighbours and the accuracy
#for different numbers of eigenvectors from 2 to 20
#this code takes a very long time to run!
pacc=c()
for(n in c(2:20))
{
  neighbours=matrix(0,n,10000)
  for(i in c(1:10000))
  {
    neighbours[,i]=findnns(p[1:n,],p[1:n,i])
  }
  results = apply(neighbours,2,function(x) classify(x,5))
  t=table(results,labels)
  append(pacc,sum(diag(t)))
}

plot(pacc)