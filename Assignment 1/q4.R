d = readRDS("d.rds")
classify=readRDS("classify.RDS")
findnns=readRDS("findnns.RDS")

d=t(d)
c=var(d)
e=eigen(c)
plot(e$values)
p=d %*% e$vectors[,1:20]
p=t(p)

#this code calcuates the 40 nearest neighbours by searching within a box
nbrsbox = matrix(0,40,10000)
system.time(
  for(i in c(1:10000))
  {
    box=which(abs(p[1,]-p[1,i]) < 600 & abs(p[2,]-p[2,i]) < 500)
    nnb=findnns(p[1:20,box],p[1:20,i])
    nbrsbox[,i]=box[nnb]
  }
)
#   user  system elapsed 
#  49.95    0.25   50.31

print(length(box)) # Length of box
# 3267

## Finding the  accuracy, using 4 nearest neighbours
results=0
for(i in c(1:10000))
{
  results[i]=classify(nbrsbox[,i],4)
}
labels = c(rep("0",1000),rep("1",1000),rep("2",1000),rep("3",1000),rep("4",1000),rep("5",1000),rep("6",1000),rep("7",1000),rep("8",1000),rep("9",1000))
t=table(results,labels)
acc=sum(diag(t))
# 9747




## Expanding the box, how does execution time change?
#this code calcuates the 40 nearest neighbours by searching within a box
nbrsbox = matrix(0,40,10000)
system.time(
  for(i in c(1:10000))
  {
    box=which(abs(p[1,]-p[1,i]) < 700 & abs(p[2,]-p[2,i]) < 600)
    nnb=findnns(p[1:20,box],p[1:20,i])
    nbrsbox[,i]=box[nnb]
  }
)
#   user  system elapsed 
#  62.89    3.72   66.63


print(length(box)) # Length of box
# 4050

## Finding the  accuracy, using 4 nearest neighbours
results=0
for(i in c(1:10000))
{
  results[i]=classify(nbrsbox[,i],4)
}
labels = c(rep("0",1000),rep("1",1000),rep("2",1000),rep("3",1000),rep("4",1000),rep("5",1000),rep("6",1000),rep("7",1000),rep("8",1000),rep("9",1000))
t=table(results,labels)
acc=sum(diag(t))
# 9747

## Finding the most accurate box size
acc=0
size=0
j=1
for(x in seq(0,700,50))
{
  nbrsbox = matrix(0,40,10000)
  system.time(
    for(i in c(1:10000))
    {
      box=which(abs(p[1,]-p[1,i]) < (250 + x) & abs(p[2,]-p[2,i]) < (150 + x))
      nnb=findnns(p[1:20,box],p[1:20,i])
      nbrsbox[,i]=box[nnb]
    }
  )
  print(paste("Size of box:",length(box)))
  size[j]=length(box)
  
  results=0
  for(i in c(1:10000))
  {
    results[i]=classify(nbrsbox[,i],4)
  }
  t=table(results,labels)
  accur=sum(diag(t))
  print(paste("Accuracy:",accur))
  acc[j]=accur
  writeLines("")
  j=j+1
}

## The accuracy seems to level off at about 2500 (500, 400)