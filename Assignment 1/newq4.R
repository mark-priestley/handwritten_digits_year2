d = readRDS("d.rds")
classify=readRDS("classify.RDS")
findnns=readRDS("findnns.RDS")

d=t(d)
c=var(d)
e=eigen(c)
plot(e$values)
p=d %*% e$vectors[,1:20]
p=t(p)

## Finding the most accurate box size
acc=0
avg_size=0
labels = c(rep("0",1000),rep("1",1000),rep("2",1000),rep("3",1000),rep("4",1000),rep("5",1000),rep("6",1000),rep("7",1000),rep("8",1000),rep("9",1000))
j=1
for(x in seq(0,500,50))
{
  nbrsbox = matrix(0,40,10000)
  box_length=0
  system.time(
    for(i in c(1:10000))
    {
      box=which(abs(p[1,]-p[1,i]) < (250 + x) & abs(p[2,]-p[2,i]) < (150 + x))
      box_length[i]=length(box)
      nnb=findnns(p[1:20,box],p[1:20,i])
      nbrsbox[,i]=box[nnb]
    }
  )
  print(paste("Size of box:",length(sum(box_length)/length(box_length))))
  avg_size[j]=sum(box_length)/length(box_length)
  
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
plot(avg_size,acc)
