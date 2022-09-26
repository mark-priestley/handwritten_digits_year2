d3=scan("C:/Users/User/Desktop/ca274/digits/Zl3d.dat",nlines=1000,n=256000)
d8=scan("C:/Users/User/Desktop/ca274/digits/Zl8d.dat",nlines=1000,n=256000)

d=c(d3,d8)
d=matrix(d,256,2000)
d=t(d)

## Need to find the digits that are wrong
classify = readRDS("assessment_1/classify.rds")
nbrs256 = readRDS("assessment_1/nbrs256.rds")

k = 2
predicted = apply(nbrs256,2, function(x) classify(x,k))
t = table(predicted,labels)

## I will look at 8s classified as 3s
misclassified = which(predicted == "3" & labels == "8")
misclassified=misclassified-7000


c=var(d)
e=eigen(c)

p = d %*% e$vectors
plot(p[,1],p[,2],col=c(rep("red",1000),rep("blue",1000)))


theta = 0.25
rot=matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2,2)
rev=matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),2,2)

colours = c(rep("red",1000),rep("blue",1000))
colours[misclassified] = "green"
mypch=c(rep(4,2000))
mypch[misclassified]=19

p=t(p)
proj=matrix(0,2,10000)

for(i in c(1:3000))
{

pos = locator(1)

if(pos$x < 0)
{
if(pos$y < 0) p[c(1,3),] = rot %*% p[c(1,3),] else  p[c(1,2),] = rot %*% p[c(1,2),]
}
else
{
if(pos$y < 0) p[c(1,3),] = rev %*% p[c(1,3),] else p[c(1,2),] = rev %*% p[c(1,2),]
}


proj[1,] = p[1,]*4000/(9000-p[3,])
proj[2,] = p[2,]*4000/(9000-p[3,])

zorder = order(p[3,])

plot(proj[1,zorder],proj[2,zorder],xlim=c(-1000,1000),ylim=c(-1000,1000),col=colours[zorder],pch=mypch[zorder])

}

# The 3s are red, and the 8s are blue, the misclassified 8s are green solid circles
# The majority of the points of the misclassified images are on the 'border' between the two clouds,
# which would explain why they were misclassified

# There is however about 5 points which are quite far away from the 3 cloud, so why were they miclassified?

group=identify(proj[1,zorder],proj[2,zorder],n=2)
group=zorder[group]

dev.new()
d=t(d)
par(mfrow=c(2,2))
for(i in group)
{
z=matrix(d[,i],16,16)
image(c(1:16),c(1:16),z[,c(16:1)],col=gray(c(0:255)/255))
z=matrix(d[,nbrs256[2,i+7000]-3000],16,16)
image(c(1:16),c(1:16),z[,c(16:1)],col=gray(c(0:255)/255))
readline()
}

# From the images, you can see that these are normal 8s, its there nearest nieghbour thats unusual for a 3






