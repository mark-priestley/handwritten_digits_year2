setwd("C:/Users/User/Desktop/ca274/final_assessment")

stroke=readRDS("stroke.RDS")

# Creating model 2 with 3 strokes

# s1
x1=runif(1,2,6)
y1=runif(1,12,16)
len1=runif(1,9,12)
theta1=runif(1,-0.7*pi,-0.5*pi)
width1=runif(1,0.5,3)

s1=stroke(x1,y1,theta1,len1,width1)
image(c(1:16),c(1:16),s1,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

# s2
x2=x1 - len1*sin(theta1)
y2=y1 + len1*cos(theta1)
len2=runif(1,10,16)
theta2=runif(1,0.6*pi,0.85*pi)
x3=x2 - len2*sin(theta2)
y3=y2 + len2*cos(theta2)
j = 0
while ((!(x3 >= 0 & x3 <= 7) | !(y3 >= 0 & y3 <= 7)) & j < 100 )
{
  theta2=runif(1,0.6*pi,0.85*pi)
  len2=runif(1,8,12)
  x3=x2 - len2*sin(theta2)
  y3=y2 + len2*cos(theta2)
  j = j + 1
}
width2=runif(1,0.5,3)

s2=stroke(x2,y2,theta2,len2,width2)
#im=s1+s2
#im[im > 1]=1  #this thresholds the image so that the max brightness is 1
#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))


# s3
x3=x2 - len2*sin(theta2)
y3=y2 + len2*cos(theta2)
len2=runif(1,8,14)
theta2=runif(1,0.6*pi,0.85*pi)
width2=runif(1,0.5,3)











