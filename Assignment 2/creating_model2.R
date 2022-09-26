setwd("C:/Users/User/Desktop/ca274/final_assessment")

stroke=readRDS("stroke.RDS")

# 4 strokes

for (i in 1:20)
{
# Creating s1
x1=runif(1,8,12)
y1=runif(1,13,16)
len1=runif(1,5,10)
theta1=runif(1,0.5*pi,0.7*pi)
width1=runif(1,0.5,3)

s1=stroke(x1,y1,theta1,len1,width1)
#image(c(1:16),c(1:16),s1,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

# Creating s2
# x1 y1 the same

# Need to make sure p3 is not off the screen, {x2, y2 >= 0}
theta2=runif(1,1*pi,1.4*pi)
len2=runif(1,4,8)
width2=runif(1,0.5,3)

s2=stroke(x1,y1,theta2,len2,width2)
#im=s1+s2
#im[im > 1]=1  #this thresholds the image so that the max brightness is 1
#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

# Creating s3
# x2,y2 endpoints of s2
x2=x1 - len2*sin(theta2)
y2=y1 + len2*cos(theta2)
theta3=runif(1,0.6*pi,0.85*pi)
len3=runif(1,8,12)
x3=x2 - len3*sin(theta3)
y3=y2 + len3*cos(theta3)
j = 0
while ((!(x3 >= 0 & x3 <= 7) | !(y3 >= 0 & y3 <= 7)) & j < 100 )
{
  theta3=runif(1,0.6*pi,0.85*pi)
  len3=runif(1,8,12)
  x3=x2 - len3*sin(theta3)
  y3=y2 + len3*cos(theta3)
  j = j + 1
}
width3=runif(1,0.5,3)

s3=stroke(x2,y2,theta3,len3,width3)


# Creating s4
# x2,y2 endpoints of s3
x3=x2 - len3*sin(theta3)
y3=y2 + len3*cos(theta3)

theta4=runif(1,-0.53*pi,-0.47*pi)
len4=runif(1,7,12)
width4=runif(1,0.5,3)

s4=stroke(x3,y3,theta4,len4,width4)

im=s1+s2+s3+s4
im[im > 1]=1  #this thresholds the image so that the max brightness is 1
image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
readline()
}