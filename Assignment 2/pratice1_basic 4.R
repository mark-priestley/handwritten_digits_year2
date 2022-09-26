stroke = function(x1,y1,theta,len,width)
{
  x=c(1:16)
  y=c(1:16)
  
  x=x-x1
  y=y-y1
  u1=x*cos(theta)
  u2=y*sin(theta)
  u=matrix(u1,16,16)+t(matrix(u2,16,16))
  
  
  v1=-x*sin(theta)
  v2=y*cos(theta)
  v=matrix(v1,16,16)+t(matrix(v2,16,16))
  
  m = (v > 0 & v < len)
  exp(-u*u/width)*m
}

s1=stroke(4,5,-0.1*pi,10,0.5)
s2=stroke(4,9,-0.5*pi,10,1)
s3=stroke(9,1,-0.05*pi,8,1)

im=s1+s2+s3
im[im > 1]=1  #this thresholds the image so that the max brightness is 1
image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

setwd("C:/Users/User/Desktop/ca274/final_assessment")
saveRDS(stroke,"stroke.RDS")
