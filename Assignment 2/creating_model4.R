setwd("C:/Users/User/Desktop/ca274/final_assessment")

# Load stroke function
stroke=readRDS("stroke.RDS")
for (i in 1:20)
{
# Creating s1
# x1 boundary, (3 -> 5)
x1=runif(1,2,5)
# y1 boundary (4 -> 7)
y1=runif(1,4,7)
# Length (5,13)
len1=runif(1,6,12)
# Angle (-0.1*pi -> 0*pi)
theta1=runif(1,-0.1*pi,0*pi)
# Width (0.5 -> 3)
width1=runif(1,0.5,3)

s1=stroke(x1,y1,theta1,len1,width1)

#image(c(1:16),c(1:16),s1,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

# Creating s2
# x and y points same as s1
# Length (7 -> 13)
len2=runif(1,7,16)
# Angle (0.43*pi -> 0.57*pi)
theta2=runif(1,-0.57*pi,-0.43*pi)
# Width (0.5 -> 3)
width2=runif(1,0.5,3)

s2=stroke(x1,y1,theta2,len2,width2)

#im=s1+s2
#im[im > 1]=1  #this thresholds the image so that the max brightness is 1
#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

# Creating s3
# Needs to cross s2 at about half it's length
# x2 needs to find half the distance of the length of s2, and create a point -2,+5 around that
x1e = x1 - len2*sin(theta2) # endpoint of x1
half_s2 = (x1 + x1e) %/% 2
x2=runif(1,-1+half_s2,3+half_s2)

# y2 needs to be lower than y1
y2=runif(1,y1-13,y1-2)

# Length (9 -> 13)
y1e = y1 + len1*cos(theta1) # endpoint of y1

len3=runif(1,y1-y2+((y1e+y1) %/% 2)-8,y1-y2+((y1e+y1) %/% 2)) # Add drawing explaining model
#len3=runif(1,3,9)

# Angle
theta3=runif(1,-0.05*pi,0.05*pi)

# Width (0.5 -> 3)
width3=runif(1,0.5,3)

s3=stroke(x2,y2,theta3,len3,width3)

im=s1+s2+s3
im[im > 1]=1  #this thresholds the image so that the max brightness is 1
image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

readline()
}





