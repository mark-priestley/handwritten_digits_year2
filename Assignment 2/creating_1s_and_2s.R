#THIS IS THE CODE USED IN THE VIDEO FROM March 15 2021

#this code generates random sevens
#it is an edited version of the code from "random digits.R"
#which generated random ones
#the sevens have three strokes whereas the ones had two strokes
setwd("C:/Users/User/Desktop/ca274")
stroke=readRDS("final_assessment/stroke.rds")

b=matrix(0,256,10000)
for(i in c(1:10000))
{
#this code generates random values for each argument of stroke
#this is the upper stroke
x1=runif(1,12,16)
y1=runif(1,12,16)
theta1 = runif(1,pi/2,pi*0.65)
len=20
width1=runif(1,0.5,3)

#this is the lower stroke
x2=runif(1,12,16)
y2=runif(1,12,16)
theta2 = runif(1,pi*0.75,pi)
len=20
width2=runif(1,0.5,3)

#this is the cross-stroke
x3=runif(1,13,16)
y3=runif(1,7,9)
theta3 = runif(1,pi*0.45,pi*0.55)
len3=runif(1,8,16)
width3=runif(1,0.5,3)


s1=stroke(x1,y1,theta1,20,width1)
s2=stroke(x2,y2,theta2,20,width2)
s3=stroke(x3,y3,theta3,len3,width3)

im=s1+s2+s3
im[im > 1]=1

#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

im = im*256
im = im[,16:1] #we have to turn the image upside-down so that it matches the real digits

im=matrix(im,256,1)

#this is where the new random digit is added to matrix b
b[,i]=im

#readline()
}
b7= b #we save b as b7 for later use (see below)
saveRDS(b7,"b7.rds")

b=matrix(0,256,10000) #in the lecture I used values of 10,000 and 100,000
proc.time()
for(i in c(1:100000))
{
#this code generates random values for each argument of stroke
x1=runif(1,12,16)
y1=runif(1,12,16)
theta1 = runif(1,pi/2,pi*0.75)
len=20
width1=runif(1,0.5,3) #in the first lecture the upper limit was 2

x2=runif(1,12,16)
y2=runif(1,12,16)
theta2 = runif(1,pi*0.75,pi*1.1)#in the first lecture the upper limit was pi

len=20
width2=runif(1,0.5,2)

s1=stroke(x1,y1,theta1,20,width1)
s2=stroke(x2,y2,theta2,20,width2)

im=s1+s2
im[im > 1]=1

#image(c(1:16),c(1:16),im,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

im = im*256
im = im[,16:1] #we have to turn the image upside-down so that it matches the real digits

im=matrix(im,256,1)

#this is where the new random digit is added to matrix b
b[,i]=im


#readline()
}
proc.time()

saveRDS(b,"b1.rds")