
y=c(13,15,11, 14,13,19)
x=1:length(y)

# and manipulated axes:
pinned.axis=list( list(x=1, y = seq(0,100,10)),
                  list(x=1.75, y=c(0,5,seq(90,100,length.out=9))),
                  list(x=2.5, y=c(0,5,seq(90,100,length.out=9))) )


WavyPlot <- function(x,y,pinned.axis){
  

LINEAR_OUTSKIRTS = TRUE


xBits = unlist(lapply(pinned.axis, function(X) X$x))
largeChange = abs(diff(range(xBits)))
leftBit = min(xBits) ; rightBit = max(xBits)
startSpread = pinned.axis[[which.min(xBits)]]$y
endSpread = pinned.axis[[which.max(xBits)]]$y
o.lim=range(startSpread)

NN = 500
if(LINEAR_OUTSKIRTS){
  pinned.axis = append(pinned.axis, 
                       list(
                         list(x=leftBit-largeChange/15,y=startSpread),
                         list(x=leftBit-largeChange/5,y=startSpread),
                         list(x=leftBit-largeChange,y=startSpread),
                         list(x=rightBit+largeChange/15,y=endSpread),
                         list(x=rightBit+largeChange/5,y=endSpread),
                         list(x=rightBit+largeChange,y=endSpread)
                       ))
  
}
# two-d splining.
n_Lines = length(pinned.axis[[2]]$y)
SOx = lapply(1:n_Lines, function(i){
  # for each horizontal gridline, make the spline object.
  xs = unlist(lapply(pinned.axis, function(XX) XX$x))
  ys = unlist(lapply(pinned.axis, function(XX) XX$y[i]))
  splinefun(x=xs,y=ys)
})

plot(0,0,col=0,xlab="",ylab="",main="",axes=FALSE, xlim=range(x)+c(-0.1,0.1), ylim=o.lim)
axis(2,at=startSpread)
lapply(1:length(startSpread), function(i){
  xs=seq(min(x)-1,max(x)+1,length.out=NN)
  lines(x=  xs , y=SOx[[i]](xs), col="gray")
})

# now for each data point, plug it into the transform.
new_y = function(xi,yi){
  # need to make another spline using all of the above.
  v_orig = startSpread
  v_new = unlist(lapply(1:n_Lines, function(j){
    SOx[[j]](xi)
  }))
  vx1=v_orig[1];vxn=v_orig[length(v_orig)]
  vy1=v_new[1];vyn=v_new[length(v_new)]
  vxr=abs(diff(c(vx1,vxn)))
  vyr=abs(diff(c(vy1,vyn)))
  
  spline(x=c(v_orig,vx1-vxr/10,vx1-vxr,vxn+vxr,vxn+vxr/10), y=c(v_new,vy1-vyr/10,vy1-vyr,vyn+vyr,vyn+vyr/10) , xout=yi)$y
}

yMod = unlist(lapply(1:length(y), function(i){
  # at point
  xi = x[i] ; yi=y[i];
  new_y(xi,yi)
}))


abline(v=x,col="gray")
points(x=x,y=yMod, pch=16, cex=1.5)

axis(1,at=x)

# then connect the dots too.
x2 =  seq(min(x),max(x),length.out=NN)
new_y2 = unlist(lapply(1:NN, function(i){
  # at point
  xi = x2[i] ; yi=approx(x=x,y=y,xout=xi)$y
  new_y(xi,yi)
}))
#black lines connect the dots:
lines(x=x2,y=new_y2 , lwd=2, xpd=TRUE)

}

WavyPlot(x,y,pinned.axis)



y=c(6,runif(4,6,8),8)   # input data points
x=1:length(y)
# and manipulated axes:
pinned.axis=list( list(x=1,   y = c(0,2,4,6,8,10)),
                  list(x=2,   y = c(0 , 1 , 2 , 3 , 9 , 10)),
                  list(x=5,   y = c(0 , 1 , 2 , 3 , 9 , 10)),
                  list(x=6.5, y = c(0,2,4,6,8,10)) )

WavyPlot(x,y,pinned.axis)
