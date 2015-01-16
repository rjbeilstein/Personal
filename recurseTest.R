# TODO: Add comment
# 
# Author: rjb
###############################################################################


f<-function(x) {
	 t<-x;
	 x<-x-1
	 g<-function(y) t<<-y
	 g(x)
	 if (x > 0) f(x)
	 t
	 }
f(10)

t<-17
f<-function(x) {
	x<-x-1
	g<-function(y) t<<-y
	g(x)
	if (x > 0) f(x)
	t
}
f(10)

