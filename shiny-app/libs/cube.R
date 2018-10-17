# 3D phase plane analysis by the cube.R extension for grind.R
# This script defines cube() and run3d() and uses the plot3D library.
# The default projection of this library is to have the origin in
# the front-left corner, and to have y pointing backwards.
# The viewpoint is defined by the defaults theta=40 and phi=40,
# where theta gives the azimuthal direction and phi the colatitude.
# One can use plotdev(theta=60,phi=40) to change the viewpoint.
# plotdev() "refreshes" the picture (e.g., to make a pdf)
#
# The default projection of the old GRIND was to have the origin in
# the left hand back corner, with x pointing rightwards, y upwards,
# and z forwards. If you like that call cube() and run3d() with
# (x=3, y=1, z=2, theta=130, ...).
# One can set par(mar=c(0,0,0,0)) to reduce the margins.
# Rob de Boer, Utrecht University, 01-12-2017

library(plot3D)

cube <- function(x=1, y=2, z=3, xmin=0, xmax=1, ymin=0, ymax=1, zmin=0, zmax=1, log="", npixels=150, state=s, parms=p, odes=model, time=0, eps=0, show=NULL, zero=TRUE, lwd=1, ...) {
  # Make a 3D-phase space with nullclines
  dots <- list(...)
  if (!is.null(dots)) {
    unknown <- names(dots[!names(dots) %in% c(args_run,args_plot,names(formals(contour3D)),"ticktype","nticks")])
    if (length(unknown)>0) warning(paste("Unknown argument(s):",unknown,sep=" "))
  }
  if (!is.null(dots)) dots_run <- dots[names(dots) %in% args_run]
  else dots_run <- NULL
  if (!is.numeric(x)) x <- index(x,names(state))
  if (!is.numeric(y)) y <- index(y,names(state))
  if (!is.numeric(z)) z <- index(z,names(state))
  if (!is.null(show)) ishows <- index(show, names(state))
  else ishows <- c(x, y, z)
  nvar <- length(state)
  if (zero) state[1:nvar] <- rep(0,nvar)
  logx <- ifelse(grepl('x',log), TRUE, FALSE)
  logy <- ifelse(grepl('y',log), TRUE, FALSE)
  logz <- ifelse(grepl('z',log), TRUE, FALSE)
  if (logx) xc <- 10^seq(log10(xmin),log10(xmax),length.out=npixels)
  else xc <- seq(xmin+eps,xmax,length.out=npixels)
  if (logy) yc <- 10^seq(log10(ymin),log10(ymax),length.out=npixels)
  else yc <- seq(ymin+eps,ymax,length.out=npixels)
  if (logz) zc <- 10^seq(log10(zmin),log10(zmax),length.out=npixels)
  else zc <- seq(zmin+eps,zmax,length.out=npixels)
  xvar <- names(state)[x]; yvar <- names(state)[y]; zvar <- names(state)[z]
  vstate <- as.list(state)
  npixels2 <- npixels^2
  #vstate<-lapply(vstate,rep,vstate,npixels2);vstate[[x]]<-0;vstate[[y]]<-0
  for (j in seq(1,nvar)) if (j!=x & j!=y & j!=z) vstate[[j]]<-rep(vstate[[j]],npixels2);
  FUN <- function(xc, yc, zc, x, y, z, i){  # wrapper around model()
    vstate[[x]] <- xc; vstate[[y]] <- yc; vstate[[z]] <- zc
    odes(time,vstate,parms)[[1]][seq((i-1)*npixels2+1,i*npixels2)]
  }

  add <- FALSE
  for (i in ishows) {
    fc <- rep(yc[1],npixels2)
    try(contour3D(x=xc,y=fc[1],z=zc,colvar=outer(xc,zc,FUN,fc,x,z,y,i),levels=0,col=colors[i],add=add,lwd=lwd,xlim=c(xmin,xmax),ylim=c(ymin,ymax),zlim=c(zmin,zmax),xlab=names(state[x]),ylab=names(state[y]),zlab=names(state[z]),...),silent=TRUE)
    add <- TRUE
    fc <- rep(yc[npixels],npixels2)
    try(contour3D(x=xc,y=fc[1],z=zc,colvar=outer(xc,zc,FUN,fc,x,z,y,i),levels=0,col=colors[i],add=TRUE,lwd=lwd,...),silent=TRUE)

    fc <- rep(zc[1],npixels2)
    try(contour3D(x=xc,y=yc,z=fc[1],colvar=outer(xc,yc,FUN,fc,x,y,z,i),levels=0,col=colors[i],add=TRUE,lwd=lwd,...),silent=TRUE)
    fc <- rep(zc[npixels],npixels2)
    try(contour3D(x=xc,y=yc,z=fc[1],colvar=outer(xc,yc,FUN,fc,x,y,z,i),levels=0,col=colors[i],add=TRUE,lwd=lwd,...),silent=TRUE)

    fc <- rep(xc[1],npixels2)
    try(contour3D(x=fc[1],y=yc,z=zc,colvar=outer(yc,zc,FUN,fc,y,z,x,i),levels=0,col=colors[i],add=TRUE,lwd=lwd,...),silent=TRUE)
    fc <- rep(xc[npixels],npixels2)
    try(contour3D(x=fc[1],y=yc,z=zc,colvar=outer(yc,zc,FUN,fc,y,z,x,i),levels=0,col=colors[i],add=TRUE,lwd=lwd,...),silent=TRUE)
  }
}

run3d <- function(x=1, y=2, z=3, xmin=0, xmax=1, ymin=0, ymax=1, zmin=0, zmax=1, log="", col=1, add=FALSE, state=s, ...) {
  dots <- list(...)
  if (!is.null(dots)) {
    args_run3d <- c(names(formals(scatter3D)),"ticktype","nticks")
    unknown <- names(dots[!names(dots) %in% c(args_run,args_run3d)])
    if (length(unknown)>0) warning(paste("Unknown argument(s):",unknown,sep=" "))
    dots_run <- dots[names(dots) %in% args_run]
    dots_l3d <- dots[names(dots) %in% args_run3d]
  }else dots_run <- NULL
  data <- do.call('run',c(list(state=state,timeplot=FALSE,table=TRUE),dots_run))
  if (!add) do.call('lines3D',c(list(x=data[,1+x],y=data[,1+y],z=data[,1+z],xlim=c(xmin,xmax),ylim=c(ymin,ymax),zlim=c(zmin,zmax),xlab=names(state[x]),ylab=names(state[y]),zlab=names(state[z]),col=col),dots_l3d))
  else do.call('lines3D',c(list(x=data[,1+x],y=data[,1+y],z=data[,1+z],col=col,add=TRUE),dots_l3d))
}

# cat("cube.R was sourced\n")
