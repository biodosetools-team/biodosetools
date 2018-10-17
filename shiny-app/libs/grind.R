# Phase plane analysis in R: grind.R
# Rob de Boer, Utrecht University, 10-12-2017

library(deSolve)     # run() calls the ode() function
library(rootSolve)   # newton() and continue() call steady()
library(FME)         # fit() calls modFit() and modCost()

options(stringsAsFactors=FALSE)
colors <- c("red","blue","darkgreen","darkorange","darkmagenta","gold","darkorchid","aquamarine","deeppink","gray",seq(2,991))
#colors <- seq(2,101)  # Use standard R colors
ncolors <- length(colors)
sizeLegend <- 0.75    # legend size is 75% of the default value in R
font.main <- 1        # plain (default is bold: 2)
font.sub  <- 1        # plain (default is bold: 2)

plane <- function(xmin=0, xmax=1.1, ymin=0, ymax=1.1, log="", npixels=500, state=s, parms=p, odes=model, x=1, y=2, time=0, grid=5, eps=0, show=NULL, portrait=FALSE, vector=FALSE, add=FALSE, legend=TRUE, zero=TRUE, lwd=2, col="black", pch=20, ...) {
  # Make a phase plane with nullclines and/or phase portrait
  dots <- list(...)
  if (!is.null(dots)) {
    unknown <- names(dots[!names(dots) %in% c(args_run,args_plot)])
    if (length(unknown)>0) warning(paste("Unknown argument(s):",unknown,sep=" "))
  }
  if (!is.null(dots)) dots_run <- dots[names(dots) %in% args_run]
  else dots_run <- NULL
  if (!is.numeric(x)) x <- index(x,names(state))
  if (!is.numeric(y)) y <- index(y,names(state))
  if (!is.null(show)) ishows <- index(show, names(state))
  else ishows <- c(x, y)
  nvar <- length(state)
  if (zero) state[1:nvar] <- rep(0,nvar)
  lvec <- 50                         # length of vector
  logx <- ifelse(grepl('x',log), TRUE, FALSE)
  logy <- ifelse(grepl('y',log), TRUE, FALSE)
  if (logx) xc <- 10^seq(log10(xmin),log10(xmax),length.out=npixels)
  else xc <- seq(xmin+eps,xmax,length.out=npixels)
  if (logy) yc <- 10^seq(log10(ymin),log10(ymax),length.out=npixels)
  else yc <- seq(ymin+eps,ymax,length.out=npixels)
  xvar <- names(state)[x]; yvar <- names(state)[y]
  if (!add) {
    do.call('plot',c(list(1,1,type='n',xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab=xvar,ylab=yvar,log=log,font.main=font.main,font.sub=font.sub),dots[names(dots) %in% args_plot]))
    if (legend)
      legend("topright",legend=names(state)[ishows],col=colors[ishows],lty=1,lwd=lwd,cex=sizeLegend)
  }

  vstate <- as.list(state)
  npixels2 <- npixels^2
  #vstate<-lapply(vstate,rep,vstate,npixels2);vstate[[x]]<-0;vstate[[y]]<-0
  for (j in seq(1,nvar)) if (j!=x & j!=y) vstate[[j]]<-rep(vstate[[j]],npixels2);
  FUN <- function(xc, yc, i){  # wrapper around model()
    vstate[[x]] <- xc; vstate[[y]] <- yc
    odes(time,vstate,parms)[[1]][seq((i-1)*npixels2+1,i*npixels2)]
  }
  for (i in ishows)
    contour(xc,yc,outer(xc,yc,FUN,i),levels=0,drawlabels=FALSE,add=TRUE,col=colors[i],lwd=lwd)

  if (portrait | vector) {
    if (logx) {dx <- (log10(xmax)-log10(xmin))/grid; vx <- 1+3.32*grid*dx/lvec}
    else {dx <- (xmax-xmin)/grid; vx = grid*dx/lvec}
    if (logy) {dy <- (log10(ymax)-log10(ymin))/grid; vy <- 1+3.32*grid*dy/lvec}
    else {dy <- (ymax-ymin)/grid; vy = grid*dy/lvec}

    for (i in seq(1,grid)) {
      if (logx) state[x] <- 10^((i-1)*dx + dx/2 + log10(xmin))
      else state[x] <- (i-1)*dx + dx/2 + xmin
      for (j in seq(1,grid,1)) {
        if (logy) state[y] <- 10^((j-1)*dy + dy/2 + log10(ymin))
        else state[y] <- (j-1)*dy + dy/2 + ymin
        if (portrait) {
          points(state[x],state[y],pch=pch)
          nsol <- do.call('run',c(list(state=state,parms=parms,odes=odes,timeplot=FALSE,table=TRUE),dots_run))
          lines(cbind(nsol[x+1],nsol[y+1]),col=col)
        }
        if (vector) {
          dt <- sign(unlist(odes(time,state,parms)))
          if (logx) lines(c(state[x],state[x]*vx^dt[x]), c(state[y],state[y]))
          else lines(c(state[x],state[x]+vx*dt[x]), c(state[y],state[y]))
          if (logy) lines(c(state[x],state[x]), c(state[y],state[y]*vy^dt[y]))
          else lines(c(state[x],state[x]), c(state[y],state[y]+vy*dt[y]))
        }
      }
    }
  }
}

run <- function(tmax=100, tstep=1, state=s, parms=p, odes=model, ymin=0, ymax=NULL, log="", x=1, y=2, xlab="Time", ylab="Density", tmin=0, draw=lines, times=NULL, show=NULL, arrest=NULL, after=NULL, tweak=NULL, timeplot=TRUE, traject=FALSE, table=FALSE, add=FALSE, legend=TRUE, solution=FALSE, delay=FALSE, lwd=2, col="black", pch=20, ...) {
  # run model and make a table, time plot, or trajectory
  if (delay & (solution | !is.null(after))) stop("Don't use solution or after with delay equations")
  dots <- list(...)
  if (!is.null(dots)) {
    unknown <- names(dots[!names(dots) %in% c(args_run,args_plot)])
    if (length(unknown)>0) warning(paste("Unknown argument(s):",unknown,sep=" "))
    dots_run <- dots[names(dots) %in% args_run]
  }else dots_run <- NULL
  nvar <- length(state)
  if (!is.numeric(x)) x <- index(x,names(state))
  if (!is.numeric(y)) y <- index(y,names(state))
  #if (!is.null(show)) ishows <- index(show, names(state))
  #else ishows <- seq(1,nvar)
  if (is.null(times)) times <- seq(tmin,tmax,by=tstep)
  else {
    times <- sort(times)
    tmin <- min(times)
    tmax <- max(times)
  }
  if (!is.null(arrest)) {
    if (is.numeric(arrest)) times <- sort(c(arrest,times))
    else times <- sort(c(as.numeric(parms[arrest]),times))
    times <- unique(round(times,8))
  }
  if (solution) {                              # Run in one go
    if (!is.null(after)) stop("Don't combine the option after with solution")
    nsol <- sapply(times,odes,state,parms)
    if (length(state) > 1) nsol <- data.frame(times,t(nsol))
    else nsol <- data.frame(times,nsol)
    names(nsol) <- c("time",names(state))
  }else{
    if (is.null(after)){                       # Run in one go
      if (!delay) nsol <- as.data.frame(do.call('ode',c(list(times=times,func=odes,y=state,parms=parms),dots_run)))
      else nsol <- as.data.frame(do.call('dede',c(list(times=times,func=odes,y=state,parms=parms),dots_run)))
    }else{                                    # Run many in steps
      t0 <- times[1]
      for (t in times[2:length(times)]) {
        nsol0 <- do.call('ode',c(list(times=c(t0,t),func=odes,y=state,parms=parms),dots_run))
        if (t0 == times[1]) nsol <- nsol0
        state <- nsol0[2,2:(nvar+1)]
        if (!is.null(after)) eval(parse(text=after))
        if (t0 == times[1]) nsol[2,2:(nvar+1)] <- state
        else nsol <- rbind(nsol,c(t,state))
        t0 <- t
      }
      nsol <- as.data.frame(nsol)
    }
  }
  if (!is.null(tweak)) eval(parse(text=tweak))
  if (timeplot & !traject)
    do.call('timePlot',c(list(data=nsol,tmin=tmin,tmax=tmax,ymin=ymin,ymax=ymax,log=log,add=add,xlab=xlab,ylab=ylab,show=show,draw=draw,lwd=lwd,legend=legend,font.main=font.main,font.sub=font.sub),dots[names(dots) %in% args_plot]))
  if (traject) {
    points(nsol[1,x+1],nsol[1,y+1],pch=pch)
    lines(nsol[,x+1],nsol[,y+1],lwd=lwd,col=col)
  }
  if (table) return(nsol)
  f <- state
  f[1:length(f)] <- as.numeric(nsol[nrow(nsol),2:(nvar+1)])
  return(f)
}

newton <- function(state=s, parms=p, odes=model, time=0, x=1, y=2, positive=FALSE, jacobian=FALSE, vector=FALSE, plot=FALSE, ...) {
  # find a steady state
  if (!is.numeric(x)) x <- index(x,names(state))
  if (!is.numeric(y)) y <- index(y,names(state))
  q <- steady(y=state,fun=odes,parms=parms,time=time,positive=positive, ...)
  if (attr(q,"steady")) {
    equ <- q$y
    equ <- ifelse(abs(equ) < 1e-8, 0, equ)
    print(equ)
    jac <- jacobian.full(y=equ,fun=odes,parms=parms)
    eig <- eigen(jac)
    dom <- max(sort(Re(eig$values)))
    if (dom < 0) cat("Stable point, ")
    else cat("Unstable point, ")
    cat("eigenvalues: ",eig$values,"\n")
    if (vector) {cat("Eigenvectors:\n"); print(eig$vectors)}
    if (jacobian) {cat("Jacobian:\n"); print(jac)}
    if (plot) {
      if (dom < 0) points(equ[x],equ[y],pch=19)
      else points(equ[x],equ[y],pch=1)
    }
    return(equ)
  }
  cat("No convergence: start closer to a steady state")
  return(NULL)
}

continue <- function(state=s, parms=p, odes=model, x=1, step=0.01, xmin=0, xmax=1, y=2, ymin=0, ymax=1.1, log="", time=0, positive=FALSE, add=FALSE, ...) {
  # continue a steady state
  dots <- list(...)
  if (!is.null(dots)) {
    unknown <- names(dots[!names(dots) %in% c(args_steady,args_plot)])
    if (length(unknown)>0) warning(paste("Unknown argument(s):",unknown,sep=" "))
    dots_steady <- dots[names(dots) %in% args_steady]
  }else dots_steady <- NULL
  if (!is.numeric(x)) x <- index(x,names(parms))
  if (!is.numeric(y)) y <- index(y,names(state))
  logx <- ifelse(grepl('x',log), TRUE, FALSE)
  clrs <- c("red","black","blue")
  lwds <- c(2,1,1)
  if (missing(xmax) & parms[x] >= 1) xmax <- 2*parms[x]
  if (missing(xmin) & parms[x] < 0) xmin <- 2*parms[x]
  if (!missing(xmin) & xmin >= parms[x]) stop("xmin should be smaller than parameter")
  if (!missing(xmax) & xmax <= parms[x]) stop("xmax should be larger than parameter")

  FUN <- function(lastState,lastDom,step) {
    lastP <- p0
    preLastState <- lastState
    nok <- 0
    while (xmin < lastP & lastP < xmax & ymin < lastState[y] & lastState[y] < ymax) {
      if (logx) parms[x] <- lastP*(1+step)
      else parms[x] <- lastP + step
      #predState <- lastState + (lastState-preLastState)
      q <- do.call('steady',c(list(y=lastState,fun=odes,parms=parms,time=time,positive=positive),dots_steady))
      newState <- q$y  # should be steady state and closeby
      if (attr(q,"steady") & abs(sum(newState-lastState))/(1e-9+abs(sum(lastState))) < 0.1) {
        jac <- jacobian.full(y=newState,fun=odes,parms=parms)
        dom <- sign(max(sort(Re(eigen(jac)$values))))
        if (dom != lastDom) cat("Bifurcation at",names(parms[x]),"=",parms[x],"\n")
        if (logx) lines(c(parms[x]/(1+step),parms[x]),c(lastState[y],newState[y]), col=clrs[dom+2],lwd=lwds[dom+2])
        else lines(c(parms[x]-step,parms[x]),c(lastState[y],newState[y]), col=clrs[dom+2],lwd=lwds[dom+2])
        preLastState <- lastState
        lastState <- newState
        lastDom <- dom
        lastP <- parms[x]
        if (nok > 10 & abs(step) < actualStep) step <- sign(step)*min(2*abs(step),actualStep)
        nok <- nok + 1
      }else{
        nok <- 0
        if (abs(step) > actualStep/100) step <- step/2
        else{ # Go back one step, overpredict, call steady, and turn
          parms[x] <- lastP
          predState <- lastState + 5*(lastState-preLastState)
          q <- do.call('steady',c(list(y=predState,fun=odes,parms=parms,time=time,positive=positive),dots_steady))
          newState <- q$y  # should be steady state and not the same
          #print(c(lastState,predState,newState,parms[x]))
          if (attr(q,"steady") & abs(sum(newState-lastState))/(1e-9+abs(sum(lastState))) > 0.001) {
            cat("Turning point point at",names(parms[x]),"=",parms[x],"\n")
            jac <- jacobian.full(y=newState,fun=odes,parms=parms)
            dom <- sign(max(sort(Re(eigen(jac)$values))))
            middle <- (lastState[y]+newState[y])/2
            lines(c(parms[x],parms[x]),c(lastState[y],middle), col=clrs[lastDom+2],lwd=lwds[lastDom+2])
            lines(c(parms[x],parms[x]),c(middle,newState[y]), col=clrs[dom+2],lwd=lwds[dom+2])
            step <- -step
            preLastState <- lastState
            lastState <- newState
            lastDom <- dom
            lastP <- parms[x]
          }else{
            cat("Final point at",names(parms[x]),"=",parms[x],"\n")
            cat("If this looks wrong try changing the step size\n")
            break
          }
        }
      }
    }
  }
  p0 <- parms[x]
  q0 <- do.call('steady',c(list(y=state,fun=odes,parms=parms,time=time,positive=positive),dots_steady))

  if (attr(q0,"steady")) {
    cat("Starting at",names(parms[x]),"=",parms[x],"with:\n")
    print(q0$y)
    bary <- q0$y[y]
    if (missing(ymax) & bary >= 1.1) ymax <- 2*bary
    if (missing(ymin) & bary < 0) ymin <- 2*bary
    if (!missing(ymin) & ymin >= bary) stop("ymin should be smaller than y-variable")
    if (!missing(ymax) & ymax <= bary) stop("ymax should be larger than y-variable")
    if (!add)
      do.call('plot',c(list(1,1,type='n',xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab=names(p0),ylab=names(state)[y],log=log,font.main=font.main,font.sub=font.sub),dots[names(dots) %in% args_plot]))
    orgWarn <- getOption("warn")
    options(warn = -1)
    jac <- jacobian.full(y=q0$y,fun=odes,parms=parms)
    dom <- sign(max(sort(Re(eigen(jac)$values))))
    if (logx) actualStep <- step
    else actualStep <- step*xmax
    FUN(lastState=q0$y,lastDom=dom,actualStep)
    FUN(lastState=q0$y,lastDom=dom,-actualStep)
    options(warn = orgWarn)
  } else cat("No convergence: start closer to a steady state")
}

fit <- function(datas=data, state=s, parms=p, odes=model, free=NULL, who=NULL, differ=NULL, fixed=NULL, tmin=0, tmax=NULL, ymin=NULL, ymax=NULL, log="", xlab="Time", ylab="Density", bootstrap=0, show=NULL, fun=NULL, costfun=cost, initial=FALSE, add=FALSE, timeplot=TRUE, legend=TRUE, main=NULL, sub=NULL, pchMap=NULL, ...) {
  dots <- list(...)
  if (!is.null(dots)) {
    unknown <- names(dots[!names(dots) %in% c(args_fit,args_run,args_plot)])
    if (length(unknown)>0) warning(paste("Unknown argument(s):",unknown,sep=" "))
    dots_run <- dots[names(dots) %in% args_run]
    dots_fit <- dots[names(dots) %in% c(args_fit,args_run)]
    if ("method" %in% names(dots)) {
      if (!(dots$method %in% c(methods_run,methods_fit)))
        stop(paste("Unknown method:",dots$method))
      if (!(dots$method %in% methods_fit)) {
        dots_fit[["run_method"]] <- dots$method  # Used in cost
        dots_fit[["method"]] <- NULL
      }
      if (!(dots$method %in% methods_run))
        dots_run[["method"]] <- NULL
    }
  }
  if (is.null(free) & !is.null(who)) free <- who  # for compatability
  if (!is.null(fun)) fun <- match.fun(fun)
  if (is.data.frame(datas)) datas <- list(datas)
  nsets <- length(datas)
  all <- c(state,parms);  allNms <- names(all)
  if (initial) totp <- parms else totp <- all
  isVar <- setNames(c(rep(TRUE,length(state)),rep(FALSE,length(parms))),allNms)
  if (is.null(free) & is.null(differ)) free <- allNms
  if (initial) free <- idrop(names(state),free)   # remove state from free
  ifree <- index(free,names(totp))                # Check if free is correct
  if (!is.null(fixed))
    if (!is.list(fixed)) stop("fixed should be a list")
    else {
      if (length(intersect(names(fixed),free)) > 0) stop("fixed should not overlap with free")
      if (length(intersect(names(fixed),differ)) > 0) stop("fixed should not overlap with differ")
    }
  if (is.null(differ)) guess <- setNames(rep(0,length(free)),free)
  else {
    if (!is.list(differ)) ldiff <- makelist(differ,state=state,parms=parms,nsets=nsets)
    else {ldiff <- differ; differ <- names(ldiff)}
    free <- idrop(differ,free)                      # remove differ from free
    guess <- setNames(rep(0,length(free)+nsets*length(differ)), c(free,rep(differ,nsets)))
  }
  lenfree <- length(free)
  lendiff <- length(differ)
  VarsFree <- free[isVar[free]]
  ParsFree <- free[!isVar[free]]
  if (length(VarsFree) > 0) guess[VarsFree] <- state[VarsFree]
  if (length(ParsFree) > 0) guess[ParsFree] <- parms[ParsFree]
  if (!is.null(differ))
    for (inum in seq(lendiff))
    for (iset in seq(nsets))
      guess[lenfree+inum+(iset-1)*lendiff] <- ldiff[[differ[inum]]][iset]
  logy <- ifelse(grepl('y',log), TRUE, FALSE)

  f <- do.call('modFit',c(list(f=costfun,p=guess,datas=datas,odes=odes,state=state,parms=parms,free=free,differ=differ,fixed=fixed,fun=fun,initial=initial,isVar=isVar),dots_fit))
  found <- f$par
  cat("SSR:",f$ssr," Estimates:\n")
  print(found)
  if (timeplot) {         # We are done, now plot the results
    tmaxn <- ifelse(is.null(tmax),max(unlist(lapply(seq(nsets),function(i){max(datas[[i]][1])}))),tmax)
    ymaxn <- ifelse(is.null(ymax),max(unlist(lapply(seq(nsets),function(i){max(datas[[i]][2:ncol(datas[[i]])])}))),ymax)
    yminn <- ifelse(is.null(ymin),min(unlist(lapply(seq(nsets),function(i){min(datas[[i]][2:ncol(datas[[i]])])}))),ymin)
  for (iset in seq(nsets)) {
    data <- datas[[iset]]
    if (length(VarsFree) > 0) state[VarsFree] <- found[VarsFree]
    if (length(ParsFree) > 0) parms[ParsFree] <- found[ParsFree]

    if (!is.null(fixed))
      for (inum in seq(length(fixed))) {
        name <- names(fixed)[inum]
        if (isVar[name]) state[match(name,names(state))] <- fixed[[inum]][iset]
        else parms[match(name,names(parms))] <- fixed[[inum]][iset]
      }
    if (!is.null(differ))
      for (i in seq(lendiff)) { # Copy found[iset] into state and parms
        value <- found[lenfree+i+(iset-1)*lendiff]
        if (isVar[differ[i]]) state[match(differ[i],names(state))] <- value
        else parms[match(differ[i],names(parms))] <- value
    }
    if (initial) {
      if (data[1,1] > 0) stop("Data doesn't start at time=0")
      state[1:length(state)] <- unlist(data[1,2:ncol(data)])
    }
    tmaxi <- ifelse(is.null(tmax),ifelse(add,tmaxn,max(data[,1])),tmax)
    nsol <- do.call('run',c(list(tmax=tmaxi,state=state,parms=parms,odes=odes,table=TRUE,timeplot=FALSE),dots_run))
    ymaxi <- ifelse(is.null(ymax),ifelse(add,ymaxn,max(data[2:ncol(data)],nsol[2:ncol(nsol)])),ymax)
    ymini <- ifelse(is.null(ymin),ifelse(add,yminn,min(data[2:ncol(data)],nsol[2:ncol(nsol)])),ymin)
    solnames <- names(nsol)[2:ncol(nsol)]
    colnames <- names(data)[2:ncol(data)]
    imain <- main[min(length(main),iset)]
    isub <- sub[min(length(sub),iset)]
    if (is.null(show)) {
      timePlot(nsol,tmin=tmin,tmax=tmaxi,ymin=ymini,ymax=ymaxi,log=log,main=imain,sub=isub,add=ifelse(iset>1,add,FALSE),xlab=xlab,ylab=ylab,font.main=font.main,font.sub=font.sub,legend=legend)
      timePlot(data,draw=points,add=TRUE,legend=FALSE,lwd=1.5,colMap=index(colnames,solnames),pchMap=pchMap)
    }else{
      for (i in show) {
        timePlot(nsol,tmin=tmin,tmax=tmaxi,ymin=ymini,ymax=ymaxi,log=log,show=i,main=imain,sub=isub,xlab=xlab,ylab=ylab,font.main=font.main,font.sub=font.sub,legend=legend)
        if (i %in% colnames)
          timePlot(data,draw=points,add=TRUE,legend=FALSE,lwd=1.5,show=i,colMap=index(colnames,solnames),pchMap=pchMap)
      }
    }
  }}
  if (bootstrap == 0) return(f)
  imat <- sapply(seq(bootstrap), function(i) {
    samples <- lapply(seq(nsets),function(j){datas[[j]][sample(nrow(datas[[j]]),replace=TRUE),]})
    ifit <- do.call('modFit',c(list(f=costfun,p=found,datas=samples,odes=odes,state=state,parms=parms,free=free,differ=differ,fixed=fixed,fun=fun,initial=initial,isVar=isVar),dots_fit))
    ifit$par
  })
  print(apply(imat, 1, function(i)c(mean=mean(i),sd=sd(i),median=median(i),quantile(i,c(.025, .975)))))
  f$bootstrap <- t(imat)
  return(f)
}

cost <- function(datas, odes, state, parms, guess, free, differ, fixed, fun, initial, isVar, ...) {
  dots <- list(...)
  if (!is.null(dots)) {
    dots_run <- dots[names(dots) %in% args_run]
    dots_fit <- dots[names(dots) %in% args_fit]
    if ("run_method" %in% names(dots)) dots_run[["method"]] <- dots$run_method
  }
  if (!is.null(fun)) fun <- match.fun(fun)
  VarsFree <- free[isVar[free]]
  ParsFree <- free[!isVar[free]]
  lenfree <- length(free)
  lendiff <- length(differ)
  if (length(VarsFree) > 0) state[VarsFree] <- guess[VarsFree]
  if (length(ParsFree) > 0) parms[ParsFree] <- guess[ParsFree]
  nsets <- length(datas)
  totcost <- NULL
  for (iset in seq(nsets)) {
    data <- datas[[iset]]
    if (initial) {
      if (data[1,1] > 0) stop("Data doesn't start at time=0: data[",iset,"]")
      state[1:length(state)] <- unlist(data[1,2:ncol(data)])
    }
    if (!is.null(fixed))
      for (inum in seq(length(fixed))) {
        name <- names(fixed)[inum]
        if (isVar[name]) state[match(name,names(state))] <- fixed[[inum]][iset]
        else parms[match(name,names(parms))] <- fixed[[inum]][iset]
      }
    if (!is.null(differ)) for (i in seq(lendiff)) {
        value <- guess[lenfree+i+(iset-1)*lendiff]
        if (isVar[differ[i]]) state[match(differ[i],names(state))] <- value
        else parms[match(differ[i],names(parms))] <- value
    }
    times <- sort(unique(data[,1]))
    if (!(0 %in% times)) times <- c(0,times)
    nsol <- do.call('run',c(list(times=times,state=state,parms=parms,odes=odes,timeplot=FALSE,table=TRUE),dots_run))
    if (!is.null(fun)) {
      data[2:ncol(data)]=fun(data[2:ncol(data)])
      nsol[2:ncol(nsol)]=fun(nsol[2:ncol(nsol)])
    }
    totcost <- do.call('modCost',c(list(model=nsol,obs=data,cost=totcost),dots_fit))
  }
  return (totcost)
}

timePlot <- function(data, tmin=0, tmax=NULL, ymin=0, ymax=NULL, log="", xlab="Time", ylab="Density", show=NULL, legend=TRUE, draw=lines, lwd=2, add=FALSE, main=NULL, sub=NULL, recolor=FALSE, colMap=NULL, pchMap=NULL, ...) {
  if (!is.null(draw)) draw <- match.fun(draw)
  if (is.null(tmax)) tmax <- max(data[,1])
  if (is.null(ymax)) ymax <- max(data[,2:ncol(data)])
  if (tmin == 0 & grepl('x',log)) tmin <- min(data[,1])
  if (ymin == 0 & grepl('y',log)) ymin <- min(data[,2:ncol(data)])
  if (!add)
    plot(1,1,type='n',xlim=c(tmin,tmax),ylim=c(ymin,ymax),log=log,xlab=xlab,ylab=ylab,main=main,sub=sub,font.main=font.main,font.sub=font.sub,...)
  colnames <- names(data)[2:ncol(data)]
  if (!is.null(show)) ishows <- index(show, colnames)
  else ishows <- seq(ncol(data)-1)

  if (recolor) {
    for (i in seq(length(ishows)))
      draw(data[,1],data[,ishows[i]+1],col=colors[i],lwd=lwd,pch=i)
    if (legend) {
      if (identical(draw,lines)) legend("topright",legend=colnames[ishows],col=colors,lty=1,lwd=lwd,cex=sizeLegend)
      else legend("topright",legend=colnames[ishows],col=colors,lty=1,lwd=lwd,cex=sizeLegend,pch=1:100)
    }
    return()
  }

  for (i in ishows) {
    j <- ifelse(is.null(colMap),i,colMap[i])
    k <- ifelse(is.null(pchMap),j,pchMap[i])
    draw(data[,1],data[,i+1],col=colors[min(j,ncolors)],lwd=lwd,pch=k)
  }
  if (legend) {
    if (identical(draw,lines)) legend("topright",legend=colnames[ishows],col=colors[ishows],lty=1,lwd=lwd,cex=sizeLegend)
    else legend("topright",legend=colnames[ishows],col=colors[ishows],lty=1,lwd=lwd,cex=sizeLegend,pch=ishows)
  }
}

index <- function(strings, names, error=TRUE) {   # Return indices of strings in names
  hit <- strings %in% names
  if (error & length(strings[!hit] > 0)) stop("Unknown: ", paste(strings[!hit],collapse=", "))
  m <- match(strings[hit], names)
  if (length(m) > 0) return(m)
  return(NULL)
}

idrop <- function(strings, names) {            # Remove all strings from names
  hit <- strings %in% names
  m <- match(strings[hit], names)
  if (length(m) == length(names)) return(NULL)
  if (length(m) > 0) return(names[-m])
  return(names)
}

makelist <- function(strings,state=s,parms=p,nsets=1) { # Make a list with nsets default values
  all <- c(state,parms)
  nms <- names(all)
  hit <- strings %in% nms
  if (length(strings[!hit] > 0))
    stop("Unknown: ", paste(c(strings[!hit]),collapse=", "))
  lst <- as.list(all[match(strings[hit], nms)])
  return(lapply(lst,rep,lst,nsets))
}

args_plot <- names(formals(plot.default))
args_fit <- unique(names(c(formals(modFit),formals(modCost))))
args_run <- unique(names(c(formals(run),formals(ode),formals(dede),formals(lsoda))))
args_steady <- unique(names(c(formals(steady),formals(stode))))
methods_run <- as.character(formals(ode)$method)
methods_fit <- as.character(formals(modFit)$method)

# cat("grind.R was sourced\n")
