#	Generally leaps function will not do extensive search but here I modified
# 	such that it will do extensive search. I included "really.big=TRUE"
#	  Also this program limits the number of variables to be "4" 
#	  by changing nvmax=4
leaps=
function (x, y, wt = rep(1, NROW(x)), int = TRUE, method = c("Cp", 
    "adjr2", "r2"), nbest = 10, names = NULL, df = NROW(x), strictly.compatible = TRUE) 
{
    if (!is.logical(int)) 
        stop("int should be TRUE or FALSE")
    if (!is.null(names)) 
        colnames(x) <- names
    method <- method[1]
    if (pmatch(method, c("Cp", "adjr2", "r2"), nomatch = 0) == 
        0) 
        stop("Ambiguous or unrecognised method name")
    if (strictly.compatible) {
        if (NCOL(x) > 31) 
            stop("leaps does not allow more than 31 variables; use regsubsets()")
        if (is.null(names)) 
            colnames(x) <- c(as.character(1:9), LETTERS)[1:NCOL(x)]
    }
    a <- leaps:::leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = 5, 
		intercept = int, warn.dep = FALSE)
    if (strictly.compatible & any(a$lindep)) 
        stop("leaps requires full-rank design matrix; use regsubsets()")
    b <- leaps:::leaps.exhaustive(a,really.big=TRUE)
    d <- summary(b)
    rval <- list(which = d$which)
    if (int) 
        rval$which <- rval$which[, -1, drop = FALSE]
    rval$label <- colnames(d$which)
    rval$size <- as.numeric(rownames(d$which)) + int
    if (pmatch(method, c("Cp"), nomatch = 0) == 1) {
        rval$Cp <- d$cp
    }
    if (pmatch(method, c("r2"), nomatch = 0) == 1) {
        rval$r2 <- d$rsq
    }
    if (pmatch(method, c("adjr2"), nomatch = 0) == 1) {
        rval$adjr2 <- d$adjr2
    }
    rval
}

gcvplot=function (..., alpha, df = 2) 
{
    m <- match.call()
    m[[1]] <- as.name("gcv")
    m$df <- NULL
    if (!is.matrix(alpha)) 
        alpha <- matrix(alpha, ncol = 1)
    k <- nrow(alpha)
    z <- matrix(nrow = k, ncol = 4)
    for (i in 1:k) {
        m$alpha <- alpha[i, ]
        z[i, ] <- eval(m, sys.frame(sys.parent()))
    }
    ret <- list(alpha = alpha, cri = "GCV", df = z[, df], values = z[, 
        4])
    #class(ret) <- "gcvplot"
    ret
}
