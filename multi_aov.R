multi_aov <- function(Y, X, paired = FALSE, p.adj = "none") { #X and Y are vectors
  aa <- levels(as.factor(X))
  an <- as.character(c(1:length(aa)))
  tt1 <- matrix(nrow = length(aa), ncol = 6)    
  for (i in 1:length(aa))
  {
    temp <- Y[X == aa[i]]
    tt1[i, 1] <- mean(temp, na.rm = TRUE)
    tt1[i, 2] <- sd(temp, na.rm = TRUE) / sqrt(length(temp))
    tt1[i, 3] <- sd(temp, na.rm = TRUE)
    tt1[i, 4] <- min(temp, na.rm = TRUE)
    tt1[i, 5] <- max(temp, na.rm = TRUE)
    tt1[i, 6] <- length(temp)
  }
  tt1 <- data.frame(aa, tt1)
  colnames(tt1) <- c("group", "mean", "se", "sd", "min", "max", "n")
  require(agricolae)
  Xn <- factor(X, labels = an)
  sig <- "ns"
  model <- aov(Y ~ Xn)    
  if (paired == TRUE & length(aa) == 2)
  {
    coms <- t.test(Y ~ Xn, paired = TRUE)
    pp <- coms$p.value
  }    else
  {
    pp <- anova(model)$Pr[1]
  }    
  if (pp <= 0.1)
    sig <- "."
  if (pp <= 0.05)
    sig <- "*"
  if (pp <= 0.01)
    sig <- "**"
  if (pp <= 0.001)
    sig <- "***"
  if(pp <= 0.05) {
    comp <- LSD.test(model,
                     "Xn",
                     alpha = 0.05,
                     p.adj = "none",
                     group = TRUE)
    gror <- comp$groups[order(rownames(comp$groups)), ]
    tt1$sig <- gror$groups
  }
  list(comparison = tt1, p.value = pp)
}multi_aov <- function(Y, X, paired = FALSE, p.adj = "none") { #X and Y are vectors
  aa <- levels(as.factor(X))
  an <- as.character(c(1:length(aa)))
  tt1 <- matrix(nrow = length(aa), ncol = 6)    
  for (i in 1:length(aa))
  {
    temp <- Y[X == aa[i]]
    tt1[i, 1] <- mean(temp, na.rm = TRUE)
    tt1[i, 2] <- sd(temp, na.rm = TRUE) / sqrt(length(temp))
    tt1[i, 3] <- sd(temp, na.rm = TRUE)
    tt1[i, 4] <- min(temp, na.rm = TRUE)
    tt1[i, 5] <- max(temp, na.rm = TRUE)
    tt1[i, 6] <- length(temp)
  }
  tt1 <- data.frame(aa, tt1)
  colnames(tt1) <- c("group", "mean", "se", "sd", "min", "max", "n")
  require(agricolae)
  Xn <- factor(X, labels = an)
  sig <- "ns"
  model <- aov(Y ~ Xn)    
  if (paired == TRUE & length(aa) == 2)
  {
    coms <- t.test(Y ~ Xn, paired = TRUE)
    pp <- coms$p.value
  }    else
  {
    pp <- anova(model)$Pr[1]
  }    
  if (pp <= 0.1)
    sig <- "."
  if (pp <= 0.05)
    sig <- "*"
  if (pp <= 0.01)
    sig <- "**"
  if (pp <= 0.001)
    sig <- "***"
  if(pp <= 0.05) {
    comp <- LSD.test(model,
                     "Xn",
                     alpha = 0.05,
                     p.adj = "none",
                     group = TRUE)
    gror <- comp$groups[order(rownames(comp$groups)), ]
    tt1$sig <- gror$groups
  }
  list(comparison = tt1, p.value = pp)
}