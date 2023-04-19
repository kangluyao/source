# linner model for matrix to matrix
lm.mat.cal <- function(y, x, data, method){
  y <- as.matrix(y)
  x <- as.matrix(x)
  df<-NULL
  for(i in colnames(y)){
    for(j in colnames(x)){
      a <- y[, i, drop = F]
      b <- x[, j, drop = F]
      mode <- lm(a ~ b, data, na.action=na.omit)
      coeff <- summary(mode)$coefficients[2,1]
      r.square <- round(summary(mode)$r.squared, 2)
      AIC <- round(AIC(mode), 2)
      p.value <- round(anova(mode)$"Pr(>F)"[1], 3)
      normal <- round(shapiro.test(residuals(mode))$p.value, 3)
      if (coeff>0) r = sqrt(r.square)
      else r = (-1) * sqrt(r.square)
      tmp <- c(i, j, r, r.square, AIC, p.value, normal)
      if(is.null(df)){
        df <- tmp  
      }
      else{
        df <- rbind(df, tmp)
      }    
    }
  }
  df<-data.frame(row.names=NULL,df)
  colnames(df)<-c("dependent.variables","predictor.variables","Correlation","r.square", "AIC", "Pvalue", "normality")
  df$Pvalue<-as.numeric(as.character(df$Pvalue))
  df$AdjPvalue<-rep(0,dim(df)[1])
  df$Correlation<-as.numeric(as.character(df$Correlation))
  #You can adjust the p-values for multiple comparison using Benjamini & Hochberg (1995):
  # 1 -> donot adjust
  # 2 -> adjust predictor.variables + Type (column on the correlation plot)
  # 3 -> adjust dependent.variables + Type (row on the correlation plot for each type)
  # 4 -> adjust dependent.variables (row on the correlation plot)
  # 5 -> adjust predictor.variables (panel on the correlation plot)
  adjustment_label<-c("NoAdj","Adjpredictor.variablesAndType","Adjdependent.variablesAndType","Adjdependent.variables","Adjpredictor.variables")
  adjustment<-5
  if(adjustment==1){
    df$AdjPvalue<-df$Pvalue
  } else if (adjustment==2){
    for(i in unique(df$predictor.variables)){
      for(j in unique(df$Type)){
        sel<-df$predictor.variables==i & df$Type==j
        df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
      }
    }
  } else if (adjustment==3){
    for(i in unique(df$dependent.variables)){
      for(j in unique(df$Type)){
        sel<-df$dependent.variables==i & df$Type==j
        df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
      }
    }
  } else if (adjustment==4){
    for(i in unique(df$dependent.variables)){
      sel<-df$dependent.variables==i
      df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
    }
  } else if (adjustment==5){
    for(i in unique(df$predictor.variables)){
      sel<-df$predictor.variables==i
      df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
    }
  }
  #Now we generate the labels for signifant values
  df$Significance<-cut(df$AdjPvalue, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", ""))
  df$dependent.variables <-factor(df$dependent.variables, ordered = T, levels = rev(colnames(y)))
  df$predictor.variables <-factor(df$predictor.variables, ordered = T, levels = colnames(x))
  return(df)
}

lm.quadr.mat.cal <- function(y, x, data, method){
  y <- as.matrix(y)
  x <- as.matrix(x)
  df<-NULL
  for(i in colnames(y)){
    for(j in colnames(x)){
      a <- y[, i, drop = F]
      b <- x[, j, drop = F]
      mode <- lm(a ~ b + I(b^2), data, na.action=na.omit)
      coeff <- summary(mode)$coefficients[2,1]
      r.square <- round(summary(mode)$r.squared, 2)
      AIC <- round(AIC(mode), 2)
      p.value <- round(anova(mode)$"Pr(>F)"[1], 3)
      normal <- round(shapiro.test(residuals(mode))$p.value, 3)
      if (coeff>0) r = sqrt(r.square)
      else r = (-1) * sqrt(r.square)
      tmp <- c(i, j, r, r.square, AIC, p.value, normal)
      if(is.null(df)){
        df <- tmp  
      }
      else{
        df <- rbind(df, tmp)
      }    
    }
  }
  df<-data.frame(row.names=NULL,df)
  colnames(df)<-c("dependent.variables","predictor.variables","Correlation","r.square", "AIC", "Pvalue", "normality")
  df$Pvalue<-as.numeric(as.character(df$Pvalue))
  df$AdjPvalue<-rep(0,dim(df)[1])
  df$Correlation<-as.numeric(as.character(df$Correlation))
  #You can adjust the p-values for multiple comparison using Benjamini & Hochberg (1995):
  # 1 -> donot adjust
  # 2 -> adjust predictor.variables + Type (column on the correlation plot)
  # 3 -> adjust dependent.variables + Type (row on the correlation plot for each type)
  # 4 -> adjust dependent.variables (row on the correlation plot)
  # 5 -> adjust predictor.variables (panel on the correlation plot)
  adjustment_label<-c("NoAdj","Adjpredictor.variablesAndType","Adjdependent.variablesAndType","Adjdependent.variables","Adjpredictor.variables")
  adjustment<-5
  if(adjustment==1){
    df$AdjPvalue<-df$Pvalue
  } else if (adjustment==2){
    for(i in unique(df$predictor.variables)){
      for(j in unique(df$Type)){
        sel<-df$predictor.variables==i & df$Type==j
        df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
      }
    }
  } else if (adjustment==3){
    for(i in unique(df$dependent.variables)){
      for(j in unique(df$Type)){
        sel<-df$dependent.variables==i & df$Type==j
        df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
      }
    }
  } else if (adjustment==4){
    for(i in unique(df$dependent.variables)){
      sel<-df$dependent.variables==i
      df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
    }
  } else if (adjustment==5){
    for(i in unique(df$predictor.variables)){
      sel<-df$predictor.variables==i
      df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
    }
  }
  #Now we generate the labels for signifant values
  df$Significance<-cut(df$AdjPvalue, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", ""))
  df$dependent.variables <-factor(df$dependent.variables, ordered = T, levels = rev(colnames(y)))
  df$predictor.variables <-factor(df$predictor.variables, ordered = T, levels = colnames(x))
  return(df)
}

#linner mixed model for matrix to matrix
lmm.mat.cal <- function(y, x, data, method){
  y <- as.matrix(y)
  x <- as.matrix(x)
  df<-NULL
  for(i in colnames(y)){
    for(j in colnames(x)){
      a <- y[, i, drop = F]
      b <- x[, j, drop = F]
      mode <- lmerTest::lmer(a ~ b + (1|Site), data, na.action=na.omit)
      coeff <- summary(mode)$coefficients[2,1]
      r.square <- round(MuMIn::r.squaredGLMM(mode)[1], 2)
      AIC <- round(AIC(mode), 2)
      p.value <- round(anova(mode)$Pr[1], 3)
      if (coeff>0) r = sqrt(r.square)
      else r = (-1) * sqrt(r.square)
      tmp <- c(i, j, r, r.square, AIC, p.value)
      if(is.null(df)){
        df <- tmp  
      }
      else{
        df <- rbind(df, tmp)
      }    
    }
  }
  df<-data.frame(row.names=NULL,df)
  colnames(df)<-c("dependent.variables","predictor.variables","Correlation","r.square", "AIC", "Pvalue")
  df$Pvalue<-as.numeric(as.character(df$Pvalue))
  df$AdjPvalue<-rep(0,dim(df)[1])
  df$Correlation<-as.numeric(as.character(df$Correlation))
  #You can adjust the p-values for multiple comparison using Benjamini & Hochberg (1995):
  # 1 -> donot adjust
  # 2 -> adjust predictor.variables + Type (column on the correlation plot)
  # 3 -> adjust dependent.variables + Type (row on the correlation plot for each type)
  # 4 -> adjust dependent.variables (row on the correlation plot)
  # 5 -> adjust predictor.variables (panel on the correlation plot)
  adjustment_label<-c("NoAdj","Adjpredictor.variablesAndType","Adjdependent.variablesAndType","Adjdependent.variables","Adjpredictor.variables")
  adjustment<-5
  if(adjustment==1){
    df$AdjPvalue<-df$Pvalue
  } else if (adjustment==2){
    for(i in unique(df$predictor.variables)){
      for(j in unique(df$Type)){
        sel<-df$predictor.variables==i & df$Type==j
        df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
      }
    }
  } else if (adjustment==3){
    for(i in unique(df$dependent.variables)){
      for(j in unique(df$Type)){
        sel<-df$dependent.variables==i & df$Type==j
        df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
      }
    }
  } else if (adjustment==4){
    for(i in unique(df$dependent.variables)){
      sel<-df$dependent.variables==i
      df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
    }
  } else if (adjustment==5){
    for(i in unique(df$predictor.variables)){
      sel<-df$predictor.variables==i
      df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
    }
  }
  #Now we generate the labels for signifant values
  df$Significance<-cut(df$AdjPvalue, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", ""))
  df$dependent.variables <-factor(df$dependent.variables, ordered = T, levels = rev(colnames(y)))
  df$predictor.variables <-factor(df$predictor.variables, ordered = T, levels = colnames(x))
  return(df)
}

lmm.quadr.mat.cal <- function(y, x, data, method){
  y <- as.matrix(y)
  x <- as.matrix(x)
  df<-NULL
  for(i in colnames(y)){
    for(j in colnames(x)){
      a <- y[, i, drop = F]
      b <- x[, j, drop = F]
      mode <- lmerTest::lmer(a ~ b + I(b^2) + (1|Site), data, na.action=na.omit)
      coeff <- summary(mode)$coefficients[2,1]
      r.square <- round(MuMIn::r.squaredGLMM(mode)[1], 2)
      AIC <- round(AIC(mode), 2)
      p.value <- round(anova(mode)$Pr[2], 3)
      if (coeff>0) r = sqrt(r.square)
      else r = (-1) * sqrt(r.square)
      tmp <- c(i, j, r, r.square, AIC, p.value)
      if(is.null(df)){
        df <- tmp  
      }
      else{
        df <- rbind(df, tmp)
      }    
    }
  }
  df<-data.frame(row.names=NULL,df)
  colnames(df)<-c("dependent.variables", "predictor.variables", "Correlation", "r.square", "AIC", "Pvalue")
  df$Pvalue<-as.numeric(as.character(df$Pvalue))
  df$AdjPvalue<-rep(0,dim(df)[1])
  df$Correlation<-as.numeric(as.character(df$Correlation))
  #You can adjust the p-values for multiple comparison using Benjamini & Hochberg (1995):
  # 1 -> donot adjust
  # 2 -> adjust predictor.variables + Type (column on the correlation plot)
  # 3 -> adjust dependent.variables + Type (row on the correlation plot for each type)
  # 4 -> adjust dependent.variables (row on the correlation plot)
  # 5 -> adjust predictor.variables (panel on the correlation plot)
  adjustment_label<-c("NoAdj","Adjpredictor.variablesAndType","Adjdependent.variablesAndType","Adjdependent.variables","Adjpredictor.variables")
  adjustment<-5
  if(adjustment==1){
    df$AdjPvalue<-df$Pvalue
  } else if (adjustment==2){
    for(i in unique(df$predictor.variables)){
      for(j in unique(df$Type)){
        sel<-df$predictor.variables==i & df$Type==j
        df$AdjPvalue[sel]<-c(df$Pvalue[sel],method="BH")
      }
    }
  } else if (adjustment==3){
    for(i in unique(df$dependent.variables)){
      for(j in unique(df$Type)){
        sel<-df$dependent.variables==i & df$Type==j
        df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
      }
    }
  } else if (adjustment==4){
    for(i in unique(df$dependent.variables)){
      sel<-df$dependent.variables==i
      df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
    }
  } else if (adjustment==5){
    for(i in unique(df$predictor.variables)){
      sel<-df$predictor.variables==i
      df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
    }
  }
  #Now we generate the labels for signifant values
  df$Significance<-cut(df$AdjPvalue, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", ""))
  df$dependent.variables <-factor(df$dependent.variables, ordered = T, levels = rev(colnames(y)))
  df$predictor.variables <-factor(df$predictor.variables, ordered = T, levels = colnames(x))
  return(df)
}