# correlation plot
plot_cor_df <- function(
    data = NULL,
    y.name = NULL,
    x.names = NULL,
    ncol = NULL,
    method = "pearson",
    point.color = "tomato3",
    line.color = "black"
){
  if(
    is.null(data) |
    is.null(y.name) |
    is.null(x.names)
  ){
    stop("No variables to plot.")
  }
  if (method == "pearson"){
    labels = c("cor", "p.value")
  } else if (method == "spearman"){
    labels = c("rho", "p.value")
  } else if (method == "kendall"){
    labels = c("tau", "p.value")
  }
  #x.names comes from environmental variables
  if(!is.null(x.names)){
    if(inherits(x.names, "variable_selection")){
      x.names <- x.names$selected.variables
    }
  }
  plot.list <- list()
  for(variable in x.names){
    plot.list[[variable]] <- ggplot2::ggplot(
      data = data,
      ggplot2::aes_string(
        x = variable,
        y = y.name,
        color = y.name
      )
    ) +
      ggplot2::geom_point(colour = point.color) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggpmisc::stat_poly_line(formula = y ~ x, color = line.color) +
      ggpmisc::stat_correlation(method = method, 
                                mapping = ggpmisc::use_label(labels))
  }
  p <- patchwork::wrap_plots(plot.list, ncol = ncol)
  p
}

# linear and loess regression plot
plot_loess_df <- function(
    data = NULL,
    y.name = NULL,
    x.names = NULL,
    ncol = NULL,
    method = "loess",
    point.color = "tomato3",
    line.color = "black"
){
  
  if(
    is.null(data) |
    is.null(y.name) |
    is.null(x.names)
  ){
    stop("No variables to plot.")
  }
  #x.names comes from environmental variables
  if(!is.null(x.names)){
    if(inherits(x.names, "variable_selection")){
      x.names <- x.names$selected.variables
    }
  }
  plot.list <- list()
  for(variable in x.names){
    plot.list[[variable]] <- ggplot2::ggplot(
      data = data,
      ggplot2::aes_string(
        x = variable,
        y = y.name,
        color = y.name
      )
    ) +
      ggplot2::geom_point(colour = point.color) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_smooth(
        method = method,
        col = line.color,
        formula = y ~ x,
        se = TRUE,
        alpha = 0.75
      )
  }
  p <- patchwork::wrap_plots(plot.list, ncol = ncol)
  p
}

# poly regression plot
plot_poly_df <- function(
    data = NULL,
    y.name = NULL,
    x.names = NULL,
    poly = 2,
    ncol = NULL,
    point.color = "tomato3",
    line.color = "black"
){
  
  if(
    is.null(data) |
    is.null(y.name) |
    is.null(x.names)
  ){
    stop("No variables to plot.")
  }
  #x.names comes from environmental variables
  if(!is.null(x.names)){
    if(inherits(x.names, "variable_selection")){
      x.names <- x.names$selected.variables
    }
  }
  plot.list <- list()
  for(variable in x.names){
    plot.list[[variable]] <- ggplot2::ggplot(
      data = data,
      ggplot2::aes_string(
        x = variable,
        y = y.name,
        color = y.name
      )
    ) +
      ggplot2::geom_point(colour = point.color) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggpmisc::stat_poly_line(formula = y ~ poly(x, poly), color = line.color) +
      ggpmisc::stat_poly_eq(aes(label =  paste(after_stat(eq.label), "*\", \"*",
                                  after_stat(rr.label), "*\", \"*", 
                                  after_stat(p.value.label), "*\".\"",
                                  sep = "")),
    formula = y ~ poly(x, poly), size = 3)
  }
  p <- patchwork::wrap_plots(plot.list, ncol = ncol)
  p
}

# generalized additive model plot
plot_gam_df <- function(
    data = NULL,
    y.name = NULL,
    x.names = NULL,
    ncol = NULL,
    k = 2,
    point.color = "tomato3",
    line.color = "black"
){
  
  if(
    is.null(data) |
    is.null(y.name) |
    is.null(x.names)
  ){
    stop("No variables to plot.")
  }
  #x.names comes from environmental variables
  if(!is.null(x.names)){
    if(inherits(x.names, "variable_selection")){
      x.names <- x.names$selected.variables
    }
  }
  plot.list <- list()
  for(variable in x.names){
    plot.list[[variable]] <- ggplot2::ggplot(
      data = data,
      ggplot2::aes_string(
        x = variable,
        y = y.name,
        color = y.name
      )
    ) +
      ggplot2::geom_point(colour = point.color) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_smooth(
        method = "gam",
        col = line.color,
        formula = y ~ splines::ns(x, k),
        se = TRUE,
        alpha = 0.75
      )
      # ggpmisc::stat_poly_line(formula = y ~ splines::ns(x, k), color = line.color) +
      # ggpmisc::stat_poly_eq(aes(label =  paste(after_stat(eq.label), "*\", \"*",
      #                                          after_stat(rr.label), "*\", \"*", 
      #                                          after_stat(p.value.label), "*\".\"",
      #                                          sep = "")),
      #                       formula = y ~ splines::ns(x, k), size = 3)
  }
  p <- patchwork::wrap_plots(plot.list, ncol = ncol)
  p
}
