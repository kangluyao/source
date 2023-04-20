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

# quadratic regression plot
plot_quadr_df <- function(
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
        formula = y ~ poly(x, 2),
        se = TRUE,
        alpha = 0.75
      )
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
    method = "gam",
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
        formula = y ~ splines::ns(x, 2),
        se = TRUE,
        alpha = 0.75
      )
  }
  p <- patchwork::wrap_plots(plot.list, ncol = ncol)
  p
}