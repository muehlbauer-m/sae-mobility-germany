################################################################################
##### Plotting functions
################################################################################

pacman::p_load(tidyverse, emdi, mase, viridis, ggpubr, reshape2)
source(paste0(getwd(),"/R/auxiliaries/00_themes.R"))
################################################################################

QQ_plots <- function(model, heading, color, pointshape, pointsize,
                     restitle, rantitle, alpha, Theme){
  
  residuals <- model$model$std_real_residuals
  residuals <- (residuals - mean(residuals))/sd(residuals)
  rand.eff <- model$model$random_effects
  srand.eff <- (rand.eff - mean(rand.eff))/sd(rand.eff)
  tmp <- srand.eff
  
  pointsize <- pointsize
  abline_color <- "black"
  pointcols <- color
  label <- emdi:::define_label(x = model, label = "orig")
  
  res <- ggplot(data.frame(residuals), aes(sample = residuals)) +
    stat_qq(distribution = qnorm,
            dparams = list(mean = mean(residuals), sd = sd(residuals)),
            color = pointcols[1], shape = pointshape, size = pointsize,
            fill = alpha(pointcols[1], alpha)) +
    geom_abline(colour = abline_color) + 
    restitle + 
    ylab(
      latex2exp::TeX(
        "Quantiles of std. residuals")) + 
    #ylab("") +
    xlab("Theoretical quantiles") + 
    #xlab("") +
    Theme
  
  #random effects
  ran <- ggplot(data.frame(tmp), aes(sample = tmp)) +
    stat_qq(distribution = qnorm,
            dparams = list(mean = mean(tmp), sd = sd(tmp)),
            color = pointcols[2], shape = pointshape, size = pointsize,
            fill = alpha(pointcols[2], alpha)) + 
    geom_abline(intercept = 0, slope = 1, na.rm = TRUE, col = abline_color) +
    rantitle + 
    ylab(label$qq_ran["y_lab"]) +
    #ylab("") +
    xlab(label$qq_ran["x_lab"]) +
    #xlab("") +
    Theme
  
  comb <- ggpubr::ggarrange(res, ran, ncol = 2)
  # comb <-  ggpubr::annotate_figure(p = comb,
  #                                fig.lab.pos = "bottom",
  #                                fig.lab = heading,
  #                                fig.lab.size = 12,
  #                                fig.lab.face = "bold")
  
list(res, ran, comb = comb)
  
}

################################################################################

Density_plots <- function(model, heading, color, alpha, Theme){
  
  label <- emdi:::define_label(x = model, label = "orig")
  
  residuals <- model$model$std_real_residuals
  residuals <- (residuals - mean(residuals))/sd(residuals)
  rand.eff <- model$model$random_effects
  srand.eff <- (rand.eff - mean(rand.eff))/sd(rand.eff)
  tmp <- srand.eff

  res_dens <- ggplot(data.frame(Residuals = residuals),
                   aes(x = Residuals)) +
  geom_density(fill = color[1], color = color[1], alpha = alpha) +
  stat_function(fun = dnorm, linewidth = 0.7) +
  ylab("Density") +
  xlab("Std. residuals") + ggtitle("Density - Std. Residuals") +
  Theme

  ran_dens <- ggplot(data.frame(Random = srand.eff),
                   aes(x = Random)) +
  geom_density(fill = color[2], color = color[2], alpha = alpha) +
  stat_function(fun = dnorm, linewidth = 0.7) + ylab(label$d_ran["y_lab"]) +
  xlab(label$d_ran["x_lab"]) + ggtitle("Density - Std. Random Effects") +
  Theme

  comb <-  ggpubr::ggarrange(res_dens, ran_dens, ncol = 2)
  
list(res_dens, ran_dens, comb = ggpubr::annotate_figure(p = comb,
                                                 fig.lab.pos = "bottom",
                                                 fig.lab = heading,
                                                 fig.lab.size = 12,
                                                 fig.lab.face = "bold"))

}

################################################################################

################################################################################

Compare_Density_plots <- function(model1, model2, model3 = NULL,
                                  Modnames,
                                  color1,
                                  color2, 
                                  heading,
                                  alpha,
                                  legend.key.size,
                                  Theme){
  
  if (is.null(model3)) {
    
    label <- emdi:::define_label(x = model1, label = "orig")
    
    residuals1 <- model1$model$std_real_residuals
    residuals1 <- (residuals1 - mean(residuals1))/sd(residuals1)
    
    residuals2 <- model2$model$std_real_residuals
    residuals2 <- (residuals2 - mean(residuals2))/sd(residuals2)
    
    df_resids <- data.frame(r1 = residuals1, r2 =residuals2)
    suppressMessages(resids <- reshape2::melt(df_resids) %>% 
                       mutate(col_column = variable))
    
    rand.eff1 <- model1$model$random_effects
    srand.eff1 <- (rand.eff1 - mean(rand.eff1))/sd(rand.eff1)
    
    rand.eff2 <- model2$model$random_effects
    srand.eff2 <- (rand.eff2 - mean(rand.eff2))/sd(rand.eff2)
    
    df_srandeff <- data.frame(randef1 = srand.eff1, randef2 =srand.eff2)
    suppressMessages(srandeffs <- reshape2::melt(df_srandeff) %>% 
                       mutate(col_column = variable))
    
    suppressWarnings(comb_res_dens <- ggplot(data = resids, 
                                             aes(x = value, fill = variable,
                                                 color = col_column)) + 
                       geom_density(alpha = alpha) + 
                       scale_fill_manual(values = color1, labels = Modnames) +
                       scale_color_manual(values = color1) +
                       stat_function(fun = dnorm, color = "black", linewidth = 0.7) +
                       ylab("Density") +
                       xlab("Std. residuals") +
                       ggtitle("(a) Density - Std. Residuals") +
                       Theme +
                       theme(legend.title=element_blank(),
                             legend.position = "bottom",
                             legend.key.size = legend.key.size) +
                       guides(color = "none",
                              fill = guide_legend(
                                override.aes=list(color = color1))))
    
    suppressWarnings(comb_rand_dens <- ggplot(data = srandeffs, 
                                              aes(x = value, fill = variable,
                                                  color = col_column)) + 
                       geom_density(alpha = alpha) + 
                       scale_fill_manual(values = color2, labels = Modnames) +
                       scale_color_manual(values = color2) +
                       stat_function(fun = dnorm, color = "black", linewidth = 0.7) +
                       ylab("Density") +
                       xlab("Std. random effects") +
                       ggtitle("(b) Density - Std. Random Effects") +
                       Theme +
                       theme(legend.title=element_blank(),
                             legend.position = "bottom",
                             legend.key.size = legend.key.size) +
                       guides(color = "none",
                              fill = guide_legend(
                                override.aes=list(color = color2))))
    
  } else if(!is.null(model3)) {
    
    
    label <- emdi:::define_label(x = model1, label = "orig")
    
    residuals1 <- model1$model$std_real_residuals
    residuals1 <- (residuals1 - mean(residuals1))/sd(residuals1)
    
    residuals2 <- model2$model$std_real_residuals
    residuals2 <- (residuals2 - mean(residuals2))/sd(residuals2)
    
    residuals3 <- model3$model$std_real_residuals
    residuals3 <- (residuals3 - mean(residuals3))/sd(residuals3)
    
    df_resids <- data.frame(r1 = residuals1, r2 =residuals2, r3 = residuals3)
    suppressMessages(resids <- reshape2::melt(df_resids) %>% 
                       mutate(col_column = variable))
    
    rand.eff1 <- model1$model$random_effects
    srand.eff1 <- (rand.eff1 - mean(rand.eff1))/sd(rand.eff1)
    
    rand.eff2 <- model2$model$random_effects
    srand.eff2 <- (rand.eff2 - mean(rand.eff2))/sd(rand.eff2)
    
    rand.eff3 <- model3$model$random_effects
    srand.eff3 <- (rand.eff3 - mean(rand.eff3))/sd(rand.eff3)
    
    df_srandeff <- data.frame(randef1 = srand.eff1,
                              randef2 = srand.eff2,
                              randef3 = srand.eff3)
    suppressMessages(srandeffs <- reshape2::melt(df_srandeff) %>% 
                       mutate(col_column = variable))
    
    suppressWarnings(comb_res_dens <- ggplot(data = resids, 
                                             aes(x = value, fill = variable, color = col_column)) + 
                       geom_density(alpha = alpha) + 
                       color_palette(color1) +
                       fill_palette(color1,
                                    labels = Modnames) +
                       stat_function(fun = dnorm, color = "black", linewidth = 0.7) +
                       ylab(label$d_res["y_lab"]) + 
                       xlab(label$d_res["x_lab"]) + 
                       #ggtitle(label$d_res["title"]) +
                      Theme +
                       theme(legend.title=element_blank(),
                             legend.position = "bottom",
                             legend.key.size = legend.key.size) +
                       guides(col = "none"))
    
    suppressWarnings(comb_rand_dens <- ggplot(data = srandeffs, 
                                              aes(x = value, fill = variable, color = col_column)) + 
                       geom_density(alpha = alpha) + 
                       color_palette(color2) +
                       fill_palette(color2,
                                    labels = Modnames) +
                       stat_function(fun = dnorm, color = "black", linewidth = 0.7) +
                       ylab(label$d_ran["y_lab"]) + 
                       xlab(label$d_ran["x_lab"]) + 
                       #ggtitle(label$d_res["title"]) +
                       Theme +
                       theme(legend.title=element_blank(),
                             legend.position = "bottom",
                             legend.key.size = legend.key.size) +
                       guides(col = "none"))
  }
  
  comb <-  suppressWarnings(
    ggpubr::ggarrange(comb_res_dens,
                      comb_rand_dens,
                      ncol = 2,
                      heights = c(1, 1)))
  comb <-  ggpubr::annotate_figure(p = comb,
                                   fig.lab.pos = "bottom",
                                   fig.lab = heading,
                                   fig.lab.size = 12,
                                   fig.lab.face = "bold")
  
  list(comb_res_dens, comb_rand_dens, comb = comb)
  
}

################################################################################

Compare_Density_2nd_spatial_plots <- function(model1, model2, model3 = NULL,
                                  Modnames,
                                  color, 
                                  heading,
                                  alpha){
  
  if (is.null(model3)) {
    
    label <- emdi:::define_label(x = model1, label = "orig")
    
    residuals1 <- model1$model$std_real_residuals
    residuals1 <- (residuals1 - mean(residuals1))/sd(residuals1)
    
    residuals2 <- model2$model$std_real_residuals
    residuals2 <- (residuals2 - mean(residuals2))/sd(residuals2)
    
    df_resids <- data.frame(r1 = residuals1, r2 =residuals2)
    suppressMessages(resids <- reshape2::melt(df_resids) %>% 
                       mutate(col_column = variable))
    
    rand.eff1 <- model1$model$random_effects
    srand.eff1 <- (rand.eff1 - mean(rand.eff1))/sd(rand.eff1)
    
    #Special Spatial Model part:
    u_hat <- model2$model$random_effects
    rho <- model2$model$variance$correlation
    W <- model2$framework$W
    alp <- u_hat - (rho *matrix(data=W, nrow = nrow(u_hat)) %*% 
                      as.vector(u_hat))
    rand.eff2 <- c(alp)
    srand.eff2 <- (rand.eff2 - mean(rand.eff2))/sd(rand.eff2)
    #
    
    df_srandeff <- data.frame(randef1 = srand.eff1, randef2 =srand.eff2)
    suppressMessages(srandeffs <- reshape2::melt(df_srandeff) %>% 
                       mutate(col_column = variable))
    
    
    suppressWarnings(comb_res_dens <- ggplot(data = resids, 
                                             aes(x = value, fill = variable, color = col_column)) + 
                       geom_density(alpha = alpha) + 
                       color_palette(color) +
                       fill_palette(color,
                                    labels = Modnames) +
                       stat_function(fun = dnorm, color = "black", linewidth = 0.7) +
                       ylab(label$d_res["y_lab"]) + 
                       xlab(label$d_res["x_lab"]) + 
                       #ggtitle(label$d_res["title"]) +
                       General_theme() +
                       theme(legend.title=element_blank(),
                             legend.position = "top") +
                       guides(col = "none"))
    
    suppressWarnings(comb_rand_dens <- ggplot(data = srandeffs, 
                                              aes(x = value, fill = variable, color = col_column)) + 
                       geom_density(alpha = alpha) + 
                       color_palette(color) +
                       fill_palette(color,
                                    labels = Modnames) +
                       stat_function(fun = dnorm, color = "black", linewidth = 0.7) +
                       ylab(label$d_ran["y_lab"]) + 
                       xlab(label$d_ran["x_lab"]) + 
                       #ggtitle(label$d_res["title"]) +
                       General_theme() +
                       theme(legend.title=element_blank(),
                             legend.position = "top") +
                       guides(col = "none"))
    
  } else if(!is.null(model3)) {
    
    
    label <- emdi:::define_label(x = model1, label = "orig")
    
    residuals1 <- model1$model$std_real_residuals
    residuals1 <- (residuals1 - mean(residuals1))/sd(residuals1)
    
    residuals2 <- model2$model$std_real_residuals
    residuals2 <- (residuals2 - mean(residuals2))/sd(residuals2)
    
    residuals3 <- model3$model$std_real_residuals
    residuals3 <- (residuals3 - mean(residuals3))/sd(residuals3)
    
    df_resids <- data.frame(r1 = residuals1, r2 =residuals2, r3 = residuals3)
    suppressMessages(resids <- reshape2::melt(df_resids) %>% 
                       mutate(col_column = variable))
    
    rand.eff1 <- model1$model$random_effects
    srand.eff1 <- (rand.eff1 - mean(rand.eff1))/sd(rand.eff1)
    
    
    #Special Spatial Model part:
    u_hat <- model2$model$random_effects
    rho <- model2$model$variance$correlation
    W <- model2$framework$W
    alp <- u_hat - (rho *matrix(data=W, nrow = nrow(u_hat)) %*% 
                      as.vector(u_hat))
    rand.eff2 <- c(alp)
    srand.eff2 <- (rand.eff2 - mean(rand.eff2))/sd(rand.eff2)
    #
    
    rand.eff3 <- model3$model$random_effects
    srand.eff3 <- (rand.eff3 - mean(rand.eff3))/sd(rand.eff3)
    
    df_srandeff <- data.frame(randef1 = srand.eff1,
                              randef2 = srand.eff2,
                              randef3 = srand.eff3)
    suppressMessages(srandeffs <- reshape2::melt(df_srandeff) %>% 
                       mutate(col_column = variable))
    
    suppressWarnings(comb_res_dens <- ggplot(data = resids, 
                                             aes(x = value, fill = variable, color = col_column)) + 
                       geom_density(alpha = alpha) + 
                       color_palette(color) +
                       fill_palette(color,
                                    labels = Modnames) +
                       stat_function(fun = dnorm, color = "black", linewidth = 0.7) +
                       ylab(label$d_res["y_lab"]) + 
                       xlab(label$d_res["x_lab"]) + 
                       #ggtitle(label$d_res["title"]) +
                       General_theme() +
                       theme(legend.title=element_blank(),
                             legend.position = "top") +
                       guides(col = "none"))
    
    suppressWarnings(comb_rand_dens <- ggplot(data = srandeffs, 
                                              aes(x = value, fill = variable, color = col_column)) + 
                       geom_density(alpha = alpha) + 
                       color_palette(color) +
                       fill_palette(color,
                                    labels = Modnames) +
                       stat_function(fun = dnorm, color = "black", linewidth = 0.7) +
                       ylab(label$d_ran["y_lab"]) + 
                       xlab(label$d_ran["x_lab"]) + 
                       #ggtitle(label$d_res["title"]) +
                       General_theme() +
                       theme(legend.title=element_blank(),
                             legend.position = "top") +
                       guides(col = "none"))
  }
  
  comb <-  suppressWarnings(
    ggpubr::ggarrange(comb_res_dens,
                      comb_rand_dens,
                      ncol = 2,
                      heights = c(1, 1)))
  comb <-  ggpubr::annotate_figure(p = comb,
                                   fig.lab.pos = "bottom",
                                   fig.lab = heading,
                                   fig.lab.size = 12,
                                   fig.lab.face = "bold")
  
  list(comb_res_dens, comb_rand_dens, comb = comb)
  
}  
    
################################################################################

Compare_FH_Direct_Plots <- function(MSE = FALSE,
                                    model = NULL,
                                    direct = NULL,
                                    indicator = "all",
                                    CV = FALSE, label = "orig",
                                    color = c("blue", "lightblue3","black"),
                                    shape = c(16, 16), 
                                    line_type = c("solid", "solid"),
                                    alpha,
                                    gg_theme = NULL,
                                    modname) {

Model_based <- NULL
Direct <- NULL
ID <- NULL
value <- NULL
Method <- NULL
Data <- emdi:::point_emdi(object = model, indicator = "all")$ind
Data <- Data[!is.na(Data$Direct), ]
selected_indicators <- colnames(Data)[!(colnames(Data) %in% 
                                          c("Domain", "Direct"))]
colnames(Data) <- 
  c("Domain",
    "FH_Direct",
    paste0(colnames(Data)[!(colnames(Data) %in% c("Domain",
                                                  "Direct"))],"_Model"))
if ("FH_Bench" %in% selected_indicators) {
  Data$FH_Bench_Direct <- Data$FH_Direct
}
if ("FH_Bench" %in% indicator & !("FH_Bench" %in% selected_indicators)) {
  message("emdi object does not contain benchmarked fh estimates. 
          Only \n FH estimates are compared with direct. 
          See also help(benchmark).")
}
if (!(any(indicator == "all") || any(indicator == "direct") || 
      any(indicator == "Direct"))) {
  selected_indicators <- selected_indicators[selected_indicators %in% 
                                               indicator]
}
if (is.null(model$MSE)) {
  Data$smp_size <- NULL
}
if (MSE == TRUE || CV == TRUE) {
  all_precisions <- emdi:::mse_emdi(object = model, indicator = "all", 
                             CV = TRUE)
  colnames(all_precisions$ind) <- c("Domain", paste0(c("FH_Direct", 
                                                       "FH_Model"), "_MSE"))
  colnames(all_precisions$ind_cv) <- c("Domain", paste0(c("FH_Direct", 
                                                          "FH_Model"), "_CV"))
  combined <- merge(all_precisions$ind, all_precisions$ind_cv, 
                    id = "Domain")
  combined <- combined[!is.na(combined$FH_Direct_MSE), 
  ]
  Data <- merge(Data, combined, id = "Domain")
  Data$smp_size <- -Data$FH_Direct_MSE
  Data$smp_size2 <- -Data$FH_Direct_CV
}

###

  object <-  Data
  type <-  "area"
  slope <- NULL
  intercept <- NULL
  area <- NULL
  if (MSE == FALSE & CV == FALSE) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 
                         2)
    names(plotList) <- paste(rep(c("scatter", "line"),
                                 length(selected_indicators)), 
                             rep(selected_indicators, each = 2), sep = "_")
  }
  else if ((MSE == TRUE | CV == TRUE) & !(MSE == TRUE & CV == 
                                          TRUE)) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 
                         4)
    names(plotList) <- paste(rep(c("scatter", "line"),
                                 length(selected_indicators)), 
                             rep(selected_indicators, each = 4), sep = "_")
  }
  else if (MSE == TRUE & CV == TRUE) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 
                         6)
    names(plotList) <- paste(rep(c("scatter", "line"),
                                 length(selected_indicators)), 
                             rep(selected_indicators, each = 6), sep = "_")
  }
  for (ind in selected_indicators) {
    label_ind <- emdi:::define_evallabel(type = type, label = label, 
                                  indi = modname) #divergence from emdi
    if (is.null(object$smp_size)) {
      data_tmp <- data.frame(Direct = object[, paste0(ind, 
                                                      "_Direct")],
                             Model_based = object[, paste0(ind, "_Model")])
      label_ind$line["x_lab"] <- "Domains (unordered)"
    }
    else {
      data_tmp <- data.frame(Direct = object[, paste0(ind,"_Direct")],
                             Model_based = object[, paste0(ind, "_Model")],
                             smp_size = object$smp_size)
      data_tmp <- data_tmp[order(data_tmp$smp_size), ]
      data_tmp$smp_size <- NULL
    }
    data_tmp$ID <- seq_along(object$Domain)
    data_shaped <- melt(data_tmp, id.vars = "ID")
    names(data_shaped) <- c("ID", "Method", "value")
    
    
plotList[[paste("scatter", ind, sep = "_")]] <- 
             ggplot(data_tmp,  aes(x = Direct, y = Model_based)) + 
             geom_point(shape = shape[1],
                        fill = alpha("black", alpha = alpha)) + 
             geom_smooth(method = lm, formula = y ~ x, se = FALSE, 
                         inherit.aes = FALSE, lty = line_type[1],
                         aes(colour = "Reg. line", 
                             x = Direct, y = Model_based)) + 
             geom_abline(mapping = aes(colour = "Intersection",
                                       slope = slope,
                                       intercept = intercept),
                         data.frame(slope = 1, intercept = 0),
                         lty = line_type[2]) + 
             xlim(min(min(data_tmp$Direct),
                      min(data_tmp$Model_based)),
                  max(max(data_tmp$Direct), max(data_tmp$Model_based))) +
             ylim(min(min(data_tmp$Direct),
                      min(data_tmp$Model_based)),
                  max(max(data_tmp$Direct), max(data_tmp$Model_based))) + 
             ggtitle(label_ind$scatter["title"]) + 
             ylab(label_ind$scatter["y_lab"]) + 
             xlab(label_ind$scatter["x_lab"]) + 
             scale_color_manual(name = "",
                                values = c(Intersection = color[3],
                                           `Reg. line` = color[2])) +
             gg_theme
    
    
    
plotList[[paste("line", ind, sep = "_")]] <- 
         ggplot(data = data_shaped, 
                aes(x = ID, y = value, group = Method, colour = Method)) + 
         geom_line(aes(linetype = Method), size = 0.7) + 
         geom_point(aes(color = Method, shape = Method, fill = Method), size = 2) + 
         scale_shape_manual(values = c(shape[1],
                                       shape[2]), 
                            breaks = c("Direct", "Model_based"),
                            labels = c("Direct", "Model-based")) +
         scale_linetype_manual(values = c(line_type[1], line_type[2]),
                               breaks = c("Direct", "Model_based"), 
                               labels = c("Direct", "Model-based")) +
         scale_color_manual(values = c(color[1],  color[2]),
                            breaks = c("Direct", "Model_based"),
                            labels = c("Direct", "Model-based")) + 
         scale_fill_manual(name = "Method",
                           breaks = c("Direct", "Model_based"),
                           labels = c("Direct", "Model-based"),
                           values = alpha(color, alpha = alpha)) + 
         xlab(label_ind$line["x_lab"]) + 
         ylab(label_ind$line["y_lab"]) + 
         ggtitle(label_ind$line["title"]) + 
         gg_theme

    if (MSE == TRUE) {
      data_tmp2 <- data.frame(Direct = object[, paste0(ind, "_Direct_MSE")], 
                              Model_based = object[, paste0(ind, "_Model_MSE")], 
                              smp_size = object$smp_size)
      data_tmp2 <- data_tmp2[order(data_tmp2$smp_size, 
                                   decreasing = TRUE), ]
      data_tmp2$smp_size <- NULL
      data_tmp2$ID <- seq_along(object$Domain)
      data_shaped <- melt(data_tmp2, id.vars = "ID")
      names(data_shaped) <- c("ID", "Method", "value")
      data_shaped$area <- rep(seq_len(NROW(data_tmp2$Direct)), 
                              2)
      
      
plotList[[paste("boxplot", "MSE", ind, sep = "_")]] <-
         ggplot(data_shaped,  aes(x = Method, y = value, fill = Method)) + 
         geom_boxplot(outlier.alpha = alpha,
                      outlier.shape = pointshape) + 
         coord_flip() +
         labs(title = label_ind$boxplot_MSE["title"],
              x = label_ind$boxplot_MSE["x_lab"],
              y = label_ind$boxplot_MSE["y_lab"]) + 
         scale_fill_manual(values = color) + 
         scale_color_manual(values = color) + 
         gg_theme
      
      
      
      
plotList[[paste("ordered", "MSE", ind, sep = "_")]] <- 
         ggplot(data_shaped, 
                aes(x = area, y = value, colour = Method)) + 
         geom_point(aes(shape = Method, col = Method, fill = Method)) + 
         labs(title = label_ind$ordered_MSE["title"], 
              x = label_ind$ordered_MSE["x_lab"], 
              y = label_ind$ordered_MSE["y_lab"]) + 
         scale_color_manual(values = color) + 
         scale_shape_manual(values = c(shape[1],  shape[2])) + 
         scale_fill_manual(values = alpha(color, alpha = alpha)) +
         gg_theme
    }

    if (CV == TRUE) {
      data_tmp3 <- data.frame(Direct = object[, paste0(ind, "_Direct_CV")], 
                              Model_based = object[, paste0(ind, "_Model_CV")],
                              smp_size = object$smp_size2)
      data_tmp3 <- data_tmp3[order(data_tmp3$smp_size, 
                                   decreasing = TRUE), ]
      data_tmp3$smp_size <- NULL
      data_tmp3$ID <- seq_along(object$Domain)
      data_shaped <- melt(data_tmp3, id.vars = "ID")
      names(data_shaped) <- c("ID", "Method", "value")
      data_shaped$area <- rep(seq_len(NROW(data_tmp3$Direct)), 
                              2)
      

      
      
      
plotList[[paste("boxplot", "CV", ind, sep = "_")]] <- 
         ggplot(data_shaped,
                aes(x = Method, y = value, fill = Method)) + 
         geom_boxplot(outlier.alpha = alpha,
                      outlier.shape = pointshape) + 
         coord_flip() + 
         labs(title = label_ind$boxplot_CV["title"],
              x = label_ind$boxplot_CV["x_lab"],
              y = label_ind$boxplot_CV["y_lab"]) + 
         scale_fill_manual(name = "Method", values = color) + 
         scale_color_manual(name = "Method", values = color) + 
         gg_theme

      
      
      data_shaped
      
      
      
plotList[[paste("ordered", "CV", ind, sep = "_")]] <- 
        ggplot(data_shaped, aes(x = area, y = value, colour = Method)) + 
        geom_point(aes(color = Method, shape = Method, fill = Method)) + 
        labs(title = label_ind$ordered_CV["title"], 
             x = label_ind$ordered_CV["x_lab"],
             y = label_ind$ordered_CV["y_lab"]) + 
        scale_color_manual(values = color) +
        scale_shape_manual(values = c(shape[1], shape[2])) +
        scale_fill_manual(values = alpha(color, alpha = alpha)) +
        gg_theme

    }
 
  }
  plotList
  
}



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  