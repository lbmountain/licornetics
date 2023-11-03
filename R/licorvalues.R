#' Calculate steady state and kinetics values from Li-COR data.
#'
#' Calculate steady state values and kinetics parameters and plot regression curves from data measured using the Li-COR photosystem.
#' @param identifier Keywords that distinguish the Li-COR .xlsx files for the different datasets (e.g. "wt", "mutant1").
#' @param transition Split your data into different time frames to distinguish between transition phases. Default uses all time points at once. Input as lists (e.g. 'list(c(14:25), c(26:37))')
#' @param stomden Normalise data by stomatal density (in unit stomata/mm2). Order of input must follow order of identifiers. (e.g. 'c(50, 100, 70)')
#' @param remove_outliers Optionally remove boxplot outliers by setting to "yes" (based on outliers from the 'A' column).
#' @param show_individuals Change final plot output in R to depict plots for each single input file. Default shows averaged plots for each identifier.
#' @param colours Set colour to be used for each identifier in final plots (e.g. 'c("red", "green")'). Default is black.
#' @keywords physiology plot co2 assimilation li-cor stomatal conductance leaf kinetics wue water-use efficiency photosynthesis gas exchange carbon water
#' @export


licorvalues <- function(identifier,
                        transition=list("All"),
                        stomden=NULL,
                        label=NULL,
                        show_individuals=F,
                        remove_outliers="no",
                        colours=NULL,
                        errorbars = "se") {

  ##load packages
  if (!require(tidyverse)) install.packages('tidyverse')
  if (!require(readxl)) install.packages('readxl')
  if (!require(MetBrewer)) install.packages('Metbrewer')
  if (!require(ggpubr)) install.packages('ggpubr')
  if (!require(ggtext)) install.packages("ggtext")
  library(tidyverse)
  library(readxl)
  library(MetBrewer)
  library(ggpubr)
  library(ggtext)




  #### set up data objects

  if (!is_empty(colours)) {

    colouring <- as.vector(colours)
    colouring <- colouring[1:length(identifier)]

    if (!is_empty(label)) {
      datalabels <- data.frame(identifier=identifier,
                               label=label,
                               colour=colouring)
    }

    else {
      datalabels <- data.frame(identifier=identifier,
                               label=identifier,
                               colour=colouring)
    }
  }

  else {
    if (!is_empty(label)) {
      datalabels <- data.frame(identifier=identifier,
                               label=label,
                               colour="black")
    }

    else {
      datalabels <- data.frame(identifier=identifier,
                               label=identifier,
                               colour="black")
    }
  }

  if(!is_empty(stomden)) {
    densities <- data.frame(identifier=identifier,
                            density=stomden)
  }





  licorall <- NULL
  finalresults <- NULL


  #zone <- data.frame(mean_gsw=NA, sd_abs=NA,
  #                   mean_relgsw=NA, sd_rel=NA, se_rel=NA,
  #                   mean_A=NA, sd_A=NA,
  #                   mean_WUE=NA, sd_WUE=NA)


  print("Loading files...")


  ##### create data objects from the raw data
  for (i in identifier) {
    files <- dir(pattern=i)
    for (onefile in files) {
      print(onefile)

      new_file <- suppressMessages(read_excel(onefile, sheet=1, col_names = F, range = cell_cols(1:15)))
      new_file <- new_file[-c(1:15),]
      names(new_file) <- suppressMessages(as_vector(read_excel(onefile, sheet=1, col_names = F, range = "A15:O15")))

      ### I will need the columns obs, gsw, A, Time
      lesslicor <- NULL
      lesslicor$A <- as.numeric(new_file$A)
      lesslicor$gsw<- as.numeric(new_file$gsw)
      lesslicor$elapsed <- as.numeric(as.integer(new_file$elapsed))
      lesslicor <- as.data.frame(lesslicor)


      #---------------------------------------------------------------------------------------
      ### create new columns
      lesslicor$File  <- onefile
      lesslicor$ID <- i
      lesslicor$Time <- as.integer(lesslicor$elapsed/60)

      #calculate relative stomatal conductance
      lesslicor$relgsw<- lesslicor$gsw/max(lesslicor$gsw)

      #calculate water use efficiency (WUE)
      lesslicor$WUE<- lesslicor$A/lesslicor$gsw



      #----------------------------------------------------------------------------------------
      #### remove outliers
      if(remove_outliers=="yes") {
        outliers<- boxplot(lesslicor$A, plot=FALSE)$out
        if(!is_empty(outliers)){
          lesslicor<- lesslicor[-which(lesslicor$A %in% outliers),]
        }
      }

      ## calculate means and standard deviation of the physiological values ---------------------------------------------
      if(!is_empty(stomden)) {
        ## multiply stomatal density by 1000
        stomden2 <- densities %>% filter(identifier==i) %>% .[1,2]
        stomden3 <- stomden2*1000

        ## divide by stomatal density to normalise
        lesslicor$gsw <- lesslicor$gsw/stomden3
        lesslicor$A <- lesslicor$A/stomden3
        lesslicor$WUE <- lesslicor$WUE/stomden3
      }

      licorall <- rbind(licorall, lesslicor)
    }
  }



  licorall$elapsed <- plyr::round_any(licorall$elapsed, 10)















  licorgeno <- suppressMessages(na.omit(licorall %>% group_by(Time, ID, elapsed) %>%
                                          summarise(mean_gsw=mean(gsw), sd_gsw=sd(gsw), se_gsw=sd(gsw)/sqrt(length(na.omit(gsw))),
                                                    mean_A=mean(A), #sd_A=sd(A), se_A=sd(A)/sqrt(length(na.omit(A))),
                                                    mean_WUE=mean(WUE))))











  #### calculate steady state values ------------------------------------------------------------------------
  print("Calculating steady state values for...")

  steady_values_all <- NULL

  for (i in identifier) {
    print(i)

    licorgeno_short <- subset(licorgeno, licorgeno$ID %in% i)

    for (tz in transition) {
      ##select a transition zone
      if(tz[1] == "All") {
        zone <- as.data.frame(licorgeno_short)
      }

      else{
        zone <- subset(licorgeno_short, licorgeno_short$Time %in% tz)
      }

      ###calculate steady state gsw, A and WUE
      ##select last 5 timepoints
      lastpoints <- tail(zone, 5)

      gsw <- mean(lastpoints$mean_gsw)
      gswvar <- sd(lastpoints$mean_gsw)
      gswse <- sd(lastpoints$mean_gsw)/sqrt(length(na.omit(lastpoints$mean_gsw)))
      A <- mean(lastpoints$mean_A)
      Avar <- sd(lastpoints$mean_A)
      Ase <- sd(lastpoints$mean_A)/sqrt(length(na.omit(lastpoints$mean_A)))
      iWUE <- mean(lastpoints$mean_WUE)
      iWUEvar <- sd(lastpoints$mean_WUE)
      iWUEse <- sd(lastpoints$mean_WUE)/sqrt(length(na.omit(lastpoints$mean_WUE)))


      ## summarise
      steady_values <- data.frame(ID=i, transition_zone=as.character(deparse(substitute(tz))),
                                  gsw=gsw, gsw_sd=gswvar, gsw_se=gswse,
                                  A=A, A_sd=Avar, A_se=Ase,
                                  iWUE=iWUE, iWUE_sd=iWUEvar, iWUE_se=iWUEse)

      steady_values_all <- rbind(steady_values_all, steady_values)
    }
  }











  #### create required functions -----------------------------------------------------------------------------

  opt.fun <- function(p, l)
  {
    if (any(p<0) | p[3] < 0 | p[4] < 1) return(Inf)
    gmod <- p[1] + (p[2] - p[1]) * exp(-exp((p[3] - l$t) / p[4] + 1)) # McAusland et al. (2016)
    ss <- (gmod - l$gobs)^2
    w <- rep(1, length(ss))
    w[1] <- 10
    return(mean(ss * w))
  }

  sig_model <- function(time, gsw)
  {
    xy <- sortedXyData(time, gsw)
    g0 <- gsw[1]
    g1 <- mean(tail(gsw, 3))
    xy <- subset(xy, time >= 0) # Omit any negative time points - escape for a bug
    lambda <- 10
    k <- 30
    p <- c(g0, g1, lambda, k)

    l <- NULL
    l$time <- time
    l$gobs <- gsw
    opt.out <- optim(p, opt.fun, l=l, control = list(maxit=2000))
    return(opt.out)
  }

  sig_model_predict <- function(time, p)
  {
    gmod <- p[1] + (p[2] - p[1]) * exp(-exp((p[3] - time) / p[4] + 1)) # McAusland et al. (2016)
    return(gmod)
  }





  ### fit values to Vialet-Chabrand model and calculate stomatal kinetics for single individuals -----------------
  print("Calculating stomatal kinetics values and visualize...")

  ### a folder called GraphModel will be created in the current folder
  if (!dir.exists("GraphModel")) {
    dir.create("GraphModel")
  }

  ### new data object
  kinetics_values <- NULL

  plots_final_individuals <- list()
  plots_final_identifiers <- list()

  for (tz in transition) {
    licorall_short <- as.data.frame(licorall[which(licorall$Time %in% tz),])

    ## set time to start from 0
    licorall_short$time_sec_new <- licorall_short$elapsed - licorall_short$elapsed[1]
    licorall_short$time_sec_new <- plyr::round_any(licorall_short$time_sec_new, 10)


    ### a folder called GraphModel will be created in the current folder
    if (!dir.exists(paste0("GraphModel/", "minutes_", tz[1], "_", last(tz)))) {
      dir.create(paste0("GraphModel/", "minutes_", tz[1], "_", last(tz)))
    }


    plots_individuals <- list()

    ### calculate and plot per individual file
    for (f in unique(licorall_short$File)) {
      dd <- subset(licorall_short, f == File)

      ## all values below 0 will be set to 0
      dd$gsw[dd$gsw < 0] <- 0

      ## fit model
      outG <- sig_model(dd$time_sec_new, dd$gsw)
      #outG <- sig_model(dd$Time, dd$gsw)

      fn <- paste0("GraphModel/minutes_", tz[1], "_", last(tz), "/", f, ".png")

      plotcolour <- datalabels %>% filter(identifier==dd$ID[1]) %>% .[1,3]

      dd$fitted_values <- sig_model_predict(dd$time_sec_new, outG$par)

      if(!is_empty(stomden)) {
        indi_plot <- ggplot(dd, mapping=aes(x = time_sec_new, y = gsw))+
          geom_point(size=2, colour=plotcolour)+
          geom_line(mapping = aes(x = time_sec_new, y = fitted_values), linewidth=1, alpha=1)+
          theme_classic()+
          theme(axis.title.y = element_markdown())+
          labs(x="Time [s]", y= "Absolute *g*<sub>SW</sub> [mol stoma<sup>-1</sup> s<sup>-1</sup>]")
      }

      else {
        indi_plot <- ggplot(dd, mapping=aes(x = time_sec_new, y = gsw))+
          geom_point(size=2, colour=plotcolour)+
          geom_line(mapping = aes(x = time_sec_new, y = fitted_values), linewidth=1, alpha=1)+
          theme_classic()+
          theme(axis.title.y = element_markdown())+
          labs(x="Time [s]", y= "Absolute *g*<sub>SW</sub> [mol m<sup>-2</sup> s<sup>-1</sup>]")
      }


      png(fn)
      print(indi_plot)
      dev.off()

      new_indi <- list(indi_plot)
      plots_individuals <- c(plots_individuals, new_indi)


      ## calculate Slmax
      SlG <- 1 / outG$par[4] * (outG$par[2] - outG$par[1]) / exp(1) * 1000 # in mmol m-2 s-2

      ## final dataframe
      kinetics_values <- rbind(kinetics_values, data.frame(File=f, ID=dd$ID[1],
                                                           transition_zone = as.character(deparse(substitute(tz))),
                                                           g0=outG$par[1], g1=outG$par[2],
                                                           lambda_indi=outG$par[3],
                                                           k_indi=outG$par[4],
                                                           Slmax_indi=SlG))
    }

    plots_final_individuals <- c(plots_final_individuals, plots_individuals)









    ### plot per identifier
    plots_identifiers <- list()

    for (i in identifier) {
      dd <- licorgeno %>% filter(ID == i)

      dd <- as.data.frame(dd[which(dd$Time %in% tz),])


      ## set time to start from 0
      dd$time_sec_new <- dd$elapsed - dd$elapsed[1]
      dd$time_sec_new <- plyr::round_any(dd$time_sec_new, 10)


      ## all values below 0 will be set to 0
      dd$mean_gsw[dd$mean_gsw < 0] <- 0


      ## fit model
      outG <- sig_model(dd$time_sec_new, dd$mean_gsw)



      ## plot
      fn <- paste0("GraphModel/minutes_", tz[1], "_", last(tz), "/", i, ".png")

      plotcolour <- datalabels %>% filter(identifier== i[1]) %>% .[1,3]
      plotlabel <- datalabels %>% filter(identifier==i[1]) %>% .[1,2]

      dd$fitted_values <- sig_model_predict(dd$time_sec_new, outG$par)

      if(is_empty(stomden)) {
        ident_plot <- ggplot(dd, mapping=aes(x = time_sec_new, y = mean_gsw))+
          geom_point(size=2, colour=plotcolour)+
          geom_line(mapping = aes(x = time_sec_new, y = fitted_values), linewidth=1, alpha=1)+
          geom_errorbar(mapping=aes(ymin=mean_gsw - se_gsw, ymax=mean_gsw + se_gsw), alpha=0.5, show.legend = F, colour=plotcolour)+
          theme_classic()+
          theme(axis.title.y = element_markdown(),
                plot.title = element_markdown())+
          labs(x="Time [s]", y= "Absolute *g*<sub>SW</sub> [mol m<sup>-2</sup> s<sup>-1</sup>]", title = plotlabel)
      }

      else {
        ident_plot <- ggplot(dd, mapping=aes(x = time_sec_new, y = mean_gsw))+
          geom_point(size=2, colour=plotcolour)+
          geom_line(mapping = aes(x = time_sec_new, y = fitted_values), linewidth=1, alpha=1)+
          geom_errorbar(mapping = aes(ymin=mean_gsw - se_gsw, ymax=mean_gsw + se_gsw), alpha=0.5, show.legend = F, colour=plotcolour)+
          theme_classic()+
          theme(axis.title.y = element_markdown(),
                plot.title = element_markdown())+
          labs(x="Time [s]", y= "Absolute *g*<sub>SW</sub> [mol stoma<sup>-1</sup> s<sup>-1</sup>]", title = plotlabel)
      }


      png(fn)
      print(ident_plot)
      dev.off()

      new <- list(ident_plot)
      plots_identifiers <- c(plots_identifiers, new)
    }

    plots_final_identifiers <- c(plots_final_identifiers, plots_identifiers)
  }

  mean_kinetics <- suppressMessages(kinetics_values %>% group_by(ID, transition_zone) %>% summarise(lambda = mean(lambda_indi), se_lambda = sd(lambda_indi)/sqrt(length(na.omit(lambda_indi))),
                                                                                                    lambda_min = lambda/60, se_lambda_min = se_lambda/60,
                                                                                                    k = mean(k_indi), se_k = sd(k_indi)/sqrt(length(na.omit(k_indi))),
                                                                                                    k_min = k/60, se_k_min = se_k/60,
                                                                                                    Slmax = mean(Slmax_indi), se_Slmax = sd(Slmax_indi)/sqrt(length(na.omit(Slmax_indi)))))

  finalresults <- merge(steady_values_all, mean_kinetics, by = c("ID", "transition_zone"))

  if(isTRUE(show_individuals)) {
    print(ggarrange(plotlist = plots_final_individuals))
  }

  else {
    print(ggarrange(plotlist = plots_final_identifiers))
  }

  print("Finished!")

  return(finalresults)
}
