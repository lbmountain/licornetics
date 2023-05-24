#' Calculate steady state and kinetics values from Li-COR data
#'
#' Calculate steady state values and kinetics parameters and plot regression curves from data measured using the Li-COR photosystem.
#' @param identifier Keywords that distinguish the Li-COR .xlsx files for the different datasets (e.g. "wt", "mutant1").
#' @param transition Split your data into different time frames to distinguish between transition phases. Default uses all time points at once.
#' @param area_correction Correct data by leaf area. Li-COR chamber size divided by average of measured leaf areas. Default is set to 1.
#' @param stomden Insert stomatal density to normalise values by stomatal density.
#' @param label Change the names for each 'identifier' input. Default is set to the input given by the 'identifier' argument.
#' @param remove_outliers Optionally remove boxplot outliers by setting to "yes" (based on outliers from the 'A' column).
#' @param colours Set colour to be used for each identifier in final plots (e.g. 'c("red", "green")'). Default is black.
#' @param errorbars Set type of errorbars to either standard error ("se", this is the default) or standard deviation ("sd").
#' @keywords physiology plot co2 assimilation li-cor stomatal conductance leaf kinetics wue water-use efficiency photosynthesis gas exchange carbon water
#' @export


licorvalues <- function(identifier,
                         transition=list("All"),
                         area_correction=1,
                         stomden=NULL,
                         label=NULL,
                         remove_outliers="no",
                         colours=NULL,
                         errorbars = "se") {

  ##load packages
  if (!require(tidyverse)) install.packages('tidyverse')
  if (!require(readxl)) install.packages('readxl')
  if (!require(broom)) install.packages('broom')
  if (!require(MetBrewer)) install.packages('Metbrewer')
  if (!require(ggpubr)) install.packages('ggpubr')
  library(tidyverse)
  library(readxl)
  library(broom)
  library(MetBrewer)
  library(ggpubr)



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



  if(errorbars=="se") {
    finalresults <- data.frame(genotype=NA, transition_zone=NA,
                               gsw=NA, gsw_sd=NA, gsw_se=NA,
                               A=NA, A_sd=NA, A_se=NA,
                               iWUE=NA, iWUE_sd=NA, iWUE_se=NA,
                               rate_constant=NA, T50=NA)
  }

  else {
    finalresults <- data.frame(genotype=NA, transition_zone=NA,
                               gsw=NA, gsw_sd=NA,
                               A=NA, A_sd=NA,
                               iWUE=NA, iWUE_sd=NA,
                               rate_constant=NA, T50=NA)
  }

  licorplots<-list()




  for (i in identifier) {

    plotlabel <- datalabels %>% filter(identifier==i) %>% .[1,2]

    plotcolour <- datalabels %>% filter(identifier==i) %>% .[1,3]

    plotsonegeno <- list()

    licorall <- data.frame(TIME=NA, gsw=NA, relgsw=NA, individual=NA, genotype=NA, A=NA, WUE=NA, timesec=NA, timepoint=NA)

    zone <- data.frame(mean_gsw=NA, sd_abs=NA,
                       mean_relgsw=NA, sd_rel=NA, se_rel=NA,
                       mean_A=NA, sd_A=NA,
                       mean_WUE=NA, sd_WUE=NA)

    files <- dir(pattern=i)
    for (onefile in files) {
      #new_file<- read_excel(onefile, sheet=1, col_names = F, skip = 16)
      new_file <- suppressMessages(read_excel(onefile, sheet=1, col_names = F, range = cell_cols(1:15)))
      new_file <- new_file[-c(1:15),]
      names(new_file)<- suppressMessages(as_vector(read_excel(onefile, sheet=1, col_names = F, range = "A15:O15")))

      lesslicor<- new_file %>% select(TIME, A, gsw)

      lesslicor$gsw<- as.numeric(lesslicor$gsw)
      lesslicor$A <- as.numeric(lesslicor$A)
      lesslicor$TIME<- as.numeric(lesslicor$TIME)

      #calculate relative stomatal conductance
      lesslicor$relgsw<- lesslicor$gsw/max(lesslicor$gsw)

      #calculate water use efficiency (WUE)
      lesslicor$WUE<- lesslicor$A/lesslicor$gsw

      lesslicor$individual<- onefile
      lesslicor$genotype<- i

      lesslicor$timesec <- (round(lesslicor$TIME))/60
      timezero <- as.numeric(lesslicor[1, "timesec"])
      lesslicor$timepoint <- round(lesslicor$timesec-(timezero-1))


      croplicor<- na.omit(lesslicor)


      if(remove_outliers=="yes") {
        outliers<- boxplot(croplicor$A, plot=FALSE)$out
        if(!is_empty(outliers)){
          croplicor<- croplicor[-which(croplicor$A %in% outliers),]
        }
      }
      licorall<- na.omit(rbind(licorall, croplicor))
    }

    ##correct absolute gsw and A by measured leaf area
    if (area_correction!=1) {
      licorall$gsw <- licorall$gsw*area_correction
      licorall$A <- licorall$A*area_correction
    }

    if(!is_empty(stomden)) {
      ## multiply stomatal density by 1000
      stomden2 <- densities %>% filter(identifier==i) %>% .[1,2]
      stomden3 <- stomden2*1000

      ## divide by stomatal density to normalise
      licorall$gsw <- licorall$gsw/stomden3
      licorall$A <- licorall$A/stomden3
      licorall$WUE <- licorall$WUE/stomden3
    }




    ##calculate means and standard deviation of the physiological values
    licorgeno<- suppressMessages(licorall %>% group_by(timepoint, genotype) %>%
                                   summarise(mean_gsw=mean(gsw), sd_abs=sd(gsw),
                                             mean_relgsw=mean(relgsw), sd_rel=sd(relgsw),
                                             se_rel=sd(relgsw)/sqrt(length(na.omit(relgsw))),
                                             mean_A=mean(A), sd_A=sd(A),
                                             mean_WUE=mean(WUE), sd_WUE=sd(WUE)))




    for (tz in transition) {
      ##select a transition zone
      if(tz[1] == "All") {
        zone <- as.data.frame(licorgeno)
      }

      else{
        zone <- as.data.frame(licorgeno[which(licorgeno$timepoint %in% tz),])
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


      ##curve fitting for non-linear exponential decay (SSasymp for decay/association)
      fitlicor <- nls(mean_relgsw ~ SSasymp(timepoint, yf, y0, log_alpha), data=zone)
      ###log_alpha gives the logarithm of the rate constant (here it is alpha, we call it K)

      ##extract fitted curve data (including log_alpha)
      valuesexp <- tidy(fitlicor)
      ##extract log_alpha
      log_alpha <- valuesexp[3,2]
      ##extract rate constant alpha
      alpha <- exp(log_alpha)
      alpha <- alpha$estimate

      ##calculate half-time
      T50 <- log(2)/alpha


      ##summarise
      if(errorbars=="se") {
        results<- data.frame(genotype=plotlabel, transition_zone=as.character(deparse(substitute(tz))),
                             gsw=gsw, gsw_sd=gswvar, gsw_se=gswse,
                             A=A, A_sd=Avar, A_se=Ase,
                             iWUE=iWUE, iWUE_sd=iWUEvar, iWUE_se=iWUEse,
                             rate_constant=alpha, T50=T50)
      }

      else {
        results<- data.frame(genotype=plotlabel, transition_zone=as.character(deparse(substitute(tz))),
                             gsw=gsw, gsw_sd=gswvar,
                             A=A, A_sd=Avar,
                             iWUE=iWUE, iWUE_sd=iWUEvar,
                             rate_constant=alpha, T50=T50)
      }





      ## plot
      zone$fittedvalues <- predict(fitlicor)


      if(errorbars=="se") {
        li_plot <- ggplot(zone, aes(x=timepoint, y=mean_relgsw))+
          geom_point(size=2, colour=plotcolour)+
          geom_line(aes(x=timepoint, y=fittedvalues), linewidth=1, alpha=1)+
          geom_errorbar(mapping=aes(ymin=mean_relgsw - se_rel, ymax=mean_relgsw + se_rel), alpha=0.5, show.legend = F, colour=plotcolour)+
          theme_classic()+
          theme(axis.title.y = element_markdown())+
          scale_y_continuous(limits = c(0,1))+
          labs(x="Time [min]", y="Relative *g*<sub>SW</sub> [%]", title = plotlabel)
      }

      else {
        li_plot <- ggplot(zone, aes(x=timepoint, y=mean_relgsw))+
          geom_point(size=2, colour=plotcolour)+
          geom_line(aes(x=timepoint, y=fittedvalues), linewidth=1, alpha=1)+
          geom_errorbar(mapping=aes(ymin=mean_relgsw - sd_rel, ymax=mean_relgsw + sd_rel), alpha=0.5, show.legend = F, colour=plotcolour)+
          theme_classic()+
          theme(axis.title.y = element_markdown())+
          scale_y_continuous(limits = c(0,1))+
          labs(x="Time [min]", y="Relative *g*<sub>SW</sub> [%]", title = plotlabel)
      }


      new <- list(li_plot)
      plotsonegeno<-c(plotsonegeno, new)

      finalresults <- na.omit(rbind(finalresults, results))
    }
    licorplots <- c(licorplots, plotsonegeno)
  }
  print(ggarrange(plotlist = licorplots))

  return(finalresults)
}
