#' Plot Li-COR data
#'
#' Creates plots for physiological parameters measured using the Li-COR photosystem.
#' @param identifier Keywords that distinguish the Li-COR .xlsx files for the different datasets (e.g. "wt", "mutant1").
#' @param type Determines y axis data. Can be "gsw" (absolute stomatal conductance), "relgsw" (relative stomatal conductance), "A" (CO2 assimilation), "WUE" (intrinsic water use efficiency), "Ci" (Intercellular CO2) or "Ca" (Ambient CO2).
#' @param area_correction Li-COR chamber size divided by average of total measured leaf areas (only one value). Default is set to 1.
#' @param stomden Insert stomatal density to normalise values by stomatal density.
#' @param timestamps Optionally add vertical lines to the plot as timeline indicators (e.g. 'c(20, 40, 60)').
#' @param timeframe Crop the range of time you want to show (e.g. '16:70').
#' @param y_axis_limits Change the y axis limits (e.g. 'c(0,2)' with 0 being the lower and 2 the upper limit).
#' @param errorbars Set type of errorbars to either standard error ("se", this is the default) or standard deviation ("sd").
#' @param legend_title Change the title of the legend. Default is set to "Genotype".
#' @param legend_labels Change the labels within the legend. Default is set to the input given by the 'identifier' argument.
#' @param remove_outliers Optionally remove boxplot outliers by setting to "yes" (based on outliers from the 'A' column).
#' @param colours Set colour palette (e.g. 'c("red", "green")'). Default uses the "Isfahan1" palette of the MetBrewer package.
#' @keywords physiology plot co2 assimilation li-cor stomatal conductance wue water-use efficiency photosynthesis gas exchange ci ca carbon water
#' @export

licorplots <- function(identifier,
                       type="gsw",
                       area_correction=1,
                       stomden=NULL,
                       timestamps=NULL,
                       timeframe=NULL,
                       y_axis_limits=NULL,
                       errorbars="se",
                       legend_title="Genotype",
                       legend_labels=identifier,
                       remove_outliers="no",
                       colours=NULL) {
  ##load required packages
  if (!require(tidyverse)) install.packages('tidyverse')
  if (!require(readxl)) install.packages('readxl')
  if (!require(MetBrewer)) install.packages('MetBrewer')
  library(tidyverse)
  library(readxl)
  library(MetBrewer)

  if(length(area_correction)>1) {
    stop("area_correction argument allows only one value.")
  }

  licorall<- data.frame(TIME=NA, gsw=NA, relgsw=NA, individual=NA, genotype=NA, A=NA, WUE=NA, Ci=NA, Ca=NA, timesec=NA, timepoint=NA)

  if(!is_empty(stomden)) {
    densities <- data.frame(identifier=identifier,
                            density=stomden)
  }


  ###load data files for each genotype
  for (i in identifier) {


    files <- dir(pattern=i)
    for (onefile in files) {
      #new_file<- read_excel(onefile, sheet=1, col_names = F, skip = 16)
      new_file <- suppressMessages(read_excel(onefile, sheet=1, col_names = F, range = cell_cols(1:15)))
      new_file <- new_file[-c(1:15),]
      names(new_file)<- suppressMessages(as_vector(read_excel(onefile, sheet=1, col_names = F, range = "A15:O15")))

      lesslicor<- new_file %>% select(TIME, A, gsw, Ci, Ca)
      lesslicor$A <- as.numeric(lesslicor$A)
      lesslicor$gsw<- as.numeric(lesslicor$gsw)
      lesslicor$TIME<- as.numeric(lesslicor$TIME)
      lesslicor$Ci<- as.numeric(lesslicor$Ci)
      lesslicor$Ca<- as.numeric(lesslicor$Ca)


      #calculate relative stomatal conductance
      lesslicor$relgsw<- lesslicor$gsw/max(lesslicor$gsw)

      #calculate water use efficiency (WUE)
      lesslicor$WUE<- lesslicor$A/lesslicor$gsw

      lesslicor$individual<- onefile
      lesslicor$genotype<- i

      lesslicor$timesec <- (round(lesslicor$TIME))/60
      timezero <- as.numeric(lesslicor[1, "timesec"])
      lesslicor$timepoint <- round(lesslicor$timesec-(timezero-1))

      if(!is_empty(timeframe)) {
        croplicor<- na.omit(lesslicor[timeframe,])
      }

      else {
        croplicor<- na.omit(lesslicor)
      }



      if(remove_outliers=="yes") {
        outliers<- boxplot(croplicor$A, plot=FALSE)$out
        if(!is_empty(outliers)){
          croplicor<- croplicor[-which(croplicor$A %in% outliers),]
        }
      }

      if(!is_empty(stomden)) {
        density <- densities %>% filter(identifier==i) %>% .[1,2]
        ## multiply stomatal density by 1000
        density2 <- density*1000

        ## divide by stomatal density to normalise
        croplicor$gsw <- croplicor$gsw/density2
        croplicor$A <- croplicor$A/density2
        croplicor$WUE <- croplicor$WUE/density2
        croplicor$Ci <- croplicor$Ci/density2
        croplicor$Ca <- croplicor$Ca/density2
      }


      licorall<- na.omit(rbind(licorall, croplicor))
    }

  }

  ##correct absolute gsw, A, Ci and Ca by measured leaf area
  if (area_correction!=1) {
    licorall$gsw <- licorall$gsw*area_correction
    licorall$A <- licorall$A*area_correction
    licorall$Ci <- licorall$Ci*area_correction
    licorall$Ca <- licorall$Ca*area_correction
  }



  ##calculate means, standard deviation and standard error of the gsw values
  licorgeno<- suppressMessages(licorall %>% group_by(timepoint, genotype) %>%
                                 summarise(mean_gsw=mean(gsw), sd_abs=sd(gsw), se_abs=sd(gsw)/sqrt(length(na.omit(gsw))),
                                           mean_relgsw=mean(relgsw), sd_rel=sd(relgsw), se_rel=sd(relgsw)/sqrt(length(na.omit(relgsw))),
                                           mean_A=mean(A), sd_A=sd(A), se_A=sd(A)/sqrt(length(na.omit(A))),
                                           mean_WUE=mean(WUE), sd_WUE=sd(WUE), se_WUE=sd(WUE)/sqrt(length(na.omit(WUE))),
                                           mean_Ci=mean(Ci), sd_Ci=sd(Ci), se_Ci=sd(Ci)/sqrt(length(na.omit(Ci))),
                                           mean_Ca=mean(Ca), sd_Ca=sd(Ca), se_Ca=sd(Ca)/sqrt(length(na.omit(Ca)))))


  ##order data for plots by the order of keywords in 'identifier'
  licorgeno$genotype <- ordered(licorgeno$genotype , levels=identifier)


  ##set palette for plotting
  if(is_empty(colours)) {
    colourpalette <- met.brewer("Isfahan1")
  }
  #else {
  #  if(colours=="grey"){
  #    colourpalette <- c("#000000", "#666666", "#999999", "#CCCCCC", "#EEEEEE")
  #  }
  else{
    colourpalette <- as.vector(colours)
  }
  #}



  ##create df for vertical lines
  timeline<- data.frame(mark=timestamps)



  ##what to plot?
  if(type=="relgsw") {
    plotinfo <- licorgeno$mean_relgsw

    if(errorbars=="se") {
      errors <- licorgeno$se_rel
    }

    if(errorbars=="sd") {
      errors <- licorgeno$sd_rel
    }

    ##plot time against relative stomatal conductance (relgsw) with standard error bars
    ggplot(licorgeno, mapping=aes(x=timepoint, y=plotinfo))+
      geom_errorbar(mapping=aes(ymin=plotinfo - errors, ymax=plotinfo + errors, colour=genotype), alpha=0.5, show.legend = F)+
      geom_point(aes(colour=genotype))+
      geom_vline(timeline, mapping=aes(xintercept=timestamps), linetype="dotted")+
      scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 25, 50, 75, 100))+
      guides(colour=guide_legend(title=legend_title))+
      scale_colour_manual(values=colourpalette,
                          labels=legend_labels)+
      theme_classic()+
      theme(legend.position = "bottom",
            legend.justification="left",
            legend.box.margin = margin(c(-10)),
            legend.background = element_rect(fill=NA))+
      labs(x="Time [min]", y=expression(paste("Relative g"[SW], " [%]")))
  }


  else {
    if(type=="gsw") {
      plotinfo <- licorgeno$mean_gsw

      if(errorbars=="se") {
        errors <- licorgeno$se_abs
      }

      if(errorbars=="sd") {
        errors <- licorgeno$sd_abs
      }

      if(is_empty(stomden)) {
        y_label <- expression(paste("Absolute g"[SW], " [mol m"^-2, " s"^-1, "]"))
      }
      else {
        y_label <- expression(paste("Absolute g"[SW], " [mol Stoma"^-1, " s"^-1, "]"))
      }
    }


    if(type=="A") {
      plotinfo <- licorgeno$mean_A

      if(errorbars=="se") {
        errors <- licorgeno$se_A
      }

      if(errorbars=="sd") {
        errors <- licorgeno$sd_A
      }

      if(is_empty(stomden)) {
        y_label <- expression(paste("A [mol m"^-2, " s"^-1, "]"))
      }
      else {
        y_label <- expression(paste("A [mol Stoma"^-1, " s"^-1, "]"))
      }
    }


    if(type=="WUE") {
      plotinfo <- licorgeno$mean_WUE

      if(errorbars=="se") {
        errors <- licorgeno$se_WUE
      }

      if(errorbars=="sd") {
        errors <- licorgeno$sd_WUE
      }

      y_label <- expression(paste("iWUE [mol(CO"[2], ") mol(H"[2],"O)"^-1, "]"))
    }


    if(type=="Ci") {
      plotinfo <- licorgeno$mean_Ci

      if(errorbars=="se") {
        errors <- licorgeno$se_Ci
      }

      if(errorbars=="sd") {
        errors <- licorgeno$sd_Ci
      }

      y_label <- expression(paste("C"[i], " [µmol mol"^-1, "]"))
    }


    if(type=="Ca") {
      plotinfo <- licorgeno$mean_Ca

      if(errorbars=="se") {
        errors <- licorgeno$se_Ca
      }

      if(errorbars=="sd") {
        errors <- licorgeno$sd_Ca
      }

      y_label <- expression(paste("C"[a], " [µmol mol"^-1, "]"))
    }





    #plot (except relative stomatal conductance)
    ggplot(licorgeno, mapping=aes(x=timepoint, y=plotinfo))+
      geom_errorbar(mapping=aes(ymin=plotinfo-errors, ymax=plotinfo+errors, colour=genotype), alpha=0.5, show.legend = F)+
      geom_point(aes(colour=genotype))+
      geom_vline(timeline, mapping=aes(xintercept=timestamps), linetype="dotted")+
      guides(colour=guide_legend(title=legend_title))+
      scale_colour_manual(values=colourpalette,
                          labels=legend_labels)+
      scale_y_continuous(limits = y_axis_limits)+
      theme_classic()+
      theme(legend.position = "bottom",
            legend.justification="left",
            legend.box.margin = margin(c(-10)),
            legend.background = element_rect(fill=NA))+
      labs(x="Time [min]", y=y_label)
  }
}
