#' Plot Li-COR data
#'
#' Creates plots for physiological parameters measured using the Li-COR photosystem.
#' @param identifier Keywords that distinguish the Li-COR .xlsx files for the different datasets (e.g. "wt", "mutant1").
#' @param type Determines y axis data. Can be "gsw" (absolute stomatal conductance), "relgsw" (relative stomatal conductance), "A" (CO2 assimilation), "WUE" (intrinsic water use efficiency), "Ci" (Intercellular CO2) or "Ca" (Ambient CO2). From version 2.0.1 onward also allows plotting of other columns (e.g. "Fs").
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
#' @param axis_label Change y axis label text. Allows markdown language elements.
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
                       colours=NULL,
                       axis_label=type) {
  ##load required packages
  if (!require(tidyverse)) install.packages('tidyverse')
  if (!require(readxl)) install.packages('readxl')
  if (!require(MetBrewer)) install.packages('MetBrewer')
  if (!require(ggtext)) install.packages('ggtext')
  library(tidyverse)
  library(readxl)
  library(MetBrewer)
  library(ggtext)

  if(length(area_correction)>1) {
    stop("area_correction argument allows only one value.")
  }

  #if(!type %in% c("gsw", "relgsw", "A", "WUE", "Ci", "Ca")) {
  #  stop("plot type input is something other than 'gsw', 'relgsw', 'A', 'WUE', 'Ci' or 'Ca'.")
  #}

  licorall<- data.frame(elapsed=NA, gsw=NA, relgsw=NA, individual=NA, genotype=NA, A=NA, WUE=NA, Ci=NA, Ca=NA, timepoint=NA)
  if(!type %in% c("gsw", "relgsw", "A", "WUE", "Ci", "Ca")) {
    licorall<- data.frame(elapsed=NA, gsw=NA, relgsw=NA, individual=NA, genotype=NA, A=NA, WUE=NA, Ci=NA, Ca=NA, timepoint=NA, extra_col=NA)
  }

  if(!is_empty(stomden)) {
    densities <- data.frame(identifier=identifier,
                            density=stomden)
  }


  ###load data files for each genotype
  for (i in identifier) {
    print(i)

    files <- dir(pattern=i)
    for (onefile in files) {
      print(onefile)
      new_file <- suppressMessages(read_excel(onefile, sheet=1, col_names = F, range = cell_cols(1:15)))
      new_file <- new_file[-c(1:15),]
      names(new_file) <- suppressMessages(as_vector(read_excel(onefile, sheet=1, col_names = F, range = "A15:O15")))

      if(!type %in% c("gsw", "relgsw", "A", "WUE", "Ci", "Ca")) {
        extra_file <- suppressMessages(read_excel(onefile, sheet = 1, col_names = T, trim_ws = F, skip = 14)) %>% .[, type]
        extra_file <- extra_file[-1,]
        final_name <- as_vector(colnames(extra_file))
        colnames(extra_file) <- "extra_col"
      }

      lesslicor<- new_file %>% select(elapsed, A, gsw, Ci, Ca)
      lesslicor$A <- as.numeric(lesslicor$A)
      lesslicor$gsw<- as.numeric(lesslicor$gsw)
      lesslicor$elapsed<- as.integer(lesslicor$elapsed)
      lesslicor$Ci<- as.numeric(lesslicor$Ci)
      lesslicor$Ca<- as.numeric(lesslicor$Ca)

      if(!type %in% c("gsw", "relgsw", "A", "WUE", "Ci", "Ca")) {
        lesslicor <- cbind(lesslicor, extra_file)
        lesslicor$extra_col <- as.numeric(lesslicor$extra_col)
      }



      #calculate relative stomatal conductance
      lesslicor$relgsw<- lesslicor$gsw/max(lesslicor$gsw)

      #calculate water use efficiency (WUE)
      lesslicor$WUE<- lesslicor$A/lesslicor$gsw

      lesslicor$individual<- onefile
      lesslicor$genotype<- i

      lesslicor$timepoint <- as.integer(lesslicor$elapsed/60)

      if(!is_empty(timeframe)) {
        croplicor <- lesslicor[which(lesslicor$timepoint %in% timeframe),]
      }

      else {
        croplicor<- lesslicor
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

        if(!type %in% c("gsw", "relgsw", "A", "WUE", "Ci", "Ca")) {
          croplicor$extra_col <- croplicor$extra_col/density2
        }
      }

      licorall<- rbind(croplicor, licorall)
    }
  }

  ##correct absolute gsw, A, Ci and Ca by measured leaf area
  if (area_correction!=1) {
    licorall$gsw <- licorall$gsw*area_correction
    licorall$A <- licorall$A*area_correction
    licorall$Ci <- licorall$Ci*area_correction
    licorall$Ca <- licorall$Ca*area_correction
    if (!type %in% c("gsw", "relgsw", "A", "WUE", "Ci", "Ca")) {
      licorall$extra_col <- licorall$extra_col*area_correction
    }
  }



  ##calculate means, standard deviation and standard error of the gsw values
  licorgeno<- suppressMessages(licorall %>% group_by(timepoint, genotype) %>%
                                 summarise(mean_gsw=mean(gsw), sd_abs=sd(gsw), se_abs=sd(gsw)/sqrt(length(na.omit(gsw))),
                                           mean_relgsw=mean(relgsw), sd_rel=sd(relgsw), se_rel=sd(relgsw)/sqrt(length(na.omit(relgsw))),
                                           mean_A=mean(A), sd_A=sd(A), se_A=sd(A)/sqrt(length(na.omit(A))),
                                           mean_WUE=mean(WUE), sd_WUE=sd(WUE), se_WUE=sd(WUE)/sqrt(length(na.omit(WUE))),
                                           mean_Ci=mean(Ci), sd_Ci=sd(Ci), se_Ci=sd(Ci)/sqrt(length(na.omit(Ci))),
                                           mean_Ca=mean(Ca), sd_Ca=sd(Ca), se_Ca=sd(Ca)/sqrt(length(na.omit(Ca)))))

  if (!type %in% c("gsw", "relgsw", "A", "WUE", "Ci", "Ca")) {
    extra_geno <- suppressMessages(licorall %>% group_by(timepoint, genotype) %>%
                                     summarise(mean_extra=mean(extra_col), sd_extra=sd(extra_col),
                                               se_extra=sd(extra_col)/sqrt(length(na.omit(extra_col)))))
    extra_geno$genotype <- ordered(extra_geno$genotype, levels = identifier)
  }


  ##order data for plots by the order of keywords in 'identifier'
  licorgeno$genotype <- ordered(licorgeno$genotype, levels=identifier)


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

    if(!axis_label == type) {
      y_label <- axis_label
    }

    y_label <- "Relative *g*<sub>SW</sub> [%]"

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
            legend.background = element_rect(fill=NA),
            axis.title.y = element_markdown(),
            legend.text = element_markdown())+
      labs(x="Time [min]", y= y_label)
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
        y_label <- "Absolute *g*<sub>SW</sub> [mol m<sup>-2</sup> s<sup>-1</sup>]"
      }
      else {
        y_label <- "Absolute *g*<sub>SW</sub> [mol stoma<sup>-1</sup> s<sup>-1</sup>]"
      }

      if(!axis_label == type) {
        y_label <- axis_label
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
        y_label <- "*A* [µmol m<sup>-2</sup> s<sup>-1</sup>]"
      }
      else {
        y_label <- "*A* [µmol stoma<sup>-1</sup> s<sup>-1</sup>]"
      }

      if(!axis_label == type) {
        y_label <- axis_label
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

      y_label <- "iWUE [µmol(C) mol(H<sub>2</sub>O)<sup>-1</sup>]"

      if(!axis_label == type) {
        y_label <- axis_label
      }
    }


    if(type=="Ci") {
      plotinfo <- licorgeno$mean_Ci

      if(errorbars=="se") {
        errors <- licorgeno$se_Ci
      }

      if(errorbars=="sd") {
        errors <- licorgeno$sd_Ci
      }

      y_label <- "*C*<sub>i</sub> [µmol mol<sup>-1</sup>]"

      if(!axis_label == type) {
        y_label <- axis_label
      }
    }


    if(type=="Ca") {
      plotinfo <- licorgeno$mean_Ca

      if(errorbars=="se") {
        errors <- licorgeno$se_Ca
      }

      if(errorbars=="sd") {
        errors <- licorgeno$sd_Ca
      }

      y_label <- "*C*<sub>a</sub> [µmol mol<sup>-1</sup>]"

      if(!axis_label == type) {
        y_label <- axis_label
      }
    }


    if(!type %in% c("gsw", "relgsw", "A", "WUE", "Ci", "Ca")) {
      plotinfo <- extra_geno$mean_extra

      if(errorbars=="se") {
        errors <- extra_geno$se_extra
      }

      if(errorbars=="sd") {
        errors <- extra_geno$sd_extra
      }

      y_label <- axis_label

      licorgeno <- extra_geno
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
            legend.background = element_rect(fill=NA),
            axis.title.y = element_markdown(),
            legend.text = element_markdown(),
            legend.title = element_markdown())+
      labs(x="Time [min]", y=y_label)
  }
}
