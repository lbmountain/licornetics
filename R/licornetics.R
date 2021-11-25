#' Li-COR plotting package
#'
#' Creates plots for physiological parameters measured using the Li-COR photosystem.
#' @param identifier Keywords that distinguish the Li-COR .xlsx files for the different datasets (e.g. "wt", "mutant1").
#' @param type Determines y axis data. Can be "gsw" (absolute stomatal conductance), "relgsw" (relative stomatal conductance), "A" (CO2 assimilation) or "WUE" (water use efficiency).
#' @param area_correction Li-COR chamber size divided by average of measured leaf areas. Default is set to 1.
#' @param timestamps Optionally add vertical lines to the plot as timeline indicators (e.g. 'c(20, 40, 60)').
#' @param observations Crop the range of observations (time) you want to show (e.g. 'c(16:70)').
#' @param legend_title Change the title of the legend. Default is set to "Genotype".
#' @param legend_labels Change the labels within the legend. Default is set to the input given by the 'identifier' argument.
#' @param remove_outliers Optionally remove boxplot outliers (based on data obtained by calculating A/gsw).
#' @keywords physiology plot co2 assimilation li-cor stomatal conductance leaf kinetics wue water-use efficiency photosynthesis
#' @export

licornetics<- function(identifier,
                       type="gsw",
                       area_correction=1,
                       timestamps=NULL,
                       observations=NULL,
                       legend_title="Genotype",
                       legend_labels=identifier,
                       remove_outliers="no") {
  ##load required packages
  if (!require(tidyverse)) install.packages('tidyverse')
  if (!require(readxl)) install.packages('readxl')
  library(tidyverse)
  library(readxl)

  licorall<- data.frame(obs=NA, gsw=NA, relgsw=NA, individual=NA, genotype=NA, A=NA, WUE=NA)


  ###load data files for each genotype
  for (i in identifier) {
    files <- dir(pattern=i)
    for (onefile in files) {
      new_file<- suppressMessages(read_excel(onefile, sheet=1, col_names = F, skip = 16))
      names(new_file)<- suppressMessages(as_vector(read_excel(onefile, sheet=1, col_names = F, range = "A15:EQ15")))

      lesslicor<- new_file %>% select(obs, A, gsw)
      lesslicor$gsw<- as.numeric(lesslicor$gsw)
      lesslicor$obs<- as.numeric(lesslicor$obs)

      if(!is_empty(observations)) {
        croplicor<- na.omit(lesslicor[observations,])
      }
      else {
        croplicor<- na.omit(lesslicor)
      }

      #calculate relative stomatal conductance
      croplicor$relgsw<- croplicor$gsw/max(croplicor$gsw)

      #calculate water use efficiency (WUE)
      croplicor$WUE<- croplicor$A/croplicor$gsw

      croplicor$individual<- onefile
      croplicor$genotype<- i

      if(remove_outliers=="yes") {
        outliers<- boxplot(croplicor$WUE, plot=FALSE)$out
        if(!is_empty(outliers)){
          croplicor<- croplicor[-which(croplicor$WUE %in% outliers),]
        }
      }


      licorall<- na.omit(rbind(licorall, croplicor))
    }
  }



  ##correct absolute gsw values by measured area
  if (area_correction!=1) {
    licorall$gsw <- licorall$gsw*area_correction
    licorall$A <- licorall$A*area_correction
  }



  ##calculate means and standard deviation of the gsw values
  licorgeno<- suppressMessages(licorall %>% group_by(obs, genotype) %>%
                                 summarise(mean_gsw=mean(gsw), se_abs=sd(gsw)/sqrt(length(na.omit(gsw))),
                                           mean_relgsw=mean(relgsw), se_rel=sd(relgsw)/sqrt(length(na.omit(relgsw))),
                                           mean_A=mean(A), se_A=sd(A)/sqrt(length(na.omit(A))),
                                           mean_WUE=mean(WUE), se_WUE=sd(WUE)/sqrt(length(na.omit(WUE)))))


  ##order data for plots by the order of keywords in 'identifier'
  licorgeno$genotype <- ordered(licorgeno$genotype , levels=identifier)


  ##set colourblind palette for plotting
  colourblindpalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


  if (type=="gsw") {
    ##plot observations against absolute stomatal conductance (gsw)
    if (is_empty(timestamps)) {
      ggplot(licorgeno, mapping=aes(x=obs, y=mean_gsw))+
        geom_errorbar(mapping=aes(ymin=mean_gsw-se_abs, ymax=mean_gsw+se_abs, colour=genotype), alpha=0.5, show.legend = F)+
        geom_point(aes(colour=genotype))+
        #        geom_label(mapping=aes(x=30, y=max(mean_gsw)+0.02, label="cloudy"))+
        #        geom_label(mapping=aes(x=50, y=max(mean_gsw)+0.02, label="sunny"))+
        #        geom_label(mapping=aes(x=70, y=max(mean_gsw)+0.02, label="dark"))+
        guides(colour=guide_legend(title=legend_title))+
        scale_colour_manual(values=colourblindpalette,
                            labels=legend_labels)+
        theme_classic()+
        theme(legend.position = "bottom",
              legend.justification="left",
              legend.box.margin = margin(c(-10)),
              legend.background = element_rect(fill=NA))+
        labs(x="Time [min]", y=expression(paste("Absolute g"[SW], "[mol*m"^-2, "*s"^-1, "]")))
    }
    else {
      #create df for vertical lines
      timeline<- data.frame(mark=timestamps)

      #plot
      ggplot(licorgeno, mapping=aes(x=obs, y=mean_gsw))+
        geom_errorbar(mapping=aes(ymin=mean_gsw-se_abs, ymax=mean_gsw+se_abs, colour=genotype), alpha=0.5, show.legend = F)+
        geom_point(aes(colour=genotype))+
        geom_vline(timeline, mapping=aes(xintercept=timestamps), linetype="dotted")+
        #        geom_label(mapping=aes(x=30, y=max(mean_gsw)+0.02, label="cloudy"))+
        #        geom_label(mapping=aes(x=50, y=max(mean_gsw)+0.02, label="sunny"))+
        #        geom_label(mapping=aes(x=70, y=max(mean_gsw)+0.02, label="dark"))+
        guides(colour=guide_legend(title=legend_title))+
        scale_colour_manual(values=colourblindpalette,
                            labels=legend_labels)+
        theme_classic()+
        theme(legend.position = "bottom",
              legend.justification="left",
              legend.box.margin = margin(c(-10)),
              legend.background = element_rect(fill=NA))+
        labs(x="Time [min]", y=expression(paste("Absolute g"[SW], "[mol*m"^-2, "*s"^-1, "]")))
    }
  }


  else {
    if (type=="relgsw") {
      if (is_empty(timestamps)) {
        ##plot observations against relative stomatal conductance (gsw)
        ggplot(licorgeno, mapping=aes(x=obs, y=mean_relgsw))+
          geom_errorbar(mapping=aes(ymin=mean_relgsw - se_rel, ymax=mean_relgsw + se_rel, colour=genotype), alpha=0.5, show.legend = F)+
          geom_point(aes(colour=genotype))+
          #            geom_label(mapping=aes(x=30, y=1.1, label="cloudy"))+
          #            geom_label(mapping=aes(x=50, y=1.1, label="sunny"))+
          #            geom_label(mapping=aes(x=70, y=1.1, label="dark"))+
          scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 25, 50, 75, 100))+
          guides(colour=guide_legend(title=legend_title))+
          scale_colour_manual(values=colourblindpalette,
                              labels=legend_labels)+
          theme_classic()+
          theme(legend.position = "bottom",
                legend.justification="left",
                legend.box.margin = margin(c(-10)),
                legend.background = element_rect(fill=NA))+
          labs(x="Time [min]", y=expression(paste("Relative g"[SW], "[%]")))
      }

      else {
        ##create data frame for vertical lines / timestamps
        timeline<- data.frame(mark=timestamps)

        ##plot observations against relative stomatal conductance (gsw)
        ggplot(licorgeno, mapping=aes(x=obs, y=mean_relgsw))+
          geom_errorbar(mapping=aes(ymin=mean_relgsw - se_rel, ymax=mean_relgsw + se_rel, colour=genotype), alpha=0.5, show.legend = F)+
          geom_point(aes(colour=genotype))+
          geom_vline(timeline, mapping=aes(xintercept=mark), linetype="dotted")+
          #            geom_label(mapping=aes(x=30, y=1.1, label="cloudy"))+
          #            geom_label(mapping=aes(x=50, y=1.1, label="sunny"))+
          #            geom_label(mapping=aes(x=70, y=1.1, label="dark"))+
          scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 25, 50, 75, 100))+
          guides(colour=guide_legend(title=legend_title))+
          scale_colour_manual(values=colourblindpalette,
                              labels=legend_labels)+
          theme_classic()+
          theme(legend.position = "bottom",
                legend.justification="left",
                legend.box.margin = margin(c(-10)),
                legend.background = element_rect(fill=NA))+
          labs(x="Time [min]", y=expression(paste("Relative g"[SW], "[%]")))
      }
    }


    else {
      if (type=="A") {
        if (is_empty(timestamps)) {
          ##plot observations against CO2 assimilation (A)
          ggplot(licorgeno, mapping=aes(x=obs, y=mean_A))+
            geom_errorbar(mapping=aes(ymin=mean_A - se_A, ymax=mean_A + se_A, colour=genotype), alpha=0.5, show.legend = F)+
            geom_point(aes(colour=genotype))+
            #                geom_label(mapping=aes(x=30, y=max(mean_A)+3, label="cloudy"))+
            #                geom_label(mapping=aes(x=50, y=max(mean_A)+3, label="sunny"))+
            #                geom_label(mapping=aes(x=70, y=max(mean_A)+3, label="dark"))+
            guides(colour=guide_legend(title=legend_title))+
            scale_colour_manual(values=colourblindpalette,
                                labels=legend_labels)+
            theme_classic()+
            theme(legend.position = "bottom",
                  legend.justification="left",
                  legend.box.margin = margin(c(-10)),
                  legend.background = element_rect(fill=NA))+
            labs(x="Time [min]", y=expression(paste("CO"[2], " assimilation [mol*m"^-2, "*s"^-1, "]")))
        }

        else{
          timeline<- data.frame(mark=timestamps)

          ggplot(licorgeno, mapping=aes(x=obs, y=mean_A))+
            geom_errorbar(mapping=aes(ymin=mean_A - se_A, ymax=mean_A + se_A, colour=genotype), alpha=0.5, show.legend = F)+
            geom_point(aes(colour=genotype))+
            geom_vline(timeline, mapping=aes(xintercept=mark), linetype="dotted")+
            #                geom_label(mapping=aes(x=30, y=max(mean_A)+3, label="cloudy"))+
            #                geom_label(mapping=aes(x=50, y=max(mean_A)+3, label="sunny"))+
            #                geom_label(mapping=aes(x=70, y=max(mean_A)+3, label="dark"))+
            guides(colour=guide_legend(title=legend_title))+
            scale_colour_manual(values=colourblindpalette,
                                labels=legend_labels)+
            theme_classic()+
            theme(legend.position = "bottom",
                  legend.justification="left",
                  legend.box.margin = margin(c(-10)),
                  legend.background = element_rect(fill=NA))+
            labs(x="Time [min]", y=expression(paste("CO"[2], " assimilation [mol*m"^-2, "*s"^-1, "]")))
        }
      }


      else {
        if (type=="WUE") {
          ##plot observations against water use efficiency (WUE)
          if (is_empty(timestamps)) {
            ggplot(licorgeno, mapping=aes(x=obs, y=mean_WUE))+
              geom_errorbar(mapping=aes(ymin=mean_WUE-se_WUE, ymax=mean_WUE+se_WUE, colour=genotype), alpha=0.5, show.legend = F)+
              geom_point(aes(colour=genotype))+
              #                  geom_label(mapping=aes(x=30, y=max(mean_WUE)+20, label="cloudy"))+
              #                  geom_label(mapping=aes(x=50, y=max(mean_WUE)+20, label="sunny"))+
              #                  geom_label(mapping=aes(x=70, y=max(mean_WUE)+20, label="dark"))+
              guides(colour=guide_legend(title=legend_title))+
              scale_colour_manual(values=colourblindpalette,
                                  labels=legend_labels)+
              theme_classic()+
              theme(legend.position = "bottom",
                    legend.justification="left",
                    legend.box.margin = margin(c(-10)),
                    legend.background = element_rect(fill=NA))+
              labs(x="Time [min]", y=expression(paste("WUE [mol(CO"[2], ") * mol(H"[2],"O)"^-1)))
          }
          else {
            #create df for vertical lines
            timeline<- data.frame(mark=timestamps)

            #plot
            ggplot(licorgeno, mapping=aes(x=obs, y=mean_WUE))+
              geom_errorbar(mapping=aes(ymin=mean_WUE-se_WUE, ymax=mean_WUE+se_WUE, colour=genotype), alpha=0.5, show.legend = F)+
              geom_point(aes(colour=genotype))+
              geom_vline(timeline, mapping=aes(xintercept=timestamps), linetype="dotted")+
              #                  geom_label(mapping=aes(x=30, y=min(mean_WUE)-20, label="cloudy"))+
              #                  geom_label(mapping=aes(x=50, y=min(mean_WUE)-20, label="sunny"))+
              #                  geom_label(mapping=aes(x=70, y=min(mean_WUE)-20, label="dark"))+
              guides(colour=guide_legend(title=legend_title))+
              scale_colour_manual(values=colourblindpalette,
                                  labels=legend_labels)+
              theme_classic()+
              theme(legend.position = "bottom",
                    legend.justification="left",
                    legend.box.margin = margin(c(-10)),
                    legend.background = element_rect(fill=NA))+
              labs(x="Time [min]", y=expression(paste("WUE [mol(CO"[2], ") * mol(H"[2],"O)"^-1, "]")))
          }
        }
      }
    }
  }
}
