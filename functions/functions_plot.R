plot_ribbons_tempcat <- function(i_df, lower="q15", upper="q85", plotRCP=FALSE, plotSSP=FALSE, plot_netzero=TRUE, xlim=NULL, ylim=NULL, yscale=NULL, ylab="", INVERSE=FALSE, plot_borders=FALSE) {
  require(RColorBrewer)
  
  tmp <- i_df %>% 
    gather(stat, value, -tempcat, -period) %>% 
    filter(stat %in% c(lower, upper)) %>% 
    mutate(stat = ifelse(stat == lower, "lower", "upper")) %>% 
    spread(stat, value) %>% 
    mutate(lower = as.numeric(lower)/1000) %>% 
    mutate(upper = as.numeric(upper)/1000) %>% 
    mutate(tempcat   = factor(tempcat, levels=c("1p5C", "L2C", "M2C", "L3C", "Other"), ordered=TRUE))
  
  if (INVERSE) {
    p = ggplot() +
      # Other scenario
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill=tempcat), 
                  data=tmp %>% filter(tempcat == "Other")) +
      # 1.5°C scenario
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill=tempcat), 
                  data=tmp %>% filter(tempcat == "1p5C")) +
      # Likely 2.0°C scenario
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill=tempcat), 
                  data=tmp %>% filter(tempcat == "L2C")) +
      # Medium 2.0°C scenario
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill=tempcat), 
                  data=tmp %>% filter(tempcat == "M2C")) +
      # Likely 3.0°C scenario
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill=tempcat), 
                  data=tmp %>% filter(tempcat == "L3C"))
    
    if (plot_borders) {
      p = p +
        # Other scenario
        geom_line(aes(x=period, y=lower, colour=tempcat), lty=2,
                  data=tmp %>% filter(tempcat == "Other scenario")) +
        # 1.5°C scenario
        geom_line(aes(x=period, y=lower, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "1p5C")) +
        # Likely 2.0°C scenario
        geom_line(aes(x=period, y=lower, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "L2C")) +
        # Medium 2.0°C scenario
        geom_line(aes(x=period, y=lower, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "M2C")) +
        # Likely 3.0°C scenario
        geom_line(aes(x=period, y=lower, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "L3C"))
      
      p = p +
        # Other scenario
        geom_line(aes(x=period, y=upper, colour=tempcat), lty=2,
                  data=tmp %>% filter(tempcat == "Other scenario")) +
        # 1.5°C scenario
        geom_line(aes(x=period, y=upper, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "1p5C")) +
        # Likely 2.0°C scenario
        geom_line(aes(x=period, y=upper, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "L2C")) +
        # Medium 2.0°C scenario
        geom_line(aes(x=period, y=upper, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "M2C")) +
        # Likely 3.0°C scenario
        geom_line(aes(x=period, y=upper, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "L3C"))
      
      p = p +
        scale_colour_manual(name="dGMT ceilings", values=c("1p5C"        = brewer.pal(8, "Blues")[8], 
                                                                     "L2C" = brewer.pal(8, "Blues")[6], 
                                                                     "M2C" = brewer.pal(8, "Blues")[4],
                                                                     "L3C" = brewer.pal(8, "Blues")[2],
                                                                     "Other scenario"        = "#eeeeee"))
    }
  } else  {
    p = ggplot() +
      # Other scenario
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill=tempcat), 
                  data=tmp %>% filter(tempcat == "Other scenario")) +
      # Likely 3.0°C scenario
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill=tempcat), 
                  data=tmp %>% filter(tempcat == "L3C")) +
      # Medium 2.0°C scenario
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill=tempcat), 
                  data=tmp %>% filter(tempcat == "M2C")) +
      # Likely 2.0°C scenario
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill=tempcat), 
                  data=tmp %>% filter(tempcat == "L2C")) +
      # 1.5°C scenario
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill=tempcat), 
                  data=tmp %>% filter(tempcat == "1p5C"))
    
    if (plot_borders) {
      p = p +
        # Other scenario
        geom_line(aes(x=period, y=lower, colour=tempcat), lty=2,
                  data=tmp %>% filter(tempcat == "Other scenario")) +
        # Likely 3.0°C scenario
        geom_line(aes(x=period, y=lower, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "L3C")) +
        # Medium 2.0°C scenario
        geom_line(aes(x=period, y=lower, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "M2C")) +
        # Likely 2.0°C scenario
        geom_line(aes(x=period, y=lower, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "L2C")) +
        # 1.5°C scenario
        geom_line(aes(x=period, y=lower, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "1p5C"))
      
      p = p +
        # Other scenario
        geom_line(aes(x=period, y=upper, colour=tempcat), lty=2,
                  data=tmp %>% filter(tempcat == "Other scenario")) +
        # Likely 3.0°C scenario
        geom_line(aes(x=period, y=upper, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "L3C")) +
        # Medium 2.0°C scenario
        geom_line(aes(x=period, y=upper, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "M2C")) +
        # Likely 2.0°C scenario
        geom_line(aes(x=period, y=upper, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "L2C")) +
        # 1.5°C scenario
        geom_line(aes(x=period, y=upper, colour=tempcat), lty=2, 
                  data=tmp %>% filter(tempcat == "1p5C"))
      
      
      p = p +
        scale_colour_manual(name="dGMT ceilings", values=c("1p5C"        = brewer.pal(8, "Blues")[8], 
                                                                     "L2C" = brewer.pal(8, "Blues")[6], 
                                                                     "M2C" = brewer.pal(8, "Blues")[4],
                                                                     "L3C" = brewer.pal(8, "Blues")[2],
                                                                     "Other scenario"        = "#eeeeee"))
    }
  }
  
  if (plotRCP) {
    p = p +
      # RCP
      geom_line(aes(x=period, y=value*44/12, group=scenario),
                colour = "lightgrey",
                lwd=1.4,
                data=data_rcp %>% filter(region == "World", scenario != "IMAGE - RCP3-PD (2.6)", variable == "CO2 emissions - Total")) +
      geom_point(aes(x=period, y=value*44/12, group=scenario),
                 colour = "lightgrey",
                 fill = "white",
                 pch=21,
                 data=data_rcp %>% filter(region == "World", scenario != "IMAGE - RCP3-PD (2.6)", variable == "CO2 emissions - Total")) +
      geom_line(aes(x=period, y=value*44/12, group=scenario),
                colour = brewer.pal(8, "Blues")[6],
                lwd=1.4,
                data=data_rcp %>% filter(region == "World", scenario == "IMAGE - RCP3-PD (2.6)", variable == "CO2 emissions - Total")) +
      geom_point(aes(x=period, y=value*44/12, group=scenario),
                 colour = brewer.pal(8, "Blues")[6],
                 fill = "white",
                 pch=21,
                 data=data_rcp %>% filter(region == "World", scenario == "IMAGE - RCP3-PD (2.6)", variable == "CO2 emissions - Total"))
  }
  
  if (plotSSP) {
    tmp_data <- v_data5 %>%
      filter(region    == "World",
             target    == "26",
             variable  == "Emissions|CO2") %>%
      group_by(period) %>%
      summarise(
        min  = min(value, na.rm=TRUE),
        q05  = quantile(value, 0.05, na.rm=TRUE),
        q10  = quantile(value, 0.10, na.rm=TRUE),
        q15  = quantile(value, 0.15, na.rm=TRUE),
        q25  = quantile(value, 0.25, na.rm=TRUE),
        med  = median(value, na.rm=TRUE),
        mean = mean(value, na.rm=TRUE),
        q75  = quantile(value, 0.75, na.rm=TRUE),
        q85  = quantile(value, 0.85, na.rm=TRUE),
        q90  = quantile(value, 0.90, na.rm=TRUE),
        q95  = quantile(value, 0.95, na.rm=TRUE),
        max  = max(value, na.rm=TRUE),
        count=n()) %>%
      ungroup() %>%
      gather(stat, value, -period) %>%
      filter(stat %in% c(lower, upper)) %>%
      mutate(stat = ifelse(stat == lower, "lower", "upper")) %>%
      spread(stat, value) %>%
      mutate(lower = as.numeric(lower)/1000) %>%
      mutate(upper = as.numeric(upper)/1000)
    p = p +
      # SSP2-2.6W/m2
      geom_line(aes(x=period, y=value/1000, group=scenario.model),
                colour = "black",
                lwd=1.4,
                data=v_data5 %>% filter(region == "World", scenario.model == "SSP2-26.MESSAGE-GLOBIOM", variable == "Emissions|CO2")) +
      geom_point(aes(x=period, y=value/1000, group=scenario.model),
                 colour = "#000000",
                 fill = "#ffffff", #brewer.pal(8, "Blues")[6],
                 pch=21,
                 data=v_data5 %>% filter(region == "World", scenario.model == "SSP2-26.MESSAGE-GLOBIOM", variable == "Emissions|CO2"))
    # SSP 1.9W/m2 (data not yet available)
  }

  if (plot_netzero) {
    # 0 line
    p = p + geom_segment(aes(x=2005, xend=2100, y=0, yend=0), color="red", lwd=1.2, lty=2)
  }
  
  p = p +    
    theme_bw() +
    theme(
      legend.text = element_text(size=8),
      legend.position = c(0.18, 0.82), 
      legend.background = element_rect(color = "black", 
                                       fill = "grey90", 
                                       size = 1, 
                                       linetype = "solid")
    ) +
    scale_x_continuous(breaks = seq(2010,2100,10), expand=c(0,0)) +
    scale_fill_manual(name="dGMT ceilings", values=c("1p5C"        = brewer.pal(8, "Blues")[8], 
                                                               "L2C" = brewer.pal(8, "Blues")[6], 
                                                               "M2C" = brewer.pal(8, "Blues")[4],
                                                               "L3C" = brewer.pal(8, "Blues")[2],
                                                               "Other scenario"        = "#eeeeee")) + 
    xlab("") + ylab("") + ggtitle(ylab)
  
  if (!is.null(xlim)) p = p + xlim(xlim)
  if (!is.null(ylim)) p = p + ylim(ylim)
  if (!is.null(yscale)) p = p + scale_y_continuous(breaks = yscale, expand=c(0,0))
  
  print(p)
  
  return(p)
}

plot_ribbons_allcat_grid <- function(i_df, lower="q15", upper="q85", plotRCP=TRUE, plot_netzero=TRUE, xlim=NULL, ylim=NULL, yscale=NULL, ylab="", INVERSE=FALSE, plot_borders=FALSE) {
  require(RColorBrewer)
  
  tmp <- i_df
  
  p = ggplot() +
    geom_ribbon(aes(x=period, ymin=q15, ymax=q85, fill=allcat), data=tmp) +
    facet_grid(allcat~tempcat)
  
  if (plotRCP) {
    p = p +
      # RCP
      geom_line(aes(x=period, y=value*44/12, group=scenario),
                colour = brewer.pal(8, "Blues")[6],
                lwd=1.4,
                data=data_rcp %>% filter(region == "World", scenario == "IMAGE - RCP3-PD (2.6)", variable == "CO2 emissions - Total")) +
      geom_point(aes(x=period, y=value*44/12, group=scenario),
                 colour = brewer.pal(8, "Blues")[6],
                 fill = "white",
                 pch=21,
                 data=data_rcp %>% filter(region == "World", scenario == "IMAGE - RCP3-PD (2.6)", variable == "CO2 emissions - Total"))
  }
  
  
  p = p +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.margin = unit(c(0.5,0.75,0.5,0.5), units = "cm"),
      axis.line  = element_line(size=1.05),
      axis.ticks = element_line(size=1.05),
      axis.title = element_text(colour="black", size = 14),
      axis.text  = element_text(colour="grey", size = 14),
      axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
    scale_x_continuous(breaks = seq(2010,2100,10), expand=c(0,0)) +
    xlab("") + ylab(ylab) +
    scale_fill_manual(name="Category", values=c("Default"                         = "#0072B2", 
                                                "Limited bioenergy"               = "#009E73", 
                                                "Low energy intensity"            = "#56B4E9", #"#CC79A7",
                                                "No CCS/BECCS"                    = "#E69F00",
                                                "Limited bioenergy and no CCS/BECCS" = "#009E73",
                                                "Delayed action until 2030"       = "#D55E00"))
  
  if (!is.null(xlim)) p = p + xlim(xlim)
  if (!is.null(ylim)) p = p + ylim(ylim)
  if (!is.null(yscale)) p = p + scale_y_continuous(breaks = yscale, expand=c(0,0))
  
  print(p)
  
  return(p)
}

plot_ribbons_allcat_byTemp <- function(i_df, lower="q15", upper="q85", plotRCP=FALSE, plotSSP=FALSE, plot_netzero=TRUE, xlim=NULL, ylim=NULL, yscale=NULL, ylab="", INVERSE=FALSE, plot_borders=FALSE) {
  require(RColorBrewer)
  
  tmp <- i_df
  
  p = ggplot() +
    geom_ribbon(aes(x=period, ymin=q15, ymax=q85, fill=allcat), alpha=0.3, 
                data=tmp)
  
  if (plotRCP) {
    p = p +
      # RCP
      geom_line(aes(x=period, y=value*44/12, group=scenario),
                colour = brewer.pal(8, "Blues")[6],
                lwd=1.4,
                data=data_rcp %>% filter(region == "World", scenario == "IMAGE - RCP3-PD (2.6)", variable == "CO2 emissions - Total")) +
      geom_point(aes(x=period, y=value*44/12, group=scenario),
                 colour = brewer.pal(8, "Blues")[6],
                 fill = "white",
                 pch=21,
                 data=data_rcp %>% filter(region == "World", scenario == "IMAGE - RCP3-PD (2.6)", variable == "CO2 emissions - Total"))
  }
  
  if (plotSSP) {
    tmp_data <- v_data5 %>%
      filter(region    == "World", 
             target    == "26", 
             variable  == "Emissions|CO2") %>% 
      group_by(period) %>%
      summarise(
        min  = min(value, na.rm=TRUE), 
        q05  = quantile(value, 0.05, na.rm=TRUE), 
        q10  = quantile(value, 0.10, na.rm=TRUE), 
        q15  = quantile(value, 0.15, na.rm=TRUE), 
        q25  = quantile(value, 0.25, na.rm=TRUE), 
        med  = median(value, na.rm=TRUE), 
        mean = mean(value, na.rm=TRUE), 
        q75  = quantile(value, 0.75, na.rm=TRUE), 
        q85  = quantile(value, 0.85, na.rm=TRUE), 
        q90  = quantile(value, 0.90, na.rm=TRUE), 
        q95  = quantile(value, 0.95, na.rm=TRUE), 
        max  = max(value, na.rm=TRUE),
        count=n()) %>% 
      ungroup() %>% 
      gather(stat, value, -period) %>% 
      filter(stat %in% c(lower, upper)) %>% 
      mutate(stat = ifelse(stat == lower, "lower", "upper")) %>% 
      spread(stat, value) %>% 
      mutate(lower = as.numeric(lower)/1000) %>% 
      mutate(upper = as.numeric(upper)/1000)
    p = p +
      geom_line(aes(x=period, y=value/1000, group=scenario.model),
                colour = "black",
                lwd=1.4,
                data=v_data5 %>% filter(region == "World", scenario.model == "SSP2-26.MESSAGE-GLOBIOM", variable == "Emissions|CO2")) +
      geom_point(aes(x=period, y=value/1000, group=scenario.model),
                 colour = "#000000",
                 fill = "#ffffff",
                 pch=21,
                 data=v_data5 %>% filter(region == "World", scenario.model == "SSP2-26.MESSAGE-GLOBIOM", variable == "Emissions|CO2"))
  }
  
  if (plot_netzero) {
    # 0 line
    p = p + geom_segment(aes(x=2005, xend=2100, y=0, yend=0), color="red", lwd=1.2, lty=2)
  }
  
  p = p +
    theme_bw() +
    theme(
      legend.text = element_text(size=8),
      legend.position = c(0.82, 0.82), 
      legend.background = element_rect(color = "black", 
                                       fill = "grey90", 
                                       size = 1, 
                                       linetype = "solid")) +
    scale_x_continuous(breaks = seq(2010,2100,10), expand=c(0,0)) +
    xlab("") + ylab("") + ggtitle(ylab) +
    scale_fill_manual(name="Category", values=c("Default"                         = "#0072B2", 
                                                "Limited bioenergy"               = "#009E73", 
                                                "Low energy intensity"            = "#56B4E9", #"#CC79A7",
                                                "No CCS/BECCS"                    = "#E69F00",
                                                "Limited bioenergy and no CCS/BECCS" = "#009E73",
                                                "Delayed action until 2030"       = "#D55E00"))
  
  if (!is.null(xlim)) p = p + xlim(xlim)
  if (!is.null(ylim)) p = p + ylim(ylim)
  if (!is.null(yscale)) p = p + scale_y_continuous(breaks = yscale, expand=c(0,0))
  
  print(p)
  
  return(p)
}

plot_ribbons_allcat_2C <- function(i_df, lower="q15", upper="q85", plotRCP=TRUE, plot_netzero=TRUE, xlim=NULL, ylim=NULL, yscale=NULL, ylab="", INVERSE=FALSE, plot_borders=FALSE) {
  require(RColorBrewer)
  
  tmp <- i_df
  
  p = ggplot() +
    geom_ribbon(aes(x=period, ymin=q15, ymax=q85, fill=allcat), alpha=0.3, 
                data=tmp)
  
  if (plotRCP) {
    p = p +
      # RCP
      geom_line(aes(x=period, y=value*44/12, group=scenario),
                colour = brewer.pal(8, "Blues")[6],
                lwd=1.4,
                data=data_rcp %>% filter(region == "World", scenario == "IMAGE - RCP3-PD (2.6)", variable == "CO2 emissions - Total")) +
      geom_point(aes(x=period, y=value*44/12, group=scenario),
                 colour = brewer.pal(8, "Blues")[6],
                 fill = "white",
                 pch=21,
                 data=data_rcp %>% filter(region == "World", scenario == "IMAGE - RCP3-PD (2.6)", variable == "CO2 emissions - Total"))
  }
  
  p = p +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.margin = unit(c(0.5,0.75,0.5,0.5), units = "cm"),
      axis.line  = element_line(size=1.05),
      axis.ticks = element_line(size=1.05),
      axis.title = element_text(colour="black", size = 14),
      axis.text  = element_text(colour="grey", size = 14),
      axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
    scale_x_continuous(breaks = seq(2010,2100,10), expand=c(0,0)) +
    xlab("") + ylab(ylab)
  
  if (!is.null(xlim)) p = p + xlim(xlim)
  if (!is.null(ylim)) p = p + ylim(ylim)
  if (!is.null(yscale)) p = p + scale_y_continuous(breaks = yscale, expand=c(0,0))
  
  print(p)
  
  return(p)
}

plot_cumulative_boxplots <- function(i_df, lower="q15", upper="q85", ylab="", xlim=NULL, ylim=NULL, yscale=NULL, ADD_STATS=NULL) {
  
  tmp <- i_df %>% 
    gather(stat, value, -tempcat, -period) %>% 
    filter(stat %in% c(lower, upper, "med", "mean", "min", "max")) %>% 
    mutate(stat = ifelse(stat == lower, "lower", stat)) %>% 
    mutate(stat = ifelse(stat == upper, "upper", stat)) %>% 
    spread(stat, value)
  
  p = ggplot(data = tmp) +
    geom_boxplot(aes(x=tempcat, lower=lower, upper=upper, middle=med, ymin=min, ymax=max, fill=tempcat), stat="identity") +
    geom_point(aes(x=tempcat, y=mean), pch=21, color="black", fill="white", size=2.25)  +
    theme_bw() +
    theme(legend.position = c(0.82, 0.85), 
          legend.background = element_rect(color = "black", 
                                           fill = "grey90", 
                                           size = 1, 
                                           linetype = "solid"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    xlab("") + ylab("") + ggtitle(ylab) +
    scale_fill_manual(name="dGMT ceilings", values=c("1p5C"        = brewer.pal(8, "Blues")[8], 
                                                               "L2C" = brewer.pal(8, "Blues")[6], 
                                                               "M2C" = brewer.pal(8, "Blues")[4],
                                                               "L3C" = brewer.pal(8, "Blues")[2],
                                                               "Other scenario"        = "#eeeeee"))
  
  
  if (!is.null(xlim)) p = p + xlim(xlim)
  if (!is.null(ylim)) p = p + ylim(ylim)
  if (!is.null(yscale)) p = p + scale_y_continuous(breaks = yscale, expand=c(0,0), limits=ylim)
  
  if (!is.null(ADD_STATS)) {
    
    tmp2 <- ADD_STATS %>% 
      as.data.frame() %>% 
      group_by(tempcat) %>% 
      summarise(
        count_model    = length(unique(model)),
        count_scenario = length(unique(scenario)),
        count = n()
      ) %>% 
      ungroup()
    
    p <- p +
      guides(fill=FALSE)+
      theme(plot.margin = unit(c(2,1,5,1), "lines"))
    itr <- 0
    for (var in c("count", "count_model", "count_scenario")) {
      text_15 <- textGrob(tmp2[[var]][which(tmp$tempcat == "1p5C")],        gp=gpar(fontsize=13, col="grey"))
      text_L2 <- textGrob(tmp2[[var]][which(tmp$tempcat == "L2C")], gp=gpar(fontsize=13, col="grey"))
      text_M2 <- textGrob(tmp2[[var]][which(tmp$tempcat == "M2C")], gp=gpar(fontsize=13, col="grey"))
      text_L3 <- textGrob(tmp2[[var]][which(tmp$tempcat == "L3C")], gp=gpar(fontsize=13, col="grey"))
      
      label    <- ifelse(var == "count", "Pathways", ifelse(var == "count_model", "Models", "Scenarios"))
      text_lab <- textGrob(label, gp=gpar(fontsize=13, fontface="bold", col="grey"))
      
      yoffset1 <- 0.10*(ylim[2]-ylim[1])
      yoffset2 <- 0.6*yoffset1
      
      p <- p +
        annotation_custom(text_lab,xmin=0.2,xmax=0.2,ymin=-yoffset1-yoffset2*itr,ymax=-yoffset1-yoffset2*itr) + 
        annotation_custom(text_15, xmin=1,  xmax=1,  ymin=-yoffset1-yoffset2*itr,ymax=-yoffset1-yoffset2*itr) + 
        annotation_custom(text_L2, xmin=2,  xmax=2,  ymin=-yoffset1-yoffset2*itr,ymax=-yoffset1-yoffset2*itr) +
        annotation_custom(text_M2, xmin=3,  xmax=3,  ymin=-yoffset1-yoffset2*itr,ymax=-yoffset1-yoffset2*itr) +
        annotation_custom(text_L3, xmin=4,  xmax=4,  ymin=-yoffset1-yoffset2*itr,ymax=-yoffset1-yoffset2*itr)
      
      itr <- itr+1
    }
    
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid.draw(gt)
    
    p <- gt
  } else {
    print(p)
  }
  
  return(p)
}

plot_cumulative_boxplots_allcat <- function(i_df, lower="q15", upper="q85", ylab="", xlim=NULL, ylim=NULL, yscale=NULL, ADD_STATS=NULL) {
  
  tmp <- i_df
  
  p = ggplot(data = tmp) +
    geom_boxplot(aes(x=allcat, lower=q15, upper=q85, middle=med, ymin=min, ymax=max, fill=allcat), stat="identity") +
    geom_point(aes(x=allcat, y=mean), pch=21, color="black", fill="white", size=2.25) +
    theme_bw() +
    theme(legend.position = "bottom", 
          axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))  +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    xlab("") + ylab("") + ggtitle(ylab) +
    scale_fill_manual(name="Category", values=c("Default"                         = "#0072B2", 
                                                "Limited bioenergy"               = "#009E73", 
                                                "Low energy intensity"            = "#56B4E9", #"#CC79A7",
                                                "No CCS/BECCS"                    = "#E69F00",
                                                "Limited bioenergy and no CCS/BECCS" = "#009E73",
                                                "Delayed action until 2030"       = "#D55E00"))
  
  
  if (!is.null(xlim)) p = p + xlim(xlim)
  if (!is.null(ylim)) p = p + ylim(ylim)
  if (!is.null(yscale)) p = p + scale_y_continuous(breaks = yscale, expand=c(0,0), limits=ylim)
  
  if (!is.null(ADD_STATS)) {
    
    tmp2 <- ADD_STATS %>% 
      as.data.frame() %>% 
      group_by(allcat) %>% 
      summarise(
        count_model    = length(unique(model)),
        count_scenario = length(unique(scenario)),
        count = n()
      ) %>% 
      ungroup()
    
    p <- p +
      guides(fill=FALSE) +
      theme(plot.margin = unit(c(2,1,5,1), "lines"))
    itr <- 0
    for (var in c("count", "count_model", "count_scenario")) {
      text_De <- textGrob(tmp2[[var]][which(tmp$allcat == "Default")],        gp=gpar(fontsize=13, col="grey"))
      text_LE <- textGrob(tmp2[[var]][which(tmp$allcat == "Low energy intensity")], gp=gpar(fontsize=13, col="grey"))
      text_LB <- textGrob(tmp2[[var]][which(tmp$allcat == "Limited bioenergy and no CCS/BECCS")], gp=gpar(fontsize=13, col="grey"))
      text_De <- textGrob(tmp2[[var]][which(tmp$allcat == "Delayed action until 2030")], gp=gpar(fontsize=13, col="grey"))
      
      label    <- ifelse(var == "count", "Observations", ifelse(var == "count_model", "Models", "Scenarios"))
      text_lab <- textGrob(label, gp=gpar(fontsize=13, fontface="bold", col="grey"))
      
      yoffset1 <- 70
      yoffset2 <- 40
      
      p <- p +
        annotation_custom(text_lab,xmin=0.2,xmax=0.2,ymin=-yoffset1-yoffset2*itr,ymax=-yoffset1-yoffset2*itr) + 
        annotation_custom(text_De, xmin=1,  xmax=1,  ymin=-yoffset1-yoffset2*itr,ymax=-yoffset1-yoffset2*itr) + 
        annotation_custom(text_LE, xmin=2,  xmax=2,  ymin=-yoffset1-yoffset2*itr,ymax=-yoffset1-yoffset2*itr) +
        annotation_custom(text_LB, xmin=3,  xmax=3,  ymin=-yoffset1-yoffset2*itr,ymax=-yoffset1-yoffset2*itr) +
        annotation_custom(text_De, xmin=4,  xmax=4,  ymin=-yoffset1-yoffset2*itr,ymax=-yoffset1-yoffset2*itr)
      
      itr <- itr+1
    }
    
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid.draw(gt)
    
    p <- gt
  } else {
    print(p)
  }
  
  return(p)
}

plot_deployment_boxplots <- function(i_df, lower="q15", upper="q85", ylab="", xlim=NULL, ylim=NULL, yscale=NULL) {
  
  tmp <- i_df %>% 
    gather(stat, value, -tempcat, -period) %>% 
    filter(stat %in% c(lower, upper, "med", "mean", "min", "max")) %>% 
    mutate(stat = ifelse(stat == lower, "lower", stat)) %>% 
    mutate(stat = ifelse(stat == upper, "upper", stat)) %>% 
    #mutate(value = value/1000) %>% 
    spread(stat, value)
  
  p = ggplot(data = tmp) +
    geom_boxplot(aes(x=tempcat, lower=lower, upper=upper, middle=med, ymin=min, ymax=max, fill=tempcat), stat="identity") +
    geom_point(aes(x=tempcat, y=mean), pch=21, color="black", fill="white", size=2.25) +
    facet_wrap(~period, ncol=length(unique(tmp$period))) + 
    theme_bw() +
    theme(legend.position="bottom") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    xlab("") + ylab("") + ggtitle(ylab) +
    scale_fill_manual(name="dGMT ceilings", values=c("1p5C"        = brewer.pal(8, "Blues")[8], 
                                                               "L2C" = brewer.pal(8, "Blues")[6], 
                                                               "M2C" = brewer.pal(8, "Blues")[4],
                                                               "L3C" = brewer.pal(8, "Blues")[2],
                                                               "Other scenario"        = "#eeeeee"))
  

  if (!is.null(xlim)) p = p + xlim(xlim)
  if (!is.null(ylim)) p = p + ylim(ylim)
  if (!is.null(yscale)) p = p + scale_y_continuous(breaks = yscale, expand=c(0,0), limits=ylim)
  
  print(p)
  
  return(p)
}

plot_deployment_boxplots_allcat <- function(i_df, lower="q15", upper="q85", ylab="", xlim=NULL, ylim=NULL, yscale=NULL) {
  
  tmp <- i_df 
  
  p = ggplot(data = tmp) +
    geom_boxplot(aes(x=allcat, lower=q15, upper=q85, middle=med, ymin=min, ymax=max, fill=allcat), stat="identity") +
    geom_point(aes(x=allcat, y=mean), pch=21, fill="white", colour="black", size=2.25) +
    facet_wrap(~period, ncol=3) + 
    theme_bw() +
    theme(legend.position = "bottom", 
          axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    xlab("") + ylab("") + ggtitle(ylab) +
    scale_fill_manual(name="Category", values=c("Default"                         = "#0072B2", 
                                                "Limited bioenergy"               = "#009E73", 
                                                "Low energy intensity"            = "#56B4E9", #"#CC79A7",
                                                "No CCS/BECCS"                    = "#E69F00",
                                                "Limited bioenergy and no CCS/BECCS" = "#009E73",
                                                "Delayed action until 2030"       = "#D55E00"))
  
  
  if (!is.null(xlim)) p = p + xlim(xlim)
  if (!is.null(ylim)) p = p + ylim(ylim)
  if (!is.null(yscale)) p = p + scale_y_continuous(breaks = yscale, expand=c(0,0), limits=ylim)
  
  print(p)
  
  return(p)
}

plot_avgDeployRate20302050_boxplots <- function(i_df1,i_df2, lower="q15", upper="q85", ylab="", xlim=NULL, ylim=NULL, yscale=NULL, ADD_STATS=NULL) {
  
  tmp <- i_df2 %>% 
    gather(stat, value, -tempcat, -period) %>% 
    filter(stat %in% c(lower, upper, "med", "mean", "min", "max")) %>% 
    mutate(stat = ifelse(stat == lower, "lower", stat)) %>% 
    mutate(stat = ifelse(stat == upper, "upper", stat)) %>% 
    spread(stat, value)
  
  p = ggplot() +
    geom_boxplot(aes(x=tempcat, lower=lower, upper=upper, middle=med, ymin=min, ymax=max, fill=tempcat), stat="identity", data = tmp) +
    geom_point(aes(x=tempcat, y=mean), pch=21, color="black", fill="white", size=2.25, data = tmp)  +
    theme_bw() +
    theme(legend.position="bottom") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    xlab("") + ylab("") + ggtitle(ylab) +
    scale_fill_manual(name="Paris temperature goals", values=c("1p5C"        = brewer.pal(8, "Blues")[8], 
                                                               "L2C" = brewer.pal(8, "Blues")[6], 
                                                               "M2C" = brewer.pal(8, "Blues")[4],
                                                               "L3C" = brewer.pal(8, "Blues")[2],
                                                               "Other scenario"        = "#eeeeee"))
  
  
  if (!is.null(xlim)) p = p + xlim(xlim)
  if (!is.null(ylim)) p = p + ylim(ylim)
  if (!is.null(yscale)) p = p + scale_y_continuous(breaks = yscale, expand=c(0,0), limits=ylim)
  
  if (!is.null(ADD_STATS)) {
    
    tmp2 <- ADD_STATS %>% 
      as.data.frame() %>% 
      group_by(tempcat) %>% 
      summarise(
        count_model    = length(unique(model)),
        count_scenario = length(unique(scenario)),
        count = n()
      ) %>% 
      ungroup()
    
    p <- p +
      guides(fill=FALSE) +
      theme(plot.margin = unit(c(2,1,5,1), "lines"))
    itr <- 0
    for (var in c("count", "count_model", "count_scenario")) {
      text_15 <- textGrob(tmp2[[var]][which(tmp$tempcat == "1p5C")],        gp=gpar(fontsize=13, col="grey"))
      text_L2 <- textGrob(tmp2[[var]][which(tmp$tempcat == "L2C")], gp=gpar(fontsize=13, col="grey"))
      text_M2 <- textGrob(tmp2[[var]][which(tmp$tempcat == "M2C")], gp=gpar(fontsize=13, col="grey"))
      text_L3 <- textGrob(tmp2[[var]][which(tmp$tempcat == "L3C")], gp=gpar(fontsize=13, col="grey"))
      
      label    <- ifelse(var == "count", "Pathways", ifelse(var == "count_model", "Models", "Scenarios"))
      text_lab <- textGrob(label, gp=gpar(fontsize=13, fontface="bold", col="grey"))
      
      yoffset1 <- 0.10*(ylim[2]-ylim[1])
      yoffset2 <- 0.5*yoffset1
      
      p <- p +
        annotation_custom(text_lab,xmin=0.2,xmax=0.2,ymin=ylim[1]-yoffset1-yoffset2*itr,ymax=ylim[1]-yoffset1-yoffset2*itr) + 
        annotation_custom(text_15, xmin=1,  xmax=1,  ymin=ylim[1]-yoffset1-yoffset2*itr,ymax=ylim[1]-yoffset1-yoffset2*itr) + 
        annotation_custom(text_L2, xmin=2,  xmax=2,  ymin=ylim[1]-yoffset1-yoffset2*itr,ymax=ylim[1]-yoffset1-yoffset2*itr) +
        annotation_custom(text_M2, xmin=3,  xmax=3,  ymin=ylim[1]-yoffset1-yoffset2*itr,ymax=ylim[1]-yoffset1-yoffset2*itr) +
        annotation_custom(text_L3, xmin=4,  xmax=4,  ymin=ylim[1]-yoffset1-yoffset2*itr,ymax=ylim[1]-yoffset1-yoffset2*itr)
      
      itr <- itr+1
    }
    
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid.draw(gt)
    
    p <- gt
  } else {
    print(p)
  }
  
  return(p)
}

plot_avgDeployRate20302050_boxplots_allcat <- function(i_df, lower="q15", upper="q85", ylab="", xlim=NULL, ylim=NULL, yscale=NULL) {
  
  tmp <- i_df 
  
  p <- ggplot(data = tmp) +
    geom_boxplot(aes(x=allcat, lower=lower, upper=upper, middle=med, ymin=min, ymax=max, fill=allcat), stat="identity") +
    geom_point(aes(x=allcat, y=mean), pch=21, fill="white", colour="black", size=2.25) +
    facet_wrap(~tempcat) +
    theme_bw() +
    theme(legend.position="bottom") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    xlab("") + ylab(ylab)
  
  print(p)
  
  return(p)
}



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}