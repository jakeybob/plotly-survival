library(survival)
library(survminer)
# library(plotly)
# library(dplyr)
library(ggthemes)  #  for colour palettes
source("plotly-survival.R")


gg_p <- ggsurvplot(survfit(Surv(time, status) ~sex, conf.type="plain", conf.int=0.95, data = lung), conf.int = T)
gg_p <- ggsurvplot(survfit(Surv(time, status) ~1, conf.type="plain", conf.int=0.95, data = lung), conf.int = T)

gg_p <- ggsurvplot(survfit(Surv(time, status) ~ph.ecog, conf.type="plain", conf.int=0.95, data = lung), conf.int = T)

# note palette name change from "tableau10"
plotly_survival(gg_p, strata_colours = tableau_color_pal("Tableau 10")(4),
                          plot_title="Survival of Something",
                          xaxis=list(title="days"),
                          yaxis=list(title="surv", hoverformat=".2f"),
                          plot_CIs = T,
                          CI_opacity = 0.25,
                          CI_linewidth = 0, 
                          censor_size = 10,
                          showlegend_all=T)
