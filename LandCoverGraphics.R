######################################
### Figures for land cover effects ###
######################################

### Hannah White 29.06.2022
### Edited 17.08.2022 to change axis labels

### Set up data

library(nlme)
library(multcomp)
library(RColorBrewer)
library(cowplot)
library(scales)
library(ape)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(forcats)

## Read in data
setwd("~/Documents/GitHub/Trait-diversity-stability")


load('bird.long.RData')
load('CVeffectdivExtraTraits.RData')
load('landcover.RData')


### Merge data

bird.long <- merge(bird.long, cv.effectdiv, by ='Site', all.x = TRUE, all.y = FALSE)
bird.long <- merge(bird.long, lc.agg, by.x = 'Site', by.y = 'Site_ID', all = TRUE)

bird.long$majority <- as.factor(bird.long$majority)
levels(bird.long$majority) <- c('1' = 'Artifical', '2' = 'Agriculture', '3' = 'Forest and semi-natural')
## 11 are urban, 62 are agri and 25 are forest

### Stability and diversity comparisons between land covers

# Response diversity
gls.response <- gls(response.fdis ~ majority, data = bird.long,
                    corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
# multiple comparisons - need to set up gls method
model.matrix.gls <- function(object, ...) {
  model.matrix(terms(object), data = getData(object), ...)
}
model.frame.gls <- function(object, ...) {
  model.frame(formula(object), data = getData(object), ...)
}
terms.gls <- function(object, ...) {
  terms(model.frame(object), ...)
} 

ci.response <-  confint(glht(gls.response, linfct = mcp(majority = "Tukey"))) # estimates differences (not sure if sig)
mc.response <- glht(gls.response, linfct = mcp(majority = "Tukey"))

## Figure

p.response <-
  bird.long %>% 
  ggplot(aes(x = majority, y = response.fdis, colour = majority)) +
  theme_classic(base_size = 14) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title.y = element_text(size = 14)) +
  labs(x = NULL, y = 'Response diversity') + 
  geom_point(aes(shape = majority),col = 'black',
             position = position_jitter(width = 0.35),
             size = 2,
             show.legend = FALSE) + 
  geom_boxplot(aes(fill = majority), col = 'black',
               alpha = 0.6) + 
  scale_color_brewer(palette = "Dark2", direction = -1,aesthetics = c('colour','fill')) + 
  scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural')) + 
  #annotate('text',x = 1, y = 0.138, label = 'a') + 
  #annotate('text', x = c(2, 3), y = c(0.138, 0.138), label = 'b') +
  scale_y_continuous(breaks = breaks_pretty(n = 5)) + 
  scale_shape_manual(values = c(22,2,1)) +
  theme(legend.position = "none")


## Effect trait diversity
gls.effect <- gls(effect.fdis ~ majority, data = bird.long,
                  corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
ci.effect <- confint(glht(gls.effect, linfct = mcp(majority = "Tukey"))) # forest not different from agri but both different from urban

p.effect <-
  bird.long %>% 
  ggplot(aes(x = majority, y = effect.fdis, colour = majority)) +
  theme_classic(base_size = 14) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title.y = element_text(size = 14)) +
  labs(x = NULL, y = 'Effect diversity') + 
  geom_point(aes(shape = majority),col = 'black',
             position = position_jitter(width = 0.35),
             size = 2,
             show.legend = FALSE) + 
  geom_boxplot(aes(fill = majority), col = 'black',
               alpha = 0.6) + 
  scale_color_brewer(palette = "Dark2", direction = -1,aesthetics = c('colour','fill')) + 
  scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural')) + 
  #annotate('text',x = 1, y = 0.17, label = 'a') + 
  #annotate('text', x = c(2, 3), y = c(0.17, 0.17), label = 'b') +
  scale_y_continuous(breaks = breaks_pretty(n = 5)) + 
  scale_shape_manual(values = c(22,2,1)) +
  theme(legend.position = "none")

## Effect trait variability
gls.effectvar <- gls(log(cv.fdis) ~ majority, data = bird.long,
                     corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
ci.effectvar <- confint(glht(gls.effectvar, linfct = mcp(majority = "Tukey"))) # forest significantly different from agriculture

p.effectvar <-
  bird.long %>% 
  ggplot(aes(x = majority, y = log(cv.fdis), colour = majority)) +
  theme_classic(base_size = 14) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title.y = element_text(size = 14)) +
  labs(x = NULL, y = 'Effect diversity variability (ln)') + 
  geom_point(aes(shape = majority),col = 'black',
             position = position_jitter(width = 0.35),
             size = 2,
             show.legend = FALSE) + 
  geom_boxplot(aes(fill = majority), col = 'black',
               alpha = 0.6) + 
  scale_color_brewer(palette = "Dark2", direction = -1,aesthetics = c('colour','fill')) + 
  scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural')) + 
  #annotate('text',x = c(1,2), y = c(-0.65, -0.65), label = 'a') + 
  #annotate('text', x =  3, y = -0.65, label = 'b') +
  scale_y_continuous(breaks = breaks_pretty(n = 5)) + 
  scale_shape_manual(values = c(22,2,1)) +
  theme(legend.position = "none")


## Asynchrony
gls.asyn <- gls(Com.Asynch ~ majority, data = bird.long,
                corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
ci.asyn <- confint(glht(gls.asyn, linfct = mcp(majority = "Tukey"))) # no differences

p.asyn <-
  bird.long %>% 
  ggplot(aes(x = majority, y = Com.Asynch, colour = majority)) +
  theme_classic(base_size = 14) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title.y = element_text(size = 14)) +
  labs(x = NULL, y = 'Community asynchrony') + 
  geom_point(aes(shape = majority),col = 'black',
             position = position_jitter(width = 0.35),
             size = 2,
             show.legend = FALSE) + 
  geom_boxplot(aes(fill = majority), col = 'black',
               alpha = 0.6) + 
  scale_color_brewer(palette = "Dark2", direction = -1,aesthetics = c('colour','fill')) + 
  scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural')) + 
  scale_y_continuous(breaks = breaks_pretty(n = 5)) + 
  scale_shape_manual(values = c(22,2,1)) +
  theme(legend.position = "none")


## Compositional variability
gls.temp <- gls(Temp_Var ~ majority, data = bird.long,
                corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
ci.temp <- confint(glht(gls.temp, linfct = mcp(majority = "Tukey"))) # forest and agri same but both different from urban

p.temp  <-
  bird.long %>% 
  ggplot(aes(x = majority, y = Temp_Var, colour = majority)) +
  theme_classic(base_size = 14) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title.y = element_text(size = 14)) +
  labs(x = NULL, y = 'Compositional Variability') + 
  geom_point(aes(shape = majority),col = 'black',
             position = position_jitter(width = 0.35),
             size = 2,
             show.legend = FALSE) + 
  geom_boxplot(aes(fill = majority), col = 'black',
               alpha = 0.6) + 
  scale_color_brewer(palette = "Dark2", direction = -1,aesthetics = c('colour','fill')) + 
  scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural')) + 
  #annotate('text',x = 1, y = 0.58, label = 'a') + 
  #annotate('text', x = c(2, 3), y = c(0.58, 0.58), label = 'b') +
  scale_y_continuous(breaks = breaks_pretty(n = 5)) + 
  scale_shape_manual(values = c(22,2,1)) +
  theme(legend.position = "none")

## Functional variability
gls.cov <- gls(log(CoV) ~ majority, data = bird.long,
               corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
ci.cov <- confint(glht(gls.cov, linfct = mcp(majority = "Tukey"))) # agri and urabna re significantly diff from eachother

p.cov  <-
  bird.long %>% 
  ggplot(aes(x = majority, y = log(CoV), colour = majority)) +
  theme_classic(base_size = 14) + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title.y = element_text(size = 14)) +
  labs(x = NULL, y = 'Functional Variability (ln)') + 
  geom_point(aes(shape = majority),col = 'black',
             position = position_jitter(width = 0.35),
             size = 2,
             show.legend = FALSE) + 
  geom_boxplot(aes(fill = majority), col = 'black',
               alpha = 0.6) + 
  scale_color_brewer(palette = "Dark2", direction = -1,aesthetics = c('colour','fill')) + 
  scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural')) + 
  #annotate('text',x = 1, y = 0.7, label = 'a') + 
  #annotate('text', x = 2, y = 0.7, label = 'b') +
  #annotate('text', x = 3, y = 0.7, label = 'ab') +
  scale_y_continuous(breaks = breaks_pretty(n = 5)) + 
  scale_shape_manual(values = c(22,2,1)) +
  theme(legend.position = "none")

library(patchwork)
patch <- (p.response + p.asyn) / (p.temp + p.cov) / (p.effect + p.effectvar) +  plot_annotation(tag_levels = "a", tag_suffix = ')')

#tiff('D:\\ResponseEffectDiversity\\ResultsAndFigures\\LandcoverFigures\\LandcoverBoxplots.tiff', res = 360,
#     height = 10, width = 10, unit = 'in', compression = 'lzw')
#cowplot::plot_grid(p.response, p.effect, p.effectvar, p.asyn, p.temp, p.cov, nrow = 3,
#                   labels = c('a)', 'b)', 'c)', 'd)', 'e)', 'f)'))
patch
dev.off()


#### Interaction figures
cov2 <- gls(CoV ~ response.fdis*majority, data = bird.long,
            corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = -16.53042 (-8.476406)

library(sjPlot)
colpal <- RColorBrewer::brewer.pal(3, "Dark2")[3:1]
p.int <- plot_model(cov2, 
                    type = 'int', 
                    show.values = TRUE, 
                    title = '', 
                    #colors = colpal,
                    colors = c('#fd6f77','#bd95b2','#00b0f0')) + 
  theme_classic() + 
  theme(legend.title = element_blank(),legend.position = 'top') + 
  xlab('Response diversity') + ylab('Functional Variability') 

tiff('D:\\ResponseEffectDiversity\\ResultsAndFigures\\LandcoverFigures\\CoVInteraction.tiff', res = 360,
     height = 7, width = 7, unit = 'in', compression = 'lzw')
p.int
dev.off()

