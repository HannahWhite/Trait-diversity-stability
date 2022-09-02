######################################
### Figures for land cover effects ###
######################################

### Hannah White 29.06.2022
### Edited 17.08.2022 to change axis labels

### Set up data

library(nlme)
library(multcomp)
library(ggplot2)
library(RColorBrewer)
library(cowplot)

## Read in data
setwd('')


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
p.response <- ggplot(bird.long, aes(x = majority, y = response.fdis, colour = majority)) + geom_boxplot()
p.response <- p.response + xlab(NULL) + ylab('Response Trait FDis')
p.response <- p.response + scale_color_brewer(palette = "Dark2", direction = -1)
p.response <- p.response + theme_classic() + theme(legend.position = 'none', 
                                                   axis.text = element_text(size = 14),
                                                   axis.title.y = element_text(size = 14))
p.response <- p.response + scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural'))
p.response <- p.response + annotate('text',x = 1, y = 0.138, label = 'a')
p.response <- p.response + annotate('text', x = c(2, 3), y = c(0.138, 0.138), label = 'b')


## Effect trait diversity
gls.effect <- gls(effect.fdis ~ majority, data = bird.long,
                  corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
ci.effect <- confint(glht(gls.effect, linfct = mcp(majority = "Tukey"))) # forest not different from agri but both different from urban

p.effect <- ggplot(bird.long, aes(x = majority, y = effect.fdis, colour = majority)) + geom_boxplot()
p.effect <- p.effect + xlab(NULL) + ylab('Effect Trait FDis')
p.effect <- p.effect + scale_color_brewer(palette = "Dark2", direction = -1)
p.effect <- p.effect + theme_classic() + theme(legend.position = 'none', 
                                               axis.text = element_text(size = 14),
                                               axis.title.y = element_text(size = 14))
p.effect <- p.effect + scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural'))
p.effect <- p.effect + annotate('text',x = 1, y = 0.17, label = 'a')
p.effect <- p.effect + annotate('text', x = c(2, 3), y = c(0.17, 0.17), label = 'b')


## Effect trait variability
gls.effectvar <- gls(log(cv.fdis) ~ majority, data = bird.long,
                     corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
ci.effectvar <- confint(glht(gls.effectvar, linfct = mcp(majority = "Tukey"))) # forest significantly different from agriculture

p.effectvar <- ggplot(bird.long, aes(x = majority, y = log(cv.fdis), colour = majority)) + geom_boxplot()
p.effectvar <- p.effectvar + xlab(NULL) + ylab('Effect Trait FDis Variability (log)')
p.effectvar <- p.effectvar + scale_color_brewer(palette = "Dark2", direction = -1)
p.effectvar <- p.effectvar + theme_classic() + theme(legend.position = 'none', 
                                                     axis.text = element_text(size = 14),
                                                     axis.title.y = element_text(size = 14))
p.effectvar <- p.effectvar + scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural'))
p.effectvar <- p.effectvar + annotate('text',x = c(1,2), y = c(-0.65, -0.65), label = 'a')
p.effectvar <- p.effectvar + annotate('text', x =  3, y = -0.65, label = 'b')

## Asynchrony
gls.asyn <- gls(Com.Asynch ~ majority, data = bird.long,
                corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
ci.asyn <- confint(glht(gls.asyn, linfct = mcp(majority = "Tukey"))) # no differences

p.asyn <- ggplot(bird.long, aes(x = majority, y = Com.Asynch, colour = majority)) + geom_boxplot()
p.asyn <- p.asyn + xlab(NULL) + ylab('Community Asynchony')
p.asyn <- p.asyn + scale_color_brewer(palette = "Dark2", direction = -1)
p.asyn <- p.asyn + theme_classic() + theme(legend.position = 'none', 
                                           axis.text = element_text(size = 14),
                                           axis.title.y = element_text(size = 14))
p.asyn <- p.asyn + scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural'))

## Compositional variability
gls.temp <- gls(Temp_Var ~ majority, data = bird.long,
                corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
ci.temp <- confint(glht(gls.temp, linfct = mcp(majority = "Tukey"))) # forest and agri same but both different from urban

p.temp <- ggplot(bird.long, aes(x = majority, y = Temp_Var, colour = majority)) + geom_boxplot()
p.temp <- p.temp + xlab(NULL) + ylab('Compositional Temporal Variability')
p.temp <- p.temp + scale_color_brewer(palette = "Dark2", direction = -1)
p.temp <- p.temp + theme_classic() + theme(legend.position = 'none', 
                                           axis.text = element_text(size = 14),
                                           axis.title.y = element_text(size = 14))
p.temp <- p.temp + scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural'))
p.temp <- p.temp + annotate('text',x = 1, y = 0.58, label = 'a')
p.temp <- p.temp + annotate('text', x = c(2, 3), y = c(0.58, 0.58), label = 'b')

## Functional variability
gls.cov <- gls(log(CoV) ~ majority, data = bird.long,
               corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
ci.cov <- confint(glht(gls.cov, linfct = mcp(majority = "Tukey"))) # agri and urabna re significantly diff from eachother

p.cov <- ggplot(bird.long, aes(x = majority, y = log(CoV), colour = majority)) + geom_boxplot()
p.cov <- p.cov + xlab(NULL) + ylab('Functional Temporal Variability (log)')
p.cov <- p.cov + scale_color_brewer(palette = "Dark2", direction = -1)
p.cov <- p.cov + theme_classic() + theme(legend.position = 'none', 
                                         axis.text = element_text(size = 14),
                                         axis.title.y = element_text(size = 14))
p.cov <- p.cov + scale_x_discrete(labels = c('Artificial', 'Agriculture', 'Forest and \n semi-natural'))
p.cov <- p.cov + annotate('text',x = 1, y = 0.7, label = 'a')
p.cov <- p.cov + annotate('text', x = 2, y = 0.7, label = 'b')
p.cov <- p.cov + annotate('text', x = 3, y = 0.7, label = 'ab')

patch <- p.response + p.effect + p.effectvar + p.asyn + p.temp + p.cov
patch <- patch + plot_annotation(tag_levels = "a", tag_suffix = ')') + plot_layout(ncol = 2)

tiff('D:\\ResponseEffectDiversity\\ResultsAndFigures\\LandcoverFigures\\LandcoverBoxplots.tiff', res = 360,
     height = 10, width = 10, unit = 'in', compression = 'lzw')
#cowplot::plot_grid(p.response, p.effect, p.effectvar, p.asyn, p.temp, p.cov, nrow = 3,
#                   labels = c('a)', 'b)', 'c)', 'd)', 'e)', 'f)'))
patch
dev.off()


#### Interaction figures
cov2 <- gls(CoV ~ response.fdis*majority, data = bird.long,
            corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = -16.53042 (-8.476406)

library(sjPlot)
colpal <- RColorBrewer::brewer.pal(3, "Dark2")[3:1]
p.int <- plot_model(cov2, type = 'int', show.values = TRUE, title = '', colors = colpal) + theme_classic() + theme(legend.title = element_blank()) 
p.int <- p.int + xlab('Response trait FDis') + ylab('Functional Variability') 

tiff('D:\\ResponseEffectDiversity\\ResultsAndFigures\\LandcoverFigures\\CoVInteraction.tiff', res = 360,
     height = 7, width = 7, unit = 'in', compression = 'lzw')
p.int
dev.off()

