
#Let's begin by installing and loading the necessary packages.

# if(!require('pacman')) install.packages('pacman')
# pacman::p_load(tidyverse, ggpol, gganimate, gifski, kableExtra)


library(tidyverse)
library(ggpol)
library(gganimate)
library(gifski)

#Data Input and Transformation

IEpop <- read.csv("~/armsimportExportcsvS.csv")
IEPop <- IEpop %>% 
  na.omit %>% 
  mutate(ImportorExport = as.factor(ifelse(str_detect(IOE, 'Export$') == TRUE, 'Export', 'Import')),
         ImportorExport = factor(ImportorExport, levels = c('Export', 'Import'))) %>% 
  select(-Total,-IOE) %>% 
  gather(Year, Sales, X2000:X2020) %>%
  mutate(Year = as.integer(substr(Year, 2, 5)),
    Sales = ifelse(ImportorExport == 'Import', as.integer(Sales * -1), as.integer(Sales)))

#Static Plot

IEPop <- IEPop %>%
  ggplot(aes(
    x = Country,
    y = Sales,
    fill = ImportorExport,
  )
  ) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('#f8874f', '#13840f')) + 
  coord_flip() + 
  scale_y_continuous(
    breaks = c(-1000,-2000,-4000,-6000,-8000,-10000, 0,1000,2000,4000,6000,8000,10000),
    label = c("1B","2B","4B","6B","8B","10B","0","1B","2B","4B","6B","8B","10B")
  ) +
  #Theme setting of the visualization
  theme(
    legend.position = c(0.8,0.5),
    plot.background = element_rect(fill = "#eeebea"),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    axis.text = element_text(size = 14,face = 'bold'),
    legend.key.size = unit(0.75, 'cm'),
    legend.background = element_rect(fill = "#eeebea"),
    legend.text = element_text(
      size = 10,
      hjust = 0,
      face = 'bold'
    ),
    plot.title = element_text(
      size =18,
      hjust = 0,
      face = 'bold'
    ),
    plot.subtitle = element_text(
      size = 16,
      hjust = 0.3,
      face = 'bold'
    ),
    axis.title.x = element_text(
      hjust = 0.3,
      size = 14,
      face = 'bold'
    ),
    plot.caption = element_text(
      size = 8,
      hjust = 1,
      face = 'italic',
      color = 'gray'
    )
  ) 
 IEPop <- IEPop +
#Labels and Axis text
  labs(
    title = 'International Flow of Major Conventional Arms\n',
    subtitle = '{closest_state}',
    y = '\n\nUS dollars (in Billions)',
    caption = '@Ayybeeshafi | Data Source: https://sipri.org/databases/armstransfers'
  )

 IEPop <- IEPop +

#Transition of Data in the subsequent frames  

  transition_states(
    Year,
    transition_length = 1,
    state_length = 2,
    wrap = FALSE
   ) + 
  enter_fade() +
  exit_fade() + 
  ease_aes('cubic-in-out')
 
#Creations of Gif

animate(
  IEPop,
  fps = 25,
  duration = 20,
  width = 1000,
  height = 900,
  res = 120,
  end_pause = 80,
  
  renderer = gifski_renderer('ArmsImportExport.gif')
)
