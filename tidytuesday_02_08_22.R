
# Set up 

```{r}
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
```

```{r}
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')
```

# Data wrangling 

```{r}
cnames<-colnames(breed_traits)   # select column names 
cnames_pivot<-cnames[!cnames %in% c('Breed','Coat Length','Coat Type')] # only select some variables
breed_traits_long <- breed_traits %>%  pivot_longer(cnames_pivot, names_to='variable', values_to='value') #pivot longer
head(breed_traits_long); tail(breed_traits_long) # check that it worked
```
```{r}
unique(breed_traits_long$variable)
```

```{r}
colnames(breed_traits_long)[2]<-'Coat.Type'
colnames(breed_traits_long)[3]<-'Coat.Length'
colnames(breed_traits_long)
```


```{r}
df.plot<-breed_traits_long %>% filter(variable == 'Watchdog/Protective Nature') %>% filter(str_detect(Breed, 'hound'))
head(df.plot)
```
```{r}
unique(df.plot$Breed)
```
install.packages('gplots')
install.packages('RColorBrewer')
# Plotting / visualization 

```{r}
set.seed(10)
ggplot(df.plot, aes(x=Coat.Type, y=value, color=Breed)) +
  geom_jitter(width=0.2, height=0.1) + 
  ylab('Protective Nature (mellow to vilagent)') + 
  theme_bw() + ggtitle('First Tidy Tuesday')
```
#######################
# exluding non numeric variables
breed_traits_mat <- breed_traits[,cnames_pivot]
View(breed_traits_mat)

cor_mat <- cor(breed_traits_mat)
View(cor_mat)
heat_pal <- colorRampPalette(c("white", "lightblue", "darkblue"),)
heatmap.2(cor_mat, trace= 'none', col = heat_pal(50000))


