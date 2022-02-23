
# Set up 

```{r}
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(ggpmisc)
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
heat_pal <- colorRampPalette(c("white", "lightblue", "darkblue"))
heatmap.2(cor_mat, trace= 'none', col = heat_pal(50000))


################################
# averaging ranks
breed_rank_all$rank_avg <- rowMeans(breed_rank_all[,2:9],na.rm = T)

# adding ranks to breed Traits
breed_traits$rank_avg<- breed_rank_all$rank_avg

# rename columns without spaces
colnames(breed_traits) <- make.names(colnames(breed_traits), unique = T)

# visualize unique for non-numeric variables
coat_type<- unique(breed_traits$Coat.Type)
coat_length<- unique(breed_traits$Coat.Length)

# replace non-numerics with numeric values
breed_traits_num <- within(breed_traits,Coat.Type <- factor(Coat.Type, labels = c(1:10)) )
breed_traits_num <- within(breed_traits_num,Coat.Length <- factor(Coat.Length, labels = c(1:4)) )


#scatterplots

# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA for function

lm_eqn <- function(breed_traits_num){
  m <- lm(rank_avg ~ Affectionate.With.Family, breed_traits_num);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

p1 <- ggplot(breed_traits_num, aes(x=Affectionate.With.Family,y=rank_avg))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE, color="black")+
  geom_text(x = 1, y = 175, label = lm_eqn(breed_traits_num), parse = TRUE)+
  theme_bw()
p1
  

p2 <- ggplot(breed_traits_num, aes(x=Good.With.Young.Children ,y=rank_avg))+
  geom_point()+
  theme_classic()  


