## dplyr for data parsing

# install and load library
install.packages("dplyr")
library(dplyr)

# download a file
download.file("http://files.figshare.com/2236372/combined.csv",  "data/portal_data_joined.csv")


#import file
surveys <- read.csv('data/portal_data_joined.csv')


# combine with ggplot2
install.packages("ggplot2")
library(ggplot2)

##### R PROJECT ASSIGNMENT

# filter surveys and select column
speciesPP <- surveys %>%
  filter(species_id =="PP") %>%
  select(hindfoot_length) %>%
  filter(!is.na(hindfoot_length))

# build figure
pdf("figures/histogram.pdf")

qplot(hindfoot_length, data=speciesPP, ylab= "Frequency", xlab = "hindfoot_length(mm)", binwidth = 1, geom = "histogram")

## mean hindfoot length per species

# filter surveys and select columns
allspecies <- surveys %>%
  select(hindfoot_length, species) %>%
  filter(!is.na(hindfoot_length)) %>%
  filter(!is.na(species))
  
# creating a group
 allspecies %>%
   group_by(species)

# creating dataset with average hindfoot length per species
 avg <- aggregate(allspecies$hindfoot_length, list(species = allspecies$species), mean)
 
# building bar graph
 pdf("figures/bargraph.pdf")
 
ggplot(data=avg, aes(x=species, y=x)) +xlab("species") + ylab("Mean hindfoot_length (g)") + geom_bar(stat="identity") +coord_flip()
   
   
# weight maniculatus vs weight ordii
# filter and select columns
maniculatus <- surveys %>%
  filter(species == "maniculatus") %>%
  select(species, weight) %>%
  filter(!is.na(weight))

# filter and select for 2002
ordii <- surveys %>%
  filter(species == "ordii") %>%
  select(species, weight) %>%
  filter(!is.na(weight))
    
# create combined object
combine <- rbind(maniculatus, ordii)

#create box plot
pdf("figures/boxplot.pdf")
ggplot(data = combine, aes(x=species, y=weight, fill=species)) + xlab("Species") + ylab("Weight(g)") + geom_boxplot() + scale_fill_discrete(name="p-value 2.2e-16")

# statistical test

t.test(weight~species, data=combine)

# create geom point
ggplot(data = surveys, aes(x=weight, y=hindfoot_length)) + geom_point()