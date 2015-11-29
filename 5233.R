## dplyr for data parsing

# install and load library
install.packages("dplyr")
library(dplyr)

# download a file
download.file("http://files.figshare.com/2236372/combined.csv",  "data/portal_data_joined.csv")


#import file
surveys <- read.csv('data/portal_data_joined.csv')

# select columns
select (surveys, plot_id, species_id, weight)

# select rows
filter(surveys, year == 1995)

# select only species_id, sex, weight
select(surveys, species_id, sex, weight)


# filter by weight less than 5
filter(surveys, weight < 5)


# link commands together
surveys_5ml <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

# select plot_id, species_id, and weight for only 1995
surveys %>%
  filter(year == 1995) %>%
  select(plot_id, species_id, weight)

# mutate
head(surveys)
surveys %>%
  mutate(weight_kg = weight / 1000) %>% # convert to kg and add in column
  filter(!is.na(weight)) %>%
  head

# split/apply/combine
surveys %>%
  group_by(sex) %>%
  tally

# add extra column based on groupings
surveys %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# group based on multiple columns
surveys %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))
# need filter to remove NAs from sex (multiple ways to do this)

# summarize multiple variables at once (complicated but complete scenario)
surveys %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm=TRUE), min_weight = min(weight, na.rm=TRUE)) %>%
  filter(!is.na(mean_weight))

# combine with ggplot2
install.packages("ggplot2")
library(ggplot2)

# example scatterplot
ggplot(data = surveys, aes(x=weight, y=hindfoot_length)) + geom_point()

#example boxplot
ggplot(data = surveys, aes(x=species_id, y=weight)) + geom_boxplot()

# time series
yearly_counts <-surveys %>%
  group_by(year, species_id) %>%
  tally
ggplot(data=yearly_counts, aes(x=year, y=n, group=species_id)) + geom_line()

# download a file
download.file("http://files.figshare.com/2236372/combined.csv",  "data/portal_data_joined.csv")

# import a file
surveys <- read.csv('data/portal_data_joined.csv')

# filter surveys and select column
speciesDM<-surveys %>%
  fliter(species_id == "DM") %>%
  select(weight) %>%
  filter(!is.na(weight))

# filter surveys and select column
speciesDM <- surveys %>%
  filter(species_id == "DM") %>%
  select(hindfoot_length) %>%
  filter(!is.na(hindfoot_length))

# build figure
pdf("figures/histogram.pdf")
qplot(hindfoot_length, data = speciesDM, ylab = "Frequency", xlab = "hindfoot_length(mm)", binwidth = 1, geom = "histogram")


## mean weight per species
# filter surveys abd select columns
allspecies <- surveys %>%
  select(weight, species) %>%
  filter(!is.na(weight)) %>%
  filter(!is.na(species))

# creating a group
allspecies %>%
  group_by(species)

# creating dataset with average weight per species
avg <- aggregate(allspecies$weight, list(species = allspecies$species), mean)

# building bar graph
pdf("figures/bargraph.pdf")

ggplot(data=avg, aes(x=species, y=x)) + xlab("Species") + ylab("Mean Weight (g)") + geom_bar(stat="identity") +coord_flip()

## weight albigula vs weight merriami

#fliter and select columns
eremicus <- surveys %>%
  filter(species == "eremicus") %>%
  select(species, weight) %>%
  filter(!is.na(weight))

#filter and select for 2002
merriami <- surveys %>%
  filter(species == "merriami") %>%
  select(species, weight) %>%
  filter(!is.na(weight))

# create combined object
combine <- rbind(eremicus, merriami)

# create box plot

pdf("figures/boxplot.pdf")
ggplot(data = combine, aes(x=species, y=weight, fill=species)) + xlab("Species") + ylab("Weight (g)") + geom_boxplot() + scale_fill_discrete(name="p-value 2.2e-16")

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
