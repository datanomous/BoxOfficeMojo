library(tidyverse)
library(readxl)
library(treemapify)
library(RColorBrewer)

boxoffice <- read_csv("Inputs/BoxOffice/boxoffice.csv")

# check the structure of the dataset
structure(boxoffice)

# how old is the olderst movie in the database
boxoffice[which.min(boxoffice$year),]

# movie with the smallest box office ever (it made $30 in 2006)
boxoffice[which.min(boxoffice$lifetime_gross),]

# most profitable movie for each year
boxoffice %>%
  group_by(year) %>%
  filter(lifetime_gross == max(lifetime_gross)) %>%
  arrange(desc(year))

# most profitable movie each decade
boxoffice %>%
  group_by(decade) %>%
  filter(lifetime_gross == max(lifetime_gross)) %>%
  arrange(desc(decade))

#add ggplot to it with this to get monetary perspective. Get rid of fill to see it without the studio ownership
ggplot(aes(factor(year), lifetime_gross, fill = studio)) +
  geom_histogram(stat = "identity") +
  labs(title = 'Most profitable movie each year by studio', subtitle = 'studios represented in the legend') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks = seq(1920, 2020, 10))

#studios broken down by total revenue overall
boxoffice %>%
  group_by(studio) %>%
  summarise(total_revenue = sum(lifetime_gross)) %>%
  arrange(desc(total_revenue))

#studio with the most revenue each year 
boxoffice %>%
  group_by(year, studio) %>%
  summarise(studio_gross = sum(as.numeric(lifetime_gross))) %>%
  ungroup() %>%
  group_by(year) %>%
  filter(studio_gross == max(studio_gross)) %>%
  arrange(desc(year))

#binning years into decades with a function
YearToDecade = function(year) {
  if(year >= 1920 && year < 1930) return("1920s")
  if(year >= 1930 && year < 1940) return("1930s")
  if(year >= 1940 && year < 1950) return("1940s")
  if(year >= 1950 && year < 1960) return("1950s")
  if(year >= 1960 && year < 1970) return("1960s")
  if(year >= 1970 && year < 1980) return("1970s")
  if(year >= 1980 && year < 1990) return("1980s")
  if(year >= 1990 && year < 2000) return("1990s")
  if(year >= 2000 && year < 2010) return("2000s")
  if(year >= 2010 && year < 2020) return("2010s")
}

boxoffice$decade  = factor(sapply(boxoffice$year, YearToDecade))

#studios with the most revenue each decade
boxoffice %>%
  group_by(decade, studio) %>%
  summarise(studio_gross = sum(as.numeric(lifetime_gross))) %>%
  ungroup() %>%
  group_by(decade) %>%
  filter(studio_gross == max(studio_gross)) %>%
  arrange(desc(decade))

# Graph for that
# Graph 13
boxoffice %>%
  group_by(decade, studio) %>%
  summarise(studio_gross = sum(as.numeric(lifetime_gross))) %>%
  ungroup() %>%
  group_by(decade) %>%
  filter(studio_gross == max(studio_gross)) %>%
  arrange(desc(decade)) %>%
  ggplot(aes(x = decade, y = studio_gross, fill = studio, label = studio)) +
  geom_bar(stat = 'identity') +
  geom_text(label.size = 0.5, size = 5, color = "black", fill = "white", vjust = -0.5) +
  theme(legend.position = "none", axis.title = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  ylab("Box Office") +
  xlab("Decade") +
  labs(title = "Studio with Biggest Box Office Each Decade",
       subtitle = "Based on sum of revenues for each studio")

#total revenue for everybody per year (how much people pay each year to go to movies)
boxoffice %>%
  group_by(year) %>%
  summarise(total_yearly_revenue = sum(as.numeric(lifetime_gross))) %>%
  arrange(desc(year))

#add gplot to show growth througout the years
ggplot(aes(year, total_yearly_revenue, group = 1)) +
  geom_line()

#do movies with shorter names make more money than movies with longer names? (answer: no, lol)
boxoffice %>%
  group_by(nchar(boxoffice$title) <= 14) %>%
  summarise(group_gross = sum(as.numeric(lifetime_gross)))

 OR
 
 boxoffice %>%
   mutate(character_length = nchar(title)) %>%
   mutate(length = ifelse((character_length <= 14), yes = "short", no = "long")) %>%
   group_by(length) %>%
   summarise(group_gross = sum(as.numeric(lifetime_gross)))

#possibly the right version of that: 
boxoffice %>%
  mutate(short = if(nchar(boxoffice$title <= 14), TRUE), long = nchar(boxoffice$title > 14)) %>%
  group_by(short) %>%
  summarise(group_gross = sum(as.numeric(lifetime_gross)))

## find mean and median length of words in titles
mean(sapply(boxoffice$title, function(x)length(unlist(gregexpr(" ",x)))+1))
median(sapply(boxoffice$title, function(x)length(unlist(gregexpr(" ",x)))+1))

# Average Revenue per Movie for Movie Studios who made more than 10 movies 
boxoffice %>%
  group_by(studio) %>%
  summarise(total_revenue = sum(lifetime_gross), number_of_movies = n()) %>%
  mutate(avg_return_on_investment = total_revenue / number_of_movies) %>%
  filter(number_of_movies >= 10) %>%
  arrange(desc(avg_return_on_investment))

# Graph for that
# Graph 15

boxoffice %>%
  group_by(studio) %>%
  summarise(total_revenue = sum(lifetime_gross), number_of_movies = n()) %>%
  mutate(avg_return_on_investment = total_revenue / number_of_movies) %>%
  filter(number_of_movies >= 10) %>%
  arrange(desc(avg_return_on_investment)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(studio, avg_return_on_investment), y = avg_return_on_investment, fill = "tomato2")) +
  geom_bar(stat = 'identity') +
  theme(legend.position = "none", axis.title = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  ylab("Average ROI") +
  xlab("Studio with") +
  labs(title = "Best studios per ROI",
       subtitle = "Top 10 studios shown; n(movies_made) > 10")

# Total Movies made per Studio:
boxoffice %>%
  group_by(studio) %>%
  summarise(movies_made = n()) %>%
  arrange(desc(movies_made))

# Buena Vista Count of Movies Per Year
boxoffice %>%
  filter(studio == "BV") %>%
  arrange(desc(year)) %>%
  ggplot(aes(x = factor(year))) +
  geom_histogram(position = 'identity', stat="count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks = seq(1960, 2020, 5))

# experimenting with bar graphs per year for all studios
boxoffice %>%
  group_by(year, studio) %>%
  summarise(median_revenue = median(lifetime_gross)) %>%
  filter(median_revenue >= 1000000 & median_revenue < 100000000, year >= "2005") %>%
  ggplot(aes(x = factor(year), y = median_revenue, color = median_revenue)) +
  geom_boxplot() +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1, alpha = 0.5) +
  guides(color = FALSE)

# find 10 studios with the most revenue total:
boxoffice %>%
  group_by(studio) %>%
  summarise(total_revenue = sum(lifetime_gross)) %>%
  arrange(desc(total_revenue)) %>%
  top_n(10)

# graph to show studio dominance over the years:

boxoffice %>%
  group_by(year, studio) %>%
  summarise(studio_gross = sum(as.numeric(lifetime_gross))) %>%
  filter(studio_gross > 1000000000) %>%
  ggplot() + geom_histogram(aes(x = factor(year), y = studio_gross, fill = studio), position = "fill", stat = "identity")

boxoffice %>%
  group_by(year, studio) %>%
  summarise(studio_gross = sum(as.numeric(lifetime_gross))) %>%
  top_n(n = 5, wt = studio_gross) %>%
  filter(year > 1977) %>%
  ggplot() + geom_histogram(aes(x = factor(year), y = studio_gross, fill = studio), position = "fill", stat = "identity") +
  scale_fill_brewer(name="", palette = 'Spectral')

# All studios since beginning
# Graph 9
boxoffice %>%
  group_by(year, studio) %>%
  summarise(studio_gross = sum(as.numeric(lifetime_gross))) %>%
  ggplot() + geom_histogram(aes(x = factor(year), y = studio_gross, fill = studio), position = "fill", stat = "identity", show.legend = FALSE) + 
  scale_x_discrete(breaks = seq(1920, 2020, 10)) +
  xlab("Year") +
  ylab("") +
  labs(title = "Historical Studio Density",
       subtitle = "Based on revenues per year; all studios") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


# Top 5 studios each year since 1977 
# Graph 10

spectral2 <- brewer.pal(9, "Spectral")
spectral_range <- colorRampPalette(spectral2)

boxoffice %>%
  group_by(year, studio) %>%
  summarise(studio_gross = sum(as.numeric(lifetime_gross))) %>%
  top_n(n = 5, wt = studio_gross) %>%
  filter(year > 1977) %>%
  ggplot() + geom_histogram(aes(x = factor(year), y = studio_gross, fill = studio), position = "fill", stat = "identity", size = 1.5) +
  scale_fill_manual(values = spectral_range(14)) +
  scale_x_discrete(breaks = seq(1977, 2017, 5)) +
  xlab("Year") +
  ylab("") +
  labs(title = "Studio Density since 1977",
       subtitle = "Based on revenues per year; top 5 studios") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# Top 5 studios since the beginning of MCU
# Graph 11

boxoffice %>%
  group_by(year, studio) %>%
  summarise(studio_gross = sum(as.numeric(lifetime_gross))) %>%
  top_n(n = 5, wt = studio_gross) %>%
  filter(year > 2007) %>%
  ggplot() + geom_histogram(aes(x = factor(year), y = studio_gross, fill = studio), position = "fill", stat = "identity", size = 1.5) +
  scale_fill_manual(values = spectral_range(6)) +
  xlab("Year") +
  ylab("") +
  labs(title = "Studio Density since the beginning of MCU",
       subtitle = "Based on revenues per year; top 5 studios") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#Top5 movies from top 10 studios

top_studio10 <- boxoffice %>%
  group_by(studio) %>%
  summarise(gross = sum(as.numeric(lifetime_gross))) %>%
  top_n(n = 10, wt = gross)

top5fromtop10 <- boxoffice[which(boxoffice$studio %in% top_studio10$studio),] %>%
  group_by(studio) %>%
  arrange(desc(lifetime_gross)) %>%
  slice(1:5)

#top 5 movies from top 5 studios

top_studio5 <- boxoffice %>%
  group_by(studio) %>%
  summarise(gross = sum(as.numeric(lifetime_gross))) %>%
  top_n(n = 10, wt = gross)

top5fromtop5 <- boxoffice[which(boxoffice$studio %in% top_studio5$studio),] %>%
  group_by(studio) %>%
  arrange(desc(lifetime_gross)) %>%
  slice(1:5)


# plot of movie industry growth
# Graph 1

boxoffice %>%
  group_by(year) %>%
  summarise(total_yearly_revenue = sum(as.numeric(lifetime_gross))) %>%
  arrange(desc(year)) %>%
  filter(year < 2018) %>%
  ggplot(aes(year, total_yearly_revenue, group = 1)) +
  geom_area(size = 1, fill = "tomato2", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks = seq(1920, 2020, 5)) +
  annotate("text", x = factor(1977), y = 2e+09, label = "The Star Wars", size = 3, color = "black", angle = 90) +
  annotate("text", x = factor(2009), y = 7e+09, label = "$10 bln.", size = 4, color = "black") +
  annotate("text", x = factor(1993), y = 7e+09, label = "$5 bln.", size = 4, color = "black") +
  annotate("text", x = factor(1977), y = 7e+09, label = "$1 bln.", size = 4, color = "black") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  labs(title = "Rise of US Movie Industry",
       subtitle = "Based on Total Yearly Box Office",
       caption = "Data: BoxOfficeMojo",
       y = "Total Yearly Revenue")

# Movies made by all studios 
# Graph 6

boxoffice %>%
  group_by(studio) %>%
  summarise(movies_made = n()) %>%
  arrange(movies_made) %>%
  ungroup() %>%
  mutate(studio = factor(studio, studio, ordered = T)) %>%
  arrange(desc(movies_made)) %>%
  ggplot(aes(x = studio, y = movies_made, fill = "tomato2", label = as.character(movies_made))) +
  geom_bar(stat = 'identity') +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title = element_text(size = 12), plot.title = element_text(hjust = 0.5)) +
  ylab("Movies Made") +
  labs(title = "Number of Movies Made by Each Studio")

# Movies made by studios with more than 1 movie 
# Graph 7

boxoffice %>%
  group_by(studio) %>%
  summarise(movies_made = n()) %>%
  arrange(movies_made) %>%
  ungroup() %>%
  mutate(studio = factor(studio, studio, ordered = T)) %>%
  arrange(desc(movies_made)) %>%
  filter(movies_made > 1) %>%
  ggplot(aes(x = studio, y = movies_made, fill = "tomato2", label = as.character(movies_made))) +
  geom_bar(stat = 'identity', width = 1.05) +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title = element_text(size = 12), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  ylab("Movies Made") +
  labs(title = "Number of Movies Made by Each Studio",
       subtitle = "Movies made > 1")

# Movies made by top 10 studios
# Graph 8

boxoffice %>%
  group_by(studio) %>%
  summarise(movies_made = n()) %>%
  arrange(movies_made) %>%
  ungroup() %>%
  mutate(studio = factor(studio, studio, ordered = T)) %>%
  top_n(10) %>%
  arrange(desc(movies_made)) %>%
  ggplot(aes(x = studio, y = movies_made, fill = "tomato2", label = as.character(movies_made))) +
  geom_bar(stat = 'identity', width = 0.5) +
  geom_label(label.size = 0.5, size = 5, color = "black", fill = "white") +
  theme(legend.position = "none", axis.title = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  ylab("Movies Made") +
  xlab("Studio") +
  labs(title = "Number of Movies Made by Top 10 studios")

# Most Popular Movie Each Decade

boxoffice %>%
  group_by(decade) %>%
  filter(lifetime_gross == max(lifetime_gross)) %>%
  arrange(desc(decade)) %>%
  ggplot(aes(x = title, y = lifetime_gross, fill = title, label = as.character(decade))) +
  geom_bar(stat = 'identity') +
  geom_label(label.size = 0.5, size = 4, color = "black") +
  coord_flip() +
  theme(legend.position = "none")

boxoffice %>%
  group_by(decade) %>%
  filter(lifetime_gross == max(lifetime_gross)) %>%
  arrange(desc(decade)) %>%
  ggplot(aes(x = reorder(title, -lifetime_gross), y = lifetime_gross, fill = "tomato3", label = as.character(decade))) +
  geom_bar(stat = 'identity', width = 0.3) +
  geom_label(label.size = 0.5, size = 3.5, color = "black") +
  coord_flip() +
  theme(legend.position = "none")

# Movies with short names vs long names primitive examples
boxoffice %>%
  mutate(character_length = nchar(title)) %>%
  mutate(length = ifelse((character_length <= 14), yes = "short", no = "long")) %>%
  group_by(length) %>%
  filter(lifetime_gross > 100000000) %>%
  ggplot(aes(x= length, y = lifetime_gross, color = length)) +
  geom_jitter(alpha = 0.6) +
  geom_boxplot()

# Another version with different gross
# Graph 3

boxoffice %>%
  mutate(character_length = nchar(title)) %>%
  mutate(length = ifelse((character_length <= 14), yes = "short", no = "long")) %>%
  group_by(length) %>%
  filter(lifetime_gross < 10000000) %>%
  ggplot(aes(x = length, y = lifetime_gross, col = length)) +
  geom_jitter(alpha = 0.4) +
  geom_boxplot(color = "black") +
  ylab("Box Office") +
  xlab("Title Length") +
  labs(title = "Distribution of Box Office Revenue based on Movie Title Length",
       subtitle = "With Movie Gross < $10 mln") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        legend.position = "none")

# more than 10 mln
# Graph 2

boxoffice %>%
  mutate(character_length = nchar(title)) %>%
  mutate(length = ifelse((character_length <= 14), yes = "short", no = "long")) %>%
  group_by(length) %>%
  filter(lifetime_gross > 10000000) %>%
  ggplot(aes(x = length, y = lifetime_gross, col = length)) +
  geom_jitter(alpha = 0.4) +
  geom_boxplot(color = "black") +
  ylab("Box Office") +
  xlab("Title Length") +
  labs(title = "Distribution of Box Office Revenue based on Movie Title Length",
       subtitle = "With Movie Gross > $10 mln") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        legend.position = "none")

# Another version with density graphs
# Graph 4

boxoffice %>%
  mutate(character_length = nchar(title)) %>%
  mutate(length = ifelse((character_length <= 14), yes = "short", no = "long")) %>%
  group_by(length) %>%
  filter(lifetime_gross > 100000000) %>%
  ggplot(aes(x = lifetime_gross, fill = length, color = length)) +
  geom_density(alpha = 0.1) +
  ylab("Density") +
  xlab("Box Office") +
  labs(title = "Distribution of Box Office Revenue based on Movie Title Length",
       subtitle = "With Movie Gross > $10 mln") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        legend.position = "none", axis.ticks.y = element_blank(), axis.text.y = element_blank())

# Below 10 Mln
# Graph 5

boxoffice %>%
  mutate(character_length = nchar(title)) %>%
  mutate(length = ifelse((character_length <= 14), yes = "short", no = "long")) %>%
  group_by(length) %>%
  filter(lifetime_gross < 100000000) %>%
  ggplot(aes(x = lifetime_gross, fill = length, color = length)) +
  geom_density(alpha = 0.1) +
  ylab("Density") +
  xlab("Box Office") +
  labs(title = "Distribution of Box Office Revenue based on Movie Title Length",
       subtitle = "With Movie Gross < $10 mln") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        legend.position = "none", axis.ticks.y = element_blank(), axis.text.y = element_blank())

# Top 10 Studios with their top 5 films

ggplot(top5fromtop10, aes(area = lifetime_gross, fill = studio, label = title, subgroup = studio)) +
  geom_treemap() + 
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "bottom", grow = T, alpha = 0.25, color = "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(color = "white", place = "center", reflow = T) +
  theme(legend.position = "none")

# Top 5 studios with their top 5 films
# Graph 12

ggplot(top5fromtop5, aes(area = lifetime_gross, fill = studio, label = title, subgroup = studio)) +
  geom_treemap() +
  geom_treemap_subgroup_text(place = "bottom", grow = T, alpha = 0.25, color = "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(color = "white", place = "center", reflow = T, fontface = "italic") +
  theme(legend.position = "none") +
  labs(title = "Top 5 studios with their 5 top movies",
       subtitle = "Based on all-time revenue") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


# Buena Vista Deviation and 
# Graph 17

ggplot(boxoffice2selected, aes(x = title, y = gross_z, label = gross_z)) +
  geom_point(stat = "identity", fill = "black", size = 7) +
  geom_segment(aes(y = 0,
                   x = title,
                   yend = gross_z,
                   xend = title),
               color = "black") +
  geom_text(color = "white", size = 3) +
  labs(title = "Buena Vista's Return per Movie vs. Industry Average") +
  ylim(-1, 21) +
  coord_flip() +
  xlab("") +
  ylab("Deviation from Industry's Mean") +
  theme_bw()

boxoffice2$gross_z <- round((boxoffice2$lifetime_gross - mean(boxoffice2$lifetime_gross))/sd(boxoffice2$lifetime_gross), 2)
boxoffice2$gross_type <- ifelse(boxoffice2$gross_z < 0, "below", "above")
boxoffice2 <- boxoffice2[order(boxoffice2$gross_z),]
boxoffice2 <- filter(boxoffice2, studio == "BV")
boxoffice2$title <- factor(boxoffice2$title, levels = boxoffice2$title)
boxoffice2selected <-
  boxoffice2[c(1,25,50,75,100,150,200,250,300,350,400,450,500,550,570)]

ggplot(boxoffice2selected, aes(x = title, y = gross_z, label = gross_z)) +
  geom_bar(stat = "identity", aes(fill = gross_type), width = 0.5) +
  scale_fill_manual(name = "Average Return",
                          labels = c("Above Average", "Below Average"),
                          values = c("above" = "#00ba38", "below" = "#f8766d")) +
  coord_flip()

# Dominant Studios History 
# Graph 14

boxoffice %>%
  group_by(year, studio) %>%
  summarise(studio_gross = sum(as.numeric(lifetime_gross))) %>%
  ungroup() %>%
  filter(studio %in% c("BV", "Fox", "Par.", "Sony", "Uni.", "WB")) %>%
  filter(year > 1950) %>%
  ggplot(aes(x = as.numeric(year), y = studio_gross)) +
  geom_jitter(aes(col = studio), alpha = 0.3) + 
  geom_smooth(aes(col = studio), se = FALSE, size = 1.2) +
  ylab("Box Office") +
  xlab("year") +
  labs(title = "Major Studios Revenue Trend") +
  theme(plot.title = element_text(hjust = 0.5))

# Buena Vista Movies and Revenues per Year 

boxoffice %>%
  group_by(studio, year) %>%
  summarise(movies_made = n(), lifetime_gross = sum(as.numeric(lifetime_gross))) %>%
  arrange(desc(year), desc(movies_made)) %>%
  filter(studio == "BV")

# Buena Vista Graph
# Graph 16

BV1 <- boxoffice_BV %>%
  ggplot() +
  geom_bar(aes(x = year, y = movies_made), stat = 'identity', fill = "tomato2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.minor.x = element_line(),
        plot.title = element_text(hjust = 0.5)) +
  ylab("Number of Movies") +
  labs(title = "Buena Vista's Output vs Income")

BV2 <- boxoffice_BV %>%
  ggplot() +
  geom_area(aes(x = year, y = lifetime_gross), group = 1, color = "black", fill = "tomato2", alpha = 0.5) +
  scale_x_discrete(breaks = seq(1960, 2020, 5)) +
  ylab("Box Office") +
  theme(panel.grid.minor.x = element_line())

grid.draw(rbind(ggplotGrob(BV1), ggplotGrob(BV2), size = "last"))