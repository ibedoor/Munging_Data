# import library
library(tidyverse) 
library(tidyr)
library(ggplot2)

# import dataset
link <- 'https://media.githubusercontent.com/media/metmuseum/openaccess/master/MetObjects.csv'
metropolitan <- read.csv(link)
View(metropolitan)

# list rows of data that have missing values
metropolitan[!complete.cases(metropolitan),]

# which one is NA?
which (is.na(metropolitan))

# identify count of NA values
sum(is.na(metropolitan))

# total missing values in each column
colSums(is.na(metropolitan))

# data frame
df <- data.frame(metropolitan)

# removes all incomplete cases
# df <- na.omit(df)
# delete columns have a lot of missing values
df <- df[, -c(11:16)]
df <- df[, -c(36:40)]
df <- df[, -c(30:31)]


# df$Gallery.Number[is.na(df$Gallery.Number)] <- 0
# df$Artist.Alpha.Sort[is.na(df$Artist.Alpha.Sort)] <- "None"

# df$Gallery.Number <- sub("", "0", df$Gallery.Number)
# df$Artist.Role <- sub("^$", "None", df$Artist.Role)

# fill missing values
df$Gallery.Number[df$Gallery.Number==""] <- 0
df$Artist.Alpha.Sort[df$Artist.Alpha.Sort==""] <- "Unknown"
df$Artist.Role[df$Artist.Role==""] <- "Unknown"
df$Artist.Prefix[df$Artist.Prefix==""] <- "Unknown"
df$Artist.Suffix[df$Artist.Suffix==""] <- "Unknown"
df$Artist.Display.Name[df$Artist.Display.Name==""] <- "Unknown"
df$Artist.Display.Bio[df$Artist.Display.Bio==""] <- "Unknown"
df$Artist.Nationality[df$Artist.Nationality==""] <- "Unknown"
df$Tags[df$Tags==""] <- "Unknown"
df$Artist.Gender[df$Artist.Gender==""] <- "Unspecified"
df$Artist.Begin.Date[df$Artist.Begin.Date==""] <- "Unknown"
df$Artist.End.Date[df$Artist.End.Date==""] <- "Unknown"
df$Artist.ULAN.URL[df$Artist.ULAN.URL==""] <- "Unknown"
df$Artist.Wikidata.URL[df$Artist.Wikidata.URL==""] <- "Unknown"
df$Object.Wikidata.URL[df$Object.Wikidata.URL==""] <- "Unknown"
df$Geography.Type[df$Geography.Type==""] <- "Made in"
df$County[df$County==""] <- "Unknown"
df$Country[df$Country==""] <- "Unknown"
df$Region[df$Region==""] <- "Unknown"
df$Subregion[df$Subregion==""] <- "Unknown"
df$Tags.AAT.URL[df$Tags.AAT.URL==""] <- "Unknown"
df$Tags.Wikidata.URL[df$Tags.Wikidata.URL==""] <- "Unknown"

View(df)

# filter years after 2000
Dates <- df %>% 
  filter(AccessionYear > 2000)

# visualize based on Accession years
ggplot(data = Dates, mapping = aes(x = AccessionYear, fill=AccessionYear)) +
  geom_histogram(stat = "count")

# visualize department type
z <- df %>% count(Department)
ggplot(data=z, aes(x=n, y="", fill=Department, col = Department))+
  geom_bar(stat = "identity", width=1)+
  ggtitle("Department Type")+
  coord_polar("x", start = 0)

#
obj_name <- df %>% 
  count(Object.Name)
ggplot(data = obj_name, mapping = aes(x = Object.Name, fill=Object.Name)) +
  geom_bar()
