library(tidyverse)
library(dplyr)
library(plotly)

songs <- read.csv("spotify-2023.csv")
head(songs)

#arrange songs by number of streams and add ranking column
rankedSongs <- songs %>% mutate(streams=as.numeric(streams)) %>% arrange(desc(streams)) %>% mutate(ranking=row_number()) %>% relocate(25, .before = 1)

#filter entries with N/A streams
rankedSongs <- rankedSongs %>% filter(!is.na(streams), released_year>=2010)

#rename column for easier access
rankedSongs <- rankedSongs %>% rename("artist_name"="artist.s._name")

#clean special characters from artist names and track names
rankedSongs <- rankedSongs %>% mutate(artist_name=str_replace_all(artist_name,"[^[:alnum:]], ", "?")) %>% mutate(artist_name=gsub("�","?",artist_name))
rankedSongs <- rankedSongs %>% mutate(track_name=str_replace_all(track_name,"[^[:alnum:]], ", "?")) %>% mutate(track_name=gsub("�","?",track_name))

rankedSongs


#Create new dataframe, count number of occurrences for each month/year combination
release_dates <- data.frame()
release_dates <- release_dates %>% add_column(year=NA, month=NA, num=NA)
for (i in 2010:2023) {
  for (k in 1:12) {
    release_dates
    release_dates <- release_dates %>% add_row(
      year=i,
      month=k,
      num=length(which(rankedSongs$released_year==i & rankedSongs$released_month == k)))
  }
}
    
#add text for graph tooltips
release_dates <- release_dates %>%
  mutate(text = paste0("x: ", month, "\n", "y: ", year, "\n", "Value: ", num))
    
#graph heatmap
date_heatmap <- release_dates %>% ggplot(aes(x=month,y=year,fill=num, text=text)) +
  geom_tile() +
  labs(x="Month", y="Year", fill = "# of Songs")
    
#create interactive graph
ggplotly(date_heatmap,tooltip="text") %>% 
  layout(title=list(text=paste0("Most Common Song Release Dates",'<br>','<sup>',"By Month and Year (2010-2023)", '</sup>')), margin = list(t = 60))

#count occurrences of each artist
artist_occurrences <- rankedSongs %>% group_by(artist_name) %>% summarise(num=n()) %>% arrange(desc(num))
artist_occurrences <- artist_occurrences %>% filter(num>=3) %>% mutate(id=row_number()) %>% relocate(3, .before = 1)
artist_occurrences

#Prepare graph information
graph_data <- artist_occurrences %>% filter(row_number(id) <= 10)
graph_data$discrete <- cut(graph_data$num, breaks = 10)

#Create bargraph of artists
artist_graph <- graph_data %>% ggplot(aes(y=reorder(artist_name,+num), x=num, fill=discrete)) +
  geom_bar(stat="identity") +
  scale_fill_viridis_d(guide="none", direction = 1) +
  geom_text(aes(label=num), hjust=-0.2) +
  labs(title="Top Ten Artists On Spotify 2023", subtitle="By Number of Popular Songs", x="Number Of Hit Songs",y="Artist")

artist_graph

hit_songs_tswift <- rankedSongs %>% filter(artist_name == "Taylor Swift")
hit_songs_no_tswift <- rankedSongs %>% filter(artist_name != "Taylor Swift")

head(hit_songs_tswift)
head(hit_songs_no_tswift)

taylor_dance_graph <- ggplot() +
  
  # Non-Taylor data
  geom_point(data=hit_songs_no_tswift, aes(x=ranking, y=(danceability_.), color='All Artists')) +
  geom_hline(data=hit_songs_no_tswift, aes(yintercept=mean(danceability_.)), color="blue") +
  annotate("text", x=32,y=70,label="Overall Avg.", color="blue") +
  
  # Taylor Swift data
  geom_point(data=hit_songs_tswift, aes(x=ranking, y=(danceability_.),color='Taylor Swift')) +
  geom_hline(data=hit_songs_tswift, aes(yintercept=mean(danceability_.)), color="red") +
  annotate("text", x=32,y=57,label="T. Swift Avg.", color="red") +
  
  scale_x_reverse()+
  labs(title="Danceability by Spotify Streams", subtitle="Taylor Swift vs. Overall", x="Ranking (by # of Streams)", y="Danceability %") +
  scale_color_manual(name="Artist",
                     breaks=c('Taylor Swift', 'All Artists'),
                     values=c('Taylor Swift'='red', 'All Artists'='lightblue'))

taylor_dance_graph

taylor_energy_graph <- ggplot() +
  # Non-Taylor data
  geom_point(data=hit_songs_no_tswift, aes(x=ranking, y=(energy_.), color='All Artists')) +
  geom_hline(data=hit_songs_no_tswift, aes(yintercept=mean(energy_.)), color="blue") +
  annotate("text", x=32,y=68,label="Overall Avg.", color="blue") +
  
  # Taylor Swift data
  geom_point(data=hit_songs_tswift, aes(x=ranking, y=(energy_.), color='Taylor Swift')) +
  geom_hline(data=hit_songs_tswift, aes(yintercept=mean(energy_.)), color="red") +
  annotate("text", x=32,y=54,label="T. Swift Avg.", color="red") +
  
  scale_x_reverse() +
  labs(title="Song Energy by Spotify Streams", subtitle="Taylor Swift vs. Overall", x="Ranking (by # of Streams)", y="Energy %") +
  scale_color_manual(name="Artist",
                     breaks=c('Taylor Swift', 'All Artists'),
                     values=c('Taylor Swift'='red', 'All Artists'='lightblue'))

taylor_energy_graph

taylor_speech_graph <- ggplot() +
  # Non-Taylor data
  geom_point(data=hit_songs_no_tswift, aes(x=ranking, y=speechiness_., color='All Artists')) +
  geom_hline(data=hit_songs_no_tswift, aes(yintercept=mean(speechiness_.)), color="blue") +
  annotate("text", x=32,y=13,label="Overall Avg.", color="blue") +
  
  # Taylor Swift data
  geom_point(data=hit_songs_tswift, aes(x=ranking, y=speechiness_., color='Taylor Swift')) +
  geom_hline(data=hit_songs_tswift, aes(yintercept=mean(speechiness_.)), color="red") +
  annotate("text", x=32,y=5,label="T. Swift Avg.", color="red") +
  
  scale_x_reverse() +
  labs(title="Speechiness by Spotify Streams", subtitle="Taylor Swift vs. Overall", x="Ranking (by # of Streams)", y="Speechiness %") +
  scale_color_manual(name="Artist",
                     breaks=c('Taylor Swift', 'All Artists'),
                     values=c('Taylor Swift'='red', 'All Artists'='lightblue'))

taylor_speech_graph

taylor_bpm_graph <- ggplot() +
  # Non-Taylor data
  geom_point(data=hit_songs_no_tswift, aes(x=ranking, y=bpm, color='All Artists')) +
  geom_hline(data=hit_songs_no_tswift, aes(yintercept=mean(bpm)), color="blue") +
  annotate("text", x=32,y=118,label="Overall Avg.", color="blue") +
  
  # Taylor Swift data
  geom_point(data=hit_songs_tswift, aes(x=ranking, y=bpm, color='Taylor Swift')) +
  geom_hline(data=hit_songs_tswift, aes(yintercept=mean(bpm)), color="red") +
  annotate("text", x=32,y=131,label="T. Swift Avg.", color="red") +
  
  scale_x_reverse() +
  labs(title="BPM by Spotify Streams", subtitle="Taylor Swift vs. Overall", x="Ranking (by # of Streams)", y="BPM") +
  scale_color_manual(name="Artist",
                     breaks=c('Taylor Swift', 'All Artists'),
                     values=c('Taylor Swift'='red', 'All Artists'='lightblue'))

taylor_bpm_graph

#create a new dataframe, calculate total streams and profit, and assign them new columns
artist_profit <- rankedSongs %>% 
  select(artist_name, streams) %>%
  group_by(artist_name) %>%
  summarise(totalStreams = sum(streams), spotify_profit = (round(totalStreams*0.004)))

#merge the profit dataframe with occurrence dataframe
artist_profit <- merge(artist_profit, artist_occurrences, by = "artist_name")  %>% rename("num_songs"="num") %>% arrange(desc(num_songs))

artist_profit

#prepare tooltip text 
artist_profit <- artist_profit %>% mutate(text = paste("Artist: ", artist_name, "\nSpotify Streams: ", totalStreams, "\nTotal Profit ($USD): ", spotify_profit, "\nNumber of Hit Songs: ", num_songs, sep=""))

#graph streams and profit, with size = number of songs
graph_profit <- artist_profit %>% ggplot(aes(x=num_songs, y=spotify_profit, size=totalStreams, color=artist_name, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(0.5, 10), name="Number of Hit Songs") +
  scale_color_viridis_d(guide="none") +
  labs(x="Number of Songs", y="Total Profit") +
  theme(legend.position='none') +
  annotate("text", x=30.5,y=105,label="Size = Total Spotify Streams", size=3)

#make interactive graph
ggplotly(graph_profit, tooltip="text") %>%
  layout(title=list(text=paste0("Number of Hit Songs on Spotify vs. Profit",'<br>','<sup>',"By Artist (2010-2023)", '</sup>')), margin = list(t = 60))
