install.packages("spotifyr")
library(spotifyr)
library(ggplot2)
library(dplyr)
spotify_client_id <- "your_spotify_client_id"
spotify_client_secret <- "your_spotify_client_secret"
spotify_redirect_uri <- "your_spotify_redirect_uri"
auth_response <- spotify_auth(spotify_client_id, spotify_client_secret, spotify_redirect_uri)
artist_name <- "Joy Division"
artist_audio_features <- get_artist_audio_features(artist_name, auth_response)
ggplot(artist_audio_features, aes(x = valence, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  labs(title = "Joyplot of Joy Division's audio features", subtitle = "Based on valence pulled from Spotify's Web API with spotifyr")
user_id <- "your_spotify_user_id"
user_tracks <- get_user_tracks(user_id, auth_response)
user_playlists <- get_user_playlists(user_id, auth_response)
artist_audio_features <- get_artist_audio_features("Joy Division", auth_response)

average_audio_features <- summarize_audio_features(artist_audio_features)
median_audio_features <- summarize_audio_features(artist_audio_features, type = "median")
user_playlists <- get_user_playlists("your_spotify_user_id", auth_response)

playlist_tracks <- count_tracks_in_playlists(user_playlists)
playlist_duration <- sum_track_duration_in_playlists(user_playlists)

ggplot(playlist_tracks, aes(x = n, y = playlist_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Playlists by Number of Tracks", subtitle = "Based on Spotify's Web API with spotifyr")

ggplot(playlist_duration, aes(x = total_duration_ms, y = playlist_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Playlists by Total Duration (in milliseconds)", subtitle = "Based on Spotify's Web API with spotifyr")
user_top_tracks <- get_user_top_tracks("your_spotify_user_id", auth_response)

total_listening_time <- sum(user_top_tracks$track_duration_ms)
unique_tracks <- nrow(user_top_tracks)

print("Total listening time:", total_listening_time / 1000, "hours")
print("Number of unique tracks:", unique_tracks)

