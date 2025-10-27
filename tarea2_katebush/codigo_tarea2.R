
library(rgenius)
library(foreach)

#crear objeto con token
Sys.setenv(SPOTIFY_CLIENT_ID = 'sjy_bMml6MTOshZ-dGKAhAG4Z-h0chxygZyRFFRpt0k-QINLR7ZZBEo_HkZEaGt_')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'KnKC03OTSLp3wYkGOftzfxG53I0b51inJ7pqT0rHSduPRS51bLvE598qrc6jVEmOdVeSSRNPBeWwl6wVmMYsQA')
Sys.setenv(GENIUS_API_TOKEN="pWCP72bA2ECGu5Wtkh_s0VWrBIqKmVmUTJqHcBB5ZT73qjr5kurPYzF7aGSVNxxh")
access_token

search_genius_artist(artist_name = "Kate Bush") #39200 es el artist__id de Kate Bush
search_genius_song(song_name="Suspended in Gaffa")
get_song_media(song_id="426984")


get_genius_artist_songs(artist_id="39200")

#Quiero sacar toda la letra de discografia de Kate bush
letra <- get_discography_lyrics(artist_id = 39200, cores=detectCores())


