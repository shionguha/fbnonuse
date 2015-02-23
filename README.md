foursquare_oauth2_php
=====================

This is a code repository for connecting to the foursquare API (https://developer.foursquare.com) via Oauth2 and PHP. This is meant for academic uses only. I make no pretense about code quality but you are welcome to use it if you feel it might be useful.

I am a graduate student of information science just learning to produce some rudimentary scripts. This codebase is meant to be a record of my research activities here.

**File Descriptions and usage:**

index.php - main file where user authorizes your app. Inspiration from [Colin Wren's work](http://colinwren.com/foursquare-tutorial-part-1/). Add your own client id, secret id and redirect uri before using it.

bootstrap.css - Nice minimalistic frontend UI solution. Inspiration from [twitter bootstrap](http://twitter.github.io/bootstrap/) 

4sq_Callback.php - callback page after user authorizes your app. Shows them some basic statistics about their foursquare usage and downloads their access token and basic JSON response to server.  Add your own client id, secret id and redirect uri before using it.
