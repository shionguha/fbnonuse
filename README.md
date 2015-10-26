This is a code repository for connecting to the foursquare API (https://developers.facebook.com/) via Oauth2 and Javascript. This is meant for academic uses only. I make no pretense about code quality but you are welcome to use it if you feel it might be useful.

This is part of an academic project at Cornell University (http://idl.cornell.edu/projects/facebook-non-use/)


Description of files:

index.html - FB Login button which enables access to facebook for any participant in our experiments.

counts.py - captures counts of literally everything that can be grabbed from the fb api. e.g. events, likes, comments, updates, posts, pictures etc.

fbgrab.py - init file to setup api access.

friendinteractions.py - calculate the interaction of 2 friends in different events captured in counts.py

initviz.py - setup the initial visualization of the facebook friend network

newviz.py - actually create the visualization of the friend network - takes a lot of time.

ltget.py - get the long term access token of a facebook user.
