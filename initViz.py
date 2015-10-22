import facebook
import facepy
import requests
import urllib2
import json
from apscheduler.scheduler import Scheduler
import pymongo
from pymongo import MongoClient
import time
#fixing the Insecure platform issue
import urllib3.contrib.pyopenssl
import dateutil.parser as dateparser
import gridfs
from facepy.exceptions import OAuthError
from facepy.exceptions import FacebookError
from itertools import combinations
import itertools


def main():
#Get user from Database
  client = MongoClient('localhost', 27017)
  db2 = client['fbapp-DB']
  collection2 = db2['fb-users']
  collection3 = db2['fb-interactions']
  admin_token = ""
  user = collection2.find_one({"name":"NehaDeshmukh"})
  
  for user in collection2.find():
    if 'vizDone' in user:
      if user['vizDone'] == 0:   
       name = user['name']
       Id = user['user id']
       access_token = user['access_token']
       graph = facepy.GraphAPI(access_token)
       admin = facepy.GraphAPI(admin_token)
       nodes = []
       friends = []
       links =[]
       # get profile object
       user_name = graph.get('me')['name']
       fb_feed = "https://graph.facebook.com/v2.4/me?fields=feed&access_token="+access_token
       data = requests.get(fb_feed).json()
       first_interaction = data['feed']['data'][0]
       story1 = first_interaction['story']
       story2 = "" 
       date1 = dateparser.parse(first_interaction['created_time']).strftime('%m/%d/%y')
       date2 = ""
       while "next" in data:
           url = data["next"]
           data = requests.get(url).json()
           if "data" in data:
               last_interaction = data['data'][-1]
               story2 = last_interaction['story']
               date2 = dateparser.parse(last_interaction['created_time']).strftime('%m/%d/%y')
       fb_events = "https://graph.facebook.com/v2.4/me?fields=events&access_token="+access_token
       events_data = requests.get(fb_events).json()
       no_of_events = len(events_data['events']['data'])
       while "next" in events_data:
               url = events_data["next"]
               events_data = requests.get(url).json()
               if "data" in events_data:
                  no_of_events += len(events_data['events']["data"])
       jsonTemp = {}
       jsonTemp['first'] = [story1,date1]
       jsonTemp['last'] = [story2,date2]
       jsonTemp['events'] = no_of_events 
       collection2.update(user,{"$set":{'jsonTemp':jsonTemp}})
       #collection2.update(user,{"$set":{'vizDone':0}}) 


if __name__ == "__main__":
	main()
