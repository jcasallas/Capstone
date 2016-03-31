# -*- coding: utf-8 -*-
"""
Created on Wed Nov 25 13:26:38 2015

@author: rakesh 
"""

import tweepy
import sys
import jsonpickle
from os import path

###########################
def twitter_auth():
    
    ckey    = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXX'
    csecret = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXX'
    auth = tweepy.AppAuthHandler(ckey, csecret)
 
    api = tweepy.API(auth, wait_on_rate_limit=True, wait_on_rate_limit_notify=True)
 
    if (not api):
        print ("Can't Authenticate")
        sys.exit(-1)
       
    print ("****** Twitter Account - Authenticated ********")
 
    return api


###########################             ###########################    
    ###########################    ###########################      
###########################             ###########################    

if __name__ == '__main__':
#C:\\Users\\casal_000\\Desktop\\CAPSTONE
   
    dirn ="C:\\Users\\XXXXXXX\\XXXXXXXX\\Documents\\CAPSTONE\\CAPSTONE\\Archive Data Sets"
    filen = 'JCSolarEnergy201603026.txt' 
    
    fname = path.join(dirn, filen)
    searchQuery = 'SolarEnergy'  
    maxTweets = 100000000 # Some arbitrary large number
    tweetsPerQry = 100  # this is the max the API permits
    
    sinceId = None #no lower limit, go as far back as API allows
    max_id = -1 #If results only below a specific ID are, set max_id to that ID

    api = twitter_auth() # Twitter authentication
    tweetCount = 0
    
    print("Downloading max {0} tweets".format(maxTweets))
    
    with open(fname, 'w') as f:
        while tweetCount < maxTweets:
            try:
                if (max_id <= 0):
                    if (not sinceId):
                        new_tweets = api.search(q=searchQuery, count=tweetsPerQry)
                    else:
                        new_tweets = api.search(q=searchQuery, count=tweetsPerQry,
                                                since_id=sinceId)
                else:
                    if (not sinceId):
                        new_tweets = api.search(q=searchQuery, count=tweetsPerQry,
                                                max_id=str(max_id - 1))
                    else:
                        new_tweets = api.search(q=searchQuery, count=tweetsPerQry,
                                                max_id=str(max_id - 1),
                                                since_id=sinceId)
                if not new_tweets:
                    print("No more tweets found")
                    break

        
                
                for tweet in new_tweets:
                    #f.write(tweet.text.encode('utf-8') + #'\n')
                    #f.write(jsonpickle.encode(tweet._json, unpicklable=False) +
                     f.write(jsonpickle.encode(tweet.id, unpicklable=False) +
                             "," + jsonpickle.encode(tweet.text, unpicklable=False)+
                             "," + jsonpickle.encode(tweet.user.id, unpicklable=False)+
                            "," + jsonpickle.encode(tweet.user.name, unpicklable=False)+
                            "," + jsonpickle.encode(tweet.user.location, unpicklable=False)+
                             "," + jsonpickle.encode(tweet.coordinates, unpicklable=False)+
                            "," + jsonpickle.encode(tweet.created_at, unpicklable=False)+
                           '\n')                            
                tweetCount += len(new_tweets)
                
                print("Downloaded {0} tweets".format(tweetCount))
                
                max_id = new_tweets[-1].id
                
            except tweepy.TweepError as e:
                # Just exit if any error
                print("some error : " + str(e))
                break
    
    print ("Downloaded {0} tweets, Saved to {1}".format(tweetCount, fname))
    
    
    
    
