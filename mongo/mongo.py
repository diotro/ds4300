from pymongo import MongoClient
from datetime import datetime
from pprint import pprint
from random import randrange
import nose

client = MongoClient()
client.drop_database("twitter")
db = client.twitter
tweets = db.tweets
users = db.users

john = {
    "userid": 1001,
    "name": "Rachlin",
    "follows": [1000, 2000, 3000, 4000]
}
users.insert_one(john)

tweet = {
    "userid": 1001,
    "text": "tweet",
    "tags": ["demo"],
    "date": datetime.utcnow()
}

tweets.insert_one(tweet)

def test_john_inserted():
    return users.find_one({"name":"Rachlin"}) is john


nose.main()