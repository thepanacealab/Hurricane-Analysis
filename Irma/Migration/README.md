### Steps to accumulate and visualize migration tweets data
- Read Irma residents Rds to dataframe and extract and save user ids to csv (IrmaResidents.csv)
- Bulk convert user ids to user names/handles (IrmaNames.csv)
- Convert names to python list (user.py)
- execute scrape.py (https://github.com/thepanacealab/Hurricane-Analysis/blob/master/tweet_scraper.zip) for each user in user.py and accumulate tweet id between desired date ranges
- execute get_metadata.py to get tweet details from the tweet ids.
- extract the user name, longitude and latitude info from tweet.json (read_json.py)
- create dataframe with long/lat columns and project as points and trajectory path on US map (domestic.py) and world map (international.py)
- save plot images (Mx_migrationx.PNG etc.)
