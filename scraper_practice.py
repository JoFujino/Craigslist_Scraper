# Optimally we'd run this with a proxyscraper to avoid having a large number of requests coming from the same 
#import deletetable
import lxml
import requests
from bs4 import BeautifulSoup
from random import randrange
import time
import numpy as np
import pandas as pd
from sqlalchemy import create_engine

#First lets build a way to control how large a size N sample we are collecting (each cycle takes about 3 seconds)
def getcycles():
	cycles = eval(input("Enter the number of entries to scrape. Note output\n"
				   "may be slightly less as we may scrape duplicates\n" 
				   "due to new posts pushing down existing entries: "))
	return(cycles)
	# Max 3000

def cleanhtml(list):
	for k in range(len(list)):
		list[k] = BeautifulSoup(str(list[k]),'html.parser').text
	return list

# Lets start by ensuring we can make a connection to our SQL database.

#Note: 'craigsdata' is the name of the database.
#sqlEngine = create_engine('mysql+pymysql://root:@192.168.0.108/craigsdata', pool_recycle=3600)
sqlEngine = create_engine('mysql+pymysql://root:r3e3d3@127.0.0.1', pool_recycle=3600)
dbConnection    = sqlEngine.connect()
print("connection successful")

dbConnection.execute("CREATE DATABASE IF NOT EXISTS craigsdata;")
dbConnection.execute("USE craigsdata;")
sqlEngine = create_engine('mysql+pymysql://root:r3e3d3@127.0.0.1/craigsdata', pool_recycle=3600)
dbConnection    = sqlEngine.connect()
sql = str('DROP TABLE IF EXISTS scrapeddata;')
result = sqlEngine.execute(sql)
# This section gets the results count from bottom of page
# 1. we'll pull up the front page of the area we're scraping
page = requests.get('https://sfbay.craigslist.org/d/rooms-shares/search/roo')
html_soup=BeautifulSoup(page.text, 'html.parser')
# html_soup.finds gets the results from bottom of page
results_num = html_soup.find('div', class_='search-legend')
# digs into results to find total page count.  This will work as our hard max to how much we scrape.
results_total = int(results_num.find('span', class_='totalcount').text)
cycles = getcycles()

#These are the lists we'll use as containers for our data.
resultscounter=0
post_timing = []
post_hoods = []
post_title_texts = []
post_links = []
post_prices = []
post_attribute = []
#These are tags we'll reference as to be removed from post_attributes
replacespan = {'<span>', '</span>'} 
		
print('Pre-scrape setup done big loop')
while resultscounter<=results_total:
	page = requests.get('https://sfbay.craigslist.org/d/rooms-shares/search/roo'+'?s='+str(resultscounter))
	# We need to sleep periodically to avoid craigslist anti-scraper defeneses (to avoid spammers spamming ad posters).
	# if we couldn't connect to the webpage we'll print out an error and skip the rest of the loop, which also avoids counting it in our resultscounter var. 
	if page.status_code != 200:
		print("error: "+page.status_code)
		continue
	html_soup=BeautifulSoup(page.text, 'html.parser')
	posts = html_soup.find_all('li', class_= 'result-row')
	for post in posts:
		ran = randrange(1,5)
		time.sleep(ran)
		if post.find('span', class_ = 'result-hood') is not None:
			if post.find('span', class_ = 'result-price') is not None:
				#filling out data
				post_datetime = post.find('time', class_= 'result-date')['datetime']
				post_timing.append(post_datetime)
				post_hood = post.find('span', class_= 'result-hood').text
				post_hoods.append(post_hood)
				post_title = post.find('a', class_='result-title hdrlnk')
				post_title_text = post_title.text
				post_title_texts.append(post_title_text)
				post_link = post_title['href']
				post_links.append(post_link)
				post_price = int(post.find('span', class_='result-price').text.replace("$", "")) 
				post_prices.append(post_price)
				ran = randrange(1,5)
				time.sleep(ran)
				subpage = requests.get(post_link)
				html_sub = BeautifulSoup(subpage.text, 'html.parser')
				post_attgp = html_sub.find_all('p', class_='attrgroup')
				post_spans = []
				for j in range(len(post_attgp)):
					post_attriblist = post_attgp[j].find_all('span', class_='')
				post_attriblist = cleanhtml(post_attriblist)
				post_attribute.append(str(post_attriblist).strip('[]'))
	resultscounter+=120
	print('success '+str(resultscounter))
	print(post_price)
	# Stop the loop if you have reached your desired sample size.
	if resultscounter>cycles:
		break

print("loops done")


eb_apts = pd.DataFrame({'date': post_timing,
						'neighborhood': post_hoods,
						'title': post_title_texts,
						'link' : post_links,
						'prices' :  post_prices,
						'attributes' : post_attribute})
eb_apts.sort_values(by = ['neighborhood', 'date'])
# since we're scraping while new posts are being added pushing down the existing posts we can sometimes accidentally scrape the same ad twice.
# to address this we need to drop the duplicates.
eb_apts.drop_duplicates
# Neighborhood is in parenthesis so we're stripping thosee out.
eb_apts['neighborhood'] = eb_apts['neighborhood'].map(lambda x: x.replace('(','',1))
eb_apts['neighborhood'] = eb_apts['neighborhood'].map(lambda x: x.replace(')','',1))
	
print("cleaned dataframe")
print(eb_apts)	
# "scrapeddata" will be our table name.
eb_apts.to_csv("eb_apts_sample_clean.csv", index=False)

try:

	frame           = eb_apts.to_sql('scrapeddata', dbConnection, if_exists='fail')

except ValueError as vx:

	print('value error' + '\n')
	print(vx)

except Exception as ex:   

	print('Exception' + '\n')
	print(ex)

else:

	print("Table \"scrapeddata\" created successfully.")   

finally:

	dbConnection.close()

#Now open RStudio and Run the DataVisualization script.

