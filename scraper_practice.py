#~ from lxml import html
#~ print("checkpoint0")
#~ import requests
#~ print("checkpoint1")
#~ page = requests.get('http://econpy.pythonanywhere.com/ex/001.html')
#~ tree = html.fromstring(page.content)
#~ buyers = tree.xpath('//div[@title="buyer-name"]/text()')
#~ prices = tree.xpath('//span[@class="item-price"]/text()')
#~ n = len(buyers)
#~ pairing = ()
#~ for i in range(n):
#~ pairing = pairing + (buyers[i], prices[i])
#~ print(pairing)


from lxml import html
import requests
from random import randrange
import time
from bs4 import BeautifulSoup
import numpy as np

def cleanhtml(list):
	n = len(list)
	for k in range(n):
		list[k] = BeautifulSoup(list[k], "lxml").text
	return list

page = requests.get('https://sfbay.craigslist.org/d/rooms-shares/search/roo')
tree = html.fromstring(page.content)
location = tree.xpath('//span[@class="result-hood"]/text()')
price = tree.xpath('//span[@class="result-price"]/text()')
link = tree.xpath('//a[@class="result-title hdrlnk"]/text()')
n = len(location)
k = len(price)
pairing = ()
for i in range(n):
	pairing = pairing + (location[i], price[i])
	

html_soup=BeautifulSoup(page.text, 'html.parser')
# gets the results from bottom of page
results_num = html_soup.find('div', class_='search-legend')
# digs into results to find total page count
results_total = int(results_num.find('span', class_='totalcount').text)
resultscounter=120
post_timing = []
post_hoods = []
post_title_texts = []
post_links = []
post_prices = []
post_attribute = []
replacespan = {'<span>', '</span>'} 
posts = html_soup.find_all('li', class_= 'result-row')
for post in posts:
	if post.find('span', class_ = 'result-hood') is not None:
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
		print(post_attriblist)
		post_attriblist = cleanhtml(post_attriblist)
		post_spans.append(post_attriblist)
		print(post_attribute)
		
		

"""
While pagecounter<=results_total:
	page = requests.get('https://sfbay.craigslist.org/d/rooms-shares/search/roo'+'?s='+str(resultscounter))
	ran = randrange(1,6)
	time.sleep(ran)
	if page.status_code != 200:
		print("error: "+page.status_code)
		continue
	html_soup=BeautifulSoup(page.text, 'html.parser')
	posts = html_soup.find_all('li', class_= 'result-row')
	for post in posts:
		if post.find('span', class_ = 'result-hood') is not None:
			if post.find('span', class_ = 'result-price') is not None:
				post_datetime = post.find('time', class_= 'result-date')['datetime']
				post_timing.append(post_datetime)
				post_hood = post.find('span', class_= 'result-hood').text
				post_hoods.append(post_hood)
				#post link
				post_link = post_title['href']
				post_links.append(post_link)

				#removes the \n whitespace from each side, removes the currency symbol, and turns it into an int
				post_price = int(post.a.text.strip().replace("$", "")) 
				post_prices.append(post_price)
	resultscounter+=120
	print('success '+str(resultscounter))
print(html_soup)
"""
import pandas as pd

eb_apts = pd.DataFrame({'posted': post_timing, 
						'neighborhood': post_hoods,
						'post title': post_title_texts,
						'link' : post_links,
						'prices' :  post_prices,
						'attributes' : post_attribute})
