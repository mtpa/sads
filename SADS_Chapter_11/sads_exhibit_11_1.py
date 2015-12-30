# Simple One-Site Web Crawler and Scraper (Python)
#
# prepare for Python version 3x features and functions
from __future__ import division, print_function

# scrapy documentation at http://doc.scrapy.org/

# workspace directory set to outer folder/directory wnds_chapter_3b
# the operating system commands in this example are Mac OS X

import scrapy  # object-oriented framework for crawling and scraping
import os  # operating system commands

# function for walking and printing directory structure
def list_all(current_directory):
    for root, dirs, files in os.walk(current_directory):
        level = root.replace(current_directory, '').count(os.sep)
        indent = ' ' * 4 * (level)
        print('{}{}/'.format(indent, os.path.basename(root)))
        subindent = ' ' * 4 * (level + 1)
        for f in files:
            print('{}{}'.format(subindent, f))

# initial directory should have this form (except for items beginning with .):
#    sads_exhibit_11_1
#        run_one_site_crawler.py
#        scrapy.cfg
#        scrapy_application/
#            __init__.py
#            items.py
#            pipelines.py
#            settings.py
#            spiders
#                __init__.py
#                one_site_crawler.py

# examine the directory structure
current_directory = os.getcwd()
list_all(current_directory)

# list the avaliable spiders, showing names to be used for crawling
os.system('scrapy list')

# decide upon the desired format for exporting output: csv, JSON, or XML

# run the scraper exporting results as a JSON text file items.jsonlines
# this file provides text information with linefeeds to provide
# text output that is easily readable in a plain text editor
os.system('scrapy crawl TOUTBAY -o items.jsonlines')

# output formats commented out (choose the one needed for further parsing work)
# run the scraper exporting results as a comma-delimited text file items.csv
# os.system('scrapy crawl TOUTBAY -o items.csv')

# run the scraper exporting results as a JSON text file items.json
# os.system('scrapy crawl TOUTBAY -o items.json')

# run the scraper exporting results as a dictionary XML text file items.xml
# os.system('scrapy crawl TOUTBAY -o items.xml')


# ----------------------------
# MyItem class defined by 
# items.py
# ----------------------------
# location in directory structure:
# sads_exhibit_11_1/scrapy_application/items.py

# establishes data fields for scraped items

import scrapy  # object-oriented framework for crawling and scraping

class MyItem(scrapy.item.Item):
    # define the data fields for the item (just one field used here)
    paragraph = scrapy.item.Field()  # paragraph content


# ----------------------------
# MyPipeline class defined by 
# pipelines.py
# ----------------------------
# location in directory structure:
# sads_exhibit_11_1/scrapy_application/pipelines.py

class MyPipeline(object):
    def process_item(self, item, spider):
        return item

# ----------------------------
# settings for scrapy.cfg 
# settings.py
# ----------------------------
# location in directory structure:
# sads_exhibit_11_1/scrapy_application/settings.py

BOT_NAME = 'MyBot'
BOT_VERSION = '1.0'

SPIDER_MODULES = ['scrapy_application.spiders']
NEWSPIDER_MODULE = 'scrapy_application.spiders'
USER_AGENT = '%s/%s' % (BOT_NAME, BOT_VERSION)
COOKIES_ENABLED = False
DOWNLOAD_DELAY = 2  
RETRY_ENABLED = False
DOWNLOAD_TIMEOUT = 15
REDIRECT_ENABLED = False
DEPTH_LIMIT = 50

# ----------------------------
# spider class defined by 
# script one_site_crawler.py
# ----------------------------
# location in directory structure:
# sads_exhibit_11_1/scrapy_application/spiders/one_site_crawler.py

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# each spider class gives code for crawing and scraping

import scrapy  # object-oriented framework for crawling and scraping
from scrapy_application.items import MyItem  # item class 
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor

# spider subclass inherits from BaseSpider
# this spider is designed to crawl just one website
class MySpider(CrawlSpider):
    name = "TOUTBAY"  # unique identifier for the spider
    allowed_domains = ['toutbay.com']  # limits the crawl to this domain list
    start_urls = ['http://www.toutbay.com']  # first url to crawl in domain
       
    # define the parsing method for the spider
    def parse(self, response):
        html_scraper = scrapy.selector.HtmlXPathSelector(response)
        divs = html_scraper.select('//div')  # identify all <div> nodes
        # XPath syntax to grab all the text in paragraphs in the <div> nodes
        results = []  # initialize list
        this_item = MyItem()  # use this item class
        this_item['paragraph'] = divs.select('.//p').extract()  
        results.append(this_item)  # add to the results list
        return results 

