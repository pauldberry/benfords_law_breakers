# -*- coding: utf-8 -*-
"""
Created on Thu Apr  9 05:43:50 2015

@author: Christian Pecaut
"""

from bs4 import BeautifulSoup
import urllib2

url = "http://www.bbb.org/dayton/business-reviews/property-management/lasalle-partners-in-dayton-oh-14000527"

content = urllib2.urlopen(url).read()

soup = BeautifulSoup(content)

#soup = soup.decode('utf-8')
print soup.prettify("UTF-8")

#for link in soup.find_all('principal:'):
#    print(link.nextelement)

#print soup
#print(soup.get_text())

#print title

#print soup.title.string
#
#print soup.p
 