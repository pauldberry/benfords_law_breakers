###############################################################################
#                                                                             #
#  Census tract look up program                                               #
#  Coded by Scarlett Swerdlow                                                 #
#  scarlettswerdlow@uchicago.edu                                              #
#  April 6, 2015                                                              #
#                                                                             #
###############################################################################

import bs4
import requests
import urllib

###############
#  CONSTANTS  #
###############

FCC_PATH = 'http://data.fcc.gov/api/block/find?'
GOOGLE_PATH = 'https://maps.googleapis.com/maps/api/geocode/xml?'
GOOGLE_KEY = # To be filled in by user
WSAPI_KEY = # To be filled in by user

###############
#  FUNCTIONS  #
###############

def lookup_tract(lat, lng):
	"""
	Citation: Original.
	Helper function that looks up tract for given lat and lng.
	Args:
		lat (float): Lat to be passed to API.
		lng (float): Lng to be passed to API.
	Returns:
		tract (int).
	"""
	d = {'format': 'xml', 'latitude': lat, 'longitude': lng, 
		 'showall': 'false'}
	url = FCC_PATH + urllib.urlencode(d)
	soup = make_soup(url)
	block_fips = soup.find("block").attrs['fips']
	# Tract is 6-digit string in middle of block FIPS
	tract = int(str(block_fips[5:11]))
	return tract


def lookup_ll(address, city, state):
	"""
	Citation: Original.
	Helper function that looks up lat and lng for given address.
	Args:
		address (str): Address to be passed to API.
		city (str): City to be passed to API.
		state (str): State to be passed to API.
	Returns:
		lat (float) and lng (float).
	"""
	complete_address = address + ',' + city + ',' + state
	d = {'address': complete_address, 'key': GOOGLE_KEY}
	url = GOOGLE_PATH + urllib.urlencode(d)
	soup = make_soup(url)
	if soup.find("status").text == "ZERO_RESULTS":
		print "No latitude and longitude for " + complete_address
		return None
	lat = float(soup.find("lat").text.encode("UTF-8"))
	lng = float(soup.find("lng").text.encode("UTF-8"))
	return lat, lng


def make_soup(url):
	"""
	Citation: Original.
	Helper function that makes soup of given url.
	Args:
		url (str): Url to be turned into soup.
	Returns:
		Soup.
	"""
	html = requests.get(url)
	if html == None:
		print 'No HTML for ' + url
		return None
	else:
		text = html.text.encode('iso-8859-1')
		soup = bs4.BeautifulSoup(text)
	return soup


