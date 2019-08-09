#Analyzing Google Scholar Data for CSSS Participants
#TODO: Search for all authors on coauthored publications in case some were CSSS participants without Google Scholar profiles?

import csv
import scholarly
import pickle
import os.path
from unidecode import unidecode

#Creates list of CSSS participant names from 'CSSS_Data_All.csv'
def create_list_of_names():
	with open('CSSS_Data_All.csv', newline='') as csss_all:
	
		list_of_names = []
	
		csss_reader = csv.DictReader(csss_all)	

		for row in csss_reader: #creates list of names of all participants in dataset (excluding any duplicates - i.e. people who participated in more than one project)
			if row['Name'] not in list_of_names:
				list_of_names.append(row['Name'])
			else:
				pass

	return list_of_names

#Uses Scholarly to query Google Scholar for name in list, then adds author object to dictionary and pickles dictionary to file
#Dictionary key == CSSS Name, dictionary value == author object
#DONE - doesn't need to be called again - author objects have been saved to file and it took forever the first time
def fetch_and_pickle(list_of_names, file_name):
	dictionary_of_author_objects = {}

	if os.path.exists(file_name):
		with open(file_name, 'rb') as f:
			dictionary_of_author_objects = pickle.load(f) #unpickled

	for i, name in enumerate(list_of_names): #creates dictionary of author objects stored in memory
		if name in dictionary_of_author_objects:
			continue

		search_query = scholarly.search_author(name)
		
		try:
			author_object = next(search_query).fill()
			print(i, name, author_object.name)
		except: #when name is not found in Google Scholar
			author_object = 'None'
			print(i, name, author_object)

		dictionary_of_author_objects[name] = author_object

		if i % 5 == 0:
			pickle.dump(dictionary_of_author_objects, open(file_name, 'wb')) #creates file and serializes dictionary of author objects

	pickle.dump(dictionary_of_author_objects, open(file_name, 'wb'))
#fetch_and_pickle(create_list_of_names(), 'author_objects.p')

#Opens csv of manual lookups and searches Google Scholar for those particular names. Returns dictionary with key == CSSS Name and value == new author object (for CSSS participants that initally returned 'None' but profile was located manually)
def find_correct_profile(file_name):
	new_scholar_dict = {}

	with open(file_name, newline='') as f:

		file_reader = csv.DictReader(f)

		for row in file_reader:
			if row['Found Manually?'] != 'No' and row['Found Manually?'] != 'Pubs, No Profile' and row['Found Manually?'] != 'NA':
				search_query = scholarly.search_author(row['Found Manually?'])
				author_object = next(search_query).fill()

				csss_name = row['Not Found by Scholarly']
				print(csss_name)

				new_scholar_dict[csss_name] = author_object

	return new_scholar_dict
#new_scholar_dict = find_correct_profile('Not_Found_in_Scholar.csv')

#Adds results of find_correct_profile function to original pickled dictionary
#DONE - doesn't need to be called again unless more profiles are found
def update_pickled_dict(pickled_file, new_scholar_dict):
	with open(pickled_file, 'rb') as f:
		dictionary_of_author_objects = pickle.load(f) #unpickled

		for key, value in new_scholar_dict.items():
			dictionary_of_author_objects[key] = value

	pickle.dump(dictionary_of_author_objects, open(pickled_file, 'wb')) #creates file and serializes dictionary of author objects
#update_pickled_dict('author_objects.p', new_scholar_dict)

#Converts dictionary to csv
#NOT IN USE - csv had a bunch of errors, not sure why
def dictionary_to_csv(file_name):
	with open(file_name, 'rb') as f:
		unpickled_dictionary = pickle.load(f)

	with open('author_objects.csv', 'w', newline='') as dict_to_csv:

		headers = ['csss_name', 'name', 'affiliation', 'email', 'interests', 'citedby', 'hindex', 'coauthors', 'publications']
		dict_writer = csv.DictWriter(dict_to_csv, fieldnames=headers)
		dict_writer.writeheader()

		count = 0

		for csss_name, author_object in unpickled_dictionary.items():
			print(count, csss_name)
			dict_writer.writerow(auth_to_dict(csss_name, author_object))
			
			count += 1
#dictionary_to_csv('author_objects.p')

#Creates values for scholar_dict(file_name) function
def auth_to_dict(csss_name, author_object):
	if author_object == 'None':
		value = {'csss_name': csss_name}
			
	elif not hasattr(author_object, 'citedby'): 
		value = {
			'csss_name': csss_name, 
			'name': author_object.name, 
			'affiliation': author_object.affiliation, 
			'email': author_object.email, 
			'interests': author_object.interests, 
			'hindex': author_object.hindex, 
			'coauthors': [coauth.name for coauth in author_object.coauthors], 
			'publications': [pub.bib['title'] for pub in author_object.publications]
			}

	else:
		value = {
			'csss_name': csss_name, 
			'name': author_object.name, 
			'affiliation': author_object.affiliation, 
			'email': author_object.email, 
			'interests': author_object.interests, 
			'citedby': author_object.citedby, 
			'hindex': author_object.hindex, 
			'coauthors': [coauth.name for coauth in author_object.coauthors], 
			'publications': [pub.bib['title'] for pub in author_object.publications]
			}
	
	return value

#Converts pickled file to iterable dictionary where key == CSSS Name and value == dictionary of Google Scholar data
def scholar_dict(file_name):
	with open(file_name, 'rb') as f:
		unpickled_dictionary = pickle.load(f)

		scholar_dictionary = {}

		count = 0

		for csss_name, author_object in unpickled_dictionary.items():
			#print(count, csss_name)

			scholar_dictionary[csss_name] = auth_to_dict(csss_name, author_object)

			count += 1
		
		return scholar_dictionary
scholar_dictionary = scholar_dict('author_objects.p')

#Number of CSSS names for which Google Scholar profiles could not be found
count = 0
for key, value in scholar_dictionary.items():
	if 'affiliation' not in value:
		count += 1
		#print(key)
#print(count)

#Creates list of CSSS participants where name from our dataset was exact match with name on Google Scholar profile found by Scholarly
def same_names(scholar_dictionary):
	name_matches = []

	for key, value in scholar_dictionary.items():
		try:
			if unidecode(key.lower()) == unidecode(value['name'].lower()):
				name_matches.append(value['name'])

		except:
			continue

	return name_matches
name_matches = same_names(scholar_dictionary)
#print(len(name_matches))

#Creates list of all coauthors in Google Scholar profiles found by Scholarly
def list_of_coauthors(scholar_dictionary):
	list_of_coauthors = []

	for key, value in scholar_dictionary.items():
		try:
			for coauthor in value['coauthors']:
				list_of_coauthors.append(coauthor)

		except:
			continue

	return list(set(list_of_coauthors))	
all_coauthors = list_of_coauthors(scholar_dictionary)

#Creates list of all names in Google Scholar profiles found by Scholarly (many incorrect - not CSSS participant, someone with same or similar name)
def list_of_scholar_names(scholar_dictionary):
	list_of_scholar_names = []

	for key, value in scholar_dictionary.items():
		try:
			list_of_scholar_names.append(value['name'])
		except:
			continue

	return list_of_scholar_names
all_scholar_names = list_of_scholar_names(scholar_dictionary)

#Creates dictionary where key == CSSS Name and value == Google Scholar profile name
def dict_of_scholar_names(scholar_dictionary):
	dict_of_scholar_names = {}

	for key, value in scholar_dictionary.items():
		try:
			dict_of_scholar_names[key] = value['name']
		except:
			continue

	return dict_of_scholar_names
#dict_scholar_names = dict_of_scholar_names(scholar_dictionary)

#Creates list of CSSS participants (using name on Google Scholar profile) who were found in all_coauthors 
def list_of_collaborators(all_scholar_names, all_coauthors):
	collaborators = []
	
	for name in all_scholar_names:
		if name in all_coauthors:
			collaborators.append(name)

	return list(set(collaborators))
collaborators = list_of_collaborators(all_scholar_names, all_coauthors)
#print(len(collaborators))
if 'Jonas M. B. Haslbeck' in all_coauthors:
	print('jonas is in collabs')

#for key, value in scholar_dictionary.items():
#	try:
#		if value['citedby'] > 5000:
#			print(value['csss_name'] + ' / ' + value['name'])
#	except:
#		continue

#Creates dictionary where key == publication name and value == list of CSSS participants who collaborated on it
#Conditionals required to deal with duplicate names/profiles in scholar_dictionary
#HAS ERRORS - if Google Scholar profile is missing coauthors object, person's list of coauthors will be empty - won't intersect with other CSSS participants who may be collaborators
def collabs(collaborators, scholar_dictionary):
	all_pubs = {}
	copubs = {}

	for value in scholar_dictionary.values():
		try:
			if value['name'] in collaborators:
				for pub in list(set(value['publications'])):
					if pub not in all_pubs.keys():
						all_pubs[pub] = value['name']
					elif pub in all_pubs.keys() and all_pubs[pub] == value['name']:
						all_pubs[pub] = value['name']
					elif pub in all_pubs.keys() and pub not in copubs.keys():
						copubs[pub] = all_pubs[pub].append(value['name'])
					elif pub in all_pubs.keys() and pub in copubs.keys() and value['name'] in copubs[pub]:
						continue
					elif pub in all_pubs.keys() and pub in copubs.keys():
						copubs[pub].append(value['name'])
					
		except:
			continue

	return copubs
#copubs = collabs(collaborators, scholar_dictionary)
#print(len(copubs))

###HAS ERRORS - returns many copublications between two or more people who likely did not even attend CSSS (but have common names, so Scholarly returned their profiles instead of CSSS profiles)
###Not sure what to do about it. Remove profiles of people with > 5000 citations? Remove profiles of people with short (and therefore likely more common) names?
def collab_pubs(scholar_dictionary):
	all_publications = {}
	copublications = {}

	for key, value in scholar_dictionary.items():
		try:
			for pub in list(set(value['publications'])):
				if pub in all_publications.keys() and pub in copublications.keys() and value['name'] in copublications[pub]:
					continue
				elif pub in all_publications.keys() and all_publications[pub] == value['name']:
						all_pubs[pub] = value['name']
				elif pub in all_publications.keys() and pub in copublications.keys():
					copublications[pub].append(value['name'])
				elif pub in all_publications.keys() and pub not in copublications.keys():
					copublications[pub] = [all_publications[pub], value['name']]
				else:
					all_publications[pub] = value['name']

		except:
			continue

	return copublications

copublications = collab_pubs(scholar_dictionary)

print(copublications)
print(len(copublications))


