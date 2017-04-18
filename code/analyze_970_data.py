import requests
import scipy
import sklearn
import skfuzzy
import utils
import csv
import pandas

## config
configURI = "http://grad.geography.wisc.edu:8080/mapConfigs"

## get the data
mapData = requests.get(configURI).json()

## convert it to analysis form
forAnalysis = utils.configJSONToDF(mapData['data'])

## add fields for analysis
forAnalysis['hasAuthor'] = pandas.notnull(forAnalysis['author'])
forAnalysis['hasOrganization'] =  pandas.notnull(forAnalysis['organization'])
forAnalysis['hasTitle'] =  pandas.notnull(forAnalysis['title'])
forAnalysis['hasDescription'] =  pandas.notnull(forAnalysis['description'])
forAnalysis['usedTaxonIDNumber'] = forAnalysis['configdata.state.taxonsearch'].str.isnumeric()

forAnalysis.to_csv("./../data.csv")
