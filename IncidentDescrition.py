##################################################
## Project: T-Incident
## Script purpose: incident grouping process
## Date: 7/11/2018
## Authors: Igor and Paulo H. L. Rettore
##################################################

#Aqui estarão as funções relativas a sumarização do texto extraídos dos tuítes, após processo
# de agrupamento.

# =======================================================================================================================
#   LIBRARIES
# =======================================================================================================================

import re
import nltk
from nltk.corpus import stopwords
import re
import indicoio
from nltk.stem import WordNetLemmatizer
import os
indicoio.config.api_key = 'b3ce830aabed72e9e3c708af8c2b29ff'
from nltk.corpus import wordnet
from nltk.tokenize import RegexpTokenizer
import pandas as pd
import heapq
from datetime import date
import csv
import matplotlib.pyplot as plt

# =======================================================================================================================
#   FUNCTIONS
# =======================================================================================================================

# scraped_data = urllib.request.urlopen('https://en.wikipedia.org/wiki/Artificial_intelligence')
# article = scraped_data.read()
#
# parsed_article = bs.BeautifulSoup(article,'lxml')
#
# paragraphs = parsed_article.find_all('p')
#
# article_text = ""
#
# for p in paragraphs:
#     article_text += p.text


def sumarization(data_type, hour,radius= None, type= None ):
        #data_type = data[data['incident_type'] == type]

        #data_original = data_type
        data_type['formated_text'] = ''
        data_type = data_type.reset_index()
        data_type = data_type.drop('index', 1)
        #data_type = to.frame(data_type)
        for index, row in data_type.iterrows():
            raw = row[['text']]
            #raw.lower()
            raw = raw['text'].lower()
            #COLOCAR UM FILTRO DE TEXTO AQUI PARA RETIRARA AS PALAVRAS MAIS ATRAPALHADAS
            # keep only words and removing http
            regex = re.compile(r'[^a-zA-Z\s]:', flags=re.UNICODE)
            raw = regex.sub(r'', raw)
            raw = re.sub(r"http\S+", "", raw)
            raw = re.sub("\d+", "", raw)
            ##############################################################################################################
            #TOKENIZATION/ LEMMANIZATION AND REMOVING STOP WORDS
            tokenizer = RegexpTokenizer(r'\w+')
            tokens = tokenizer.tokenize(raw)
            tagged = nltk.pos_tag(tokens)
            wnl = nltk.WordNetLemmatizer()
            #filtered_words = [wnl.lemmatize(w) for w in tokens if w not in stopwords.words('english')]
            sentence = ' '.join(tokens)
            #data_type.iloc[index]['formated_text'] = sentence
            data_type.loc[index, 'formated_text'] = sentence
        formated_data_text = ""


        #print(filtered_words)

        for p, sentence in data_type.iterrows():
            formated_data_text += sentence['formated_text']

        #sentence_list = list(data_type['text'])

        #conto a frequência de palavras
        stopwords = nltk.corpus.stopwords.words('english')
        word_frequencies = {}
        for word in nltk.word_tokenize(formated_data_text):
            if word not in stopwords:
                if word not in word_frequencies.keys():
                    word_frequencies[word] = 1
                else:
                    word_frequencies[word] += 1

        #sentence_scores = {}

        #remover os duplicados
        data_type = data_type.drop_duplicates(['formated_text'], keep='first')
        data_type ['score'] = None
        data_type = data_type.reset_index()
        data_type = data_type.drop('index', 1)

        soma = 0

        for index,sent in data_type.iterrows():
                soma = 0
                for word in sent['formated_text'].split(' '):
                    if word in word_frequencies.keys():
                        if len(sent['formated_text'].split(' ')) < 30:
                            soma += word_frequencies[word]
                data_type.loc[index, 'score'] = soma

        data_type = data_type.sort_values('score',ascending=False)
        #reindex
        data_type = data_type.reset_index()
        data_type = data_type.drop('index', 1)


        #summary_sentences = heapq.nlargest(10, sentence_scores, key=sentence_scores.get)

        #summary = '\n'.join(summary_sentences)
        summary_sentences = list(data_type.loc[1:10,]['text'])
        summary = '\n'.join(summary_sentences)
        #
        print('#####################################################################################################')
        print('The summary for ' + str(type) )
        print( 'Hour:' + str(hour))
        print('Radius:' + str(radius) + '\n')
        print(summary)
        print('#####################################################################################################')

def tweetIncidentDescriptionById(radius_list, hour_list):

    for radius in radius_list:
        data_main = pd.read_csv('data/data_grouped/50_NOT_INCIDENT/twitter_incident(' + radius + ').csv', parse_dates=['created_at'])  # CSV criado na etapa anterior contendo os tweets com seus lat-long
        data_main['created_at'] = data_main['created_at'] - pd.Timedelta(hours=4)  # adjusting local time
        incident = data_main[['id_str', 'incident_id']].groupby('incident_id').count()
        incident['id_incident'] = incident.index
        id_incident_most_commom = incident.sort_values('id_str', ascending=False).iloc[0]['id_incident']
        data_incident_select = data_main[data_main['incident_id'] == id_incident_most_commom]
        data_incident_select['hour'] = data_incident_select.created_at.apply(lambda x: x.hour)
        for hour in hour_list:
            data_incident_select_per_hour = data_incident_select[data_incident_select['hour'] == hour]
            sumarization(data_incident_select_per_hour, hour, radius=radius,
                         type='incident id ' + str(id_incident_most_commom))
def tweetDescriptionPerHourandType (radius_list, type_list, hour_list):  #Descricao por hora e tipo de incident
    for radius in radius_list:
        data_main = pd.read_csv('data/data_grouped/50_NOT_INCIDENT/twitter_incident(' + radius + ').csv', parse_dates= ['created_at'])  # CSV criado na etapa anterior contendo os tweets com seus lat-long
        data_main['created_at'] = data_main['created_at'] - pd.Timedelta(hours=4)   #adjusting local time
        data = data_main[['text','incident_type','created_at']]
        #select the categories you want
        data['hour'] = data.created_at.apply(lambda x: x.hour)
        #per hour
        #pint dos tweets per categora (tipo de incident)
        for type in type_list:
            for hour in hour_list:
                data_to_send = data[data['incident_type'] == type]
                data_to_send = data_to_send[data_to_send['hour'] == hour]
                if (len(data_to_send) > 0):
                    sumarization(data_to_send,hour, radius,type)
                else:
                    print('Not data in type ' + str(type) + ' in hour ' + str(hour))

if __name__ == '__main__':

    #radius_list = ['Radius0.01','Radius0.05','Radius0.1','Radius0.2','Radius0.3','Radius0.4','Radius0.5']
    radius_list = ['Radius0.05', 'Radius0.1']
    type_list = ['CONGESTION', 'ACCIDENT', 'CONSTRUCTION', 'DISABLED VEHICLE', 'MISCELLANEOUS', 'PLANNED EVENT']
    hour_list = range(1,24)  #list of hour you want to see

    tweetDescriptionPerHourandType(radius_list,type_list,hour_list)
    #tweetIncidentDescriptionById(radius_list, hour_list)
