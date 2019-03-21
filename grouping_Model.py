##################################################
## Project: T-Incident
## Script purpose: incident grouping process
## Date: 15/10/2018
## Authors: Igor and Paulo H. L. Rettore
##################################################

# Aqui estarão as funçoes que são relativas as fusões das duas fontes de dados INCIDENTE & TWITTER
# e tem como objetivo a clusterização dos mesmos, ou seja, coloca-los em certos grupos de acordo com
# a relação espaço temporal dos mesmos com os incidents

# =======================================================================================================================
#   LIBRARIES
# =======================================================================================================================

import csv
import time

import pandas as pd
from geopy.distance import geodesic
import os.path
import os
from multiprocessing import Process
import threading
import math
from datetime import datetime
from datetime import date
from pytz import timezone


# =======================================================================================================================
#   FUNCTIONS
# =======================================================================================================================


def creatingFolders(dataFolder):
    if (os.path.isdir(dataFolder) == False):
        os.makedirs(dataFolder)


def inside_ray(coordinates_1, coordinates_2, radius): #Verificar se o tweet está dentro do radius de alcance do incident
    distance = geodesic(coordinates_1, coordinates_2)
    if (distance < radius):
        return distance
    else:
        return False

# Recebo dois pontos e distância atual verifica se:
#   - A distância deles estão dentro de um RAIO
#   - Se a distância é menor que a atual
def spatio_filter(coordinates_tweet, coordinates_incident, atual_distance, radius):
    distance = inside_ray(coordinates_tweet,coordinates_incident, radius )
    #print(distance)
    # testo se esta dentro do raio
    if (distance > 0):
        # testo se a distância é menor que a atual
        if (distance < atual_distance):
            return True  # Passou no filtro espacial OK!
        return False
    else:
        return False

#Verifica se os tweets estão tempo do intervalo que ocorreu o 'incident'
def temporal_filter(start_incident, end_incident, tweet_timestamp):
    if (tweet_timestamp < end_incident and tweet_timestamp > start_incident):  # Se o meu twitter estiver dentro do intervalo de tempo, eu retorno True
        return True
    else:
        return False


def makeList (row_twitter):
    maked_list = [row_twitter['text'], row_twitter['id_str'],row_twitter['lat'],row_twitter['long'],row_twitter['created_at']]
    return maked_list

#Função responsável por agrupar os tweets em grupos referentes a um incident e este associado a uma label do tipo incidente
def spatiotemporal_cluster(df_twitter,df_incident, radius, save_to,processNumber):  # Recebo como entrada o twiiterdata e o incident data

    creatingFolders(save_to)
 
    listOfRows = []

    twitter_data_size = df_twitter.index[-1]
    for (index_twitter, row_twitter) in df_twitter.iterrows():    #Percorro todos os tweets para clusterizá-los
        #print(str((index_twitter/ twitter_data_size)*100) + '%\n')
        #print(str(index_twitter) +" of " + str(twitter_data_size) +'\n')
        print("Process " + str(processNumber) + " Progress: "+str(round(((index_twitter/twitter_data_size)*100),ndigits=2)) +'%\n')
        atual_id_incident = 0
        atual_incident_type = None
        atual_distance = 100000000  # algum valor acima do raio
        twiiter_coordinates = [row_twitter['lat'], row_twitter['long']]
        for (index_incident, row_incident) in df_incident.iterrows():
            if((row_twitter['section'] == row_incident['section']) or (row_twitter['section'] == row_incident['section'] -1) or (row_twitter['section'] == row_incident['section'] +1)):    #condição de secao
                # pergunto se está no espaço
                if(temporal_filter(row_incident['incidentStartTime'],row_incident['incidentEndTime'] ,row_twitter['created_at'])):
                    incident_coordinates = [row_incident['incidentLat_From'], row_incident['incidentLong_From']]
                    if (spatio_filter(twiiter_coordinates,incident_coordinates, atual_distance, radius)):
                        # atualizar a 'distance' e atualizar o 'atual_incident_id'
                        atual_id_incident = row_incident['incidentID']
                        atual_distance = inside_ray(twiiter_coordinates, incident_coordinates, radius)
                        atual_incident_type = row_incident['incidentType']
                        #primeiro eu testo se o tweet é ou não um incident

        #depois que saiu do incident
        if (not(atual_distance == 100000000) and not(atual_id_incident == 0)):# ele é um incident
            lista = makeList(row_twitter)
            lista.append(atual_id_incident)
            if(atual_incident_type != 'NOT_INCIDENT'):
                lista.append(atual_incident_type) #incidentType
                lista.append('INCIDENT') #incident
            else:
                lista.append('NULL') #incidentType
                lista.append('NOT_INCIDENT') #incident
            lista.append(atual_distance.km)
            listOfRows.append(lista)

            #escrevelinha.writerow(lista)
            #print(lista)
            #df1 = pd.DataFrame([lista], columns=COLUMNS_NAMES)
            #fusioned_dataframe = fusioned_dataframe.append(df1)
        else: # não é um incident, incident Id = 'None' e incident = 0
            lista = makeList(row_twitter)
            lista.append('NULL') #incidentID
            lista.append('NULL') #incidentType
            lista.append('UNKNOW') #incident
            lista.append(atual_distance)
            listOfRows.append(lista)

            #print(lista)
            #df1 = pd.DataFrame([lista], columns=COLUMNS_NAMES)
            #escrevelinha.writerow(lista)
            # É salvo linha por linha dentro do CSV
            #fusioned_dataframe = fusioned_dataframe.append(df1)

    # with open(save_to + '/twitter_incident(' + str(threadNumber) + ').csv', 'w') as csvfile:
    #     escrevelinha = csv.writer(csvfile, delimiter=',', quotechar='"', quoting=csv.QUOTE_ALL)
    #     escrevelinha.writerow(COLUMNS_NAMES)  # Escrevo a linha referente as variáveis do meu CSV
    #     escrevelinha.writerow(listOfRows)
    df = pd.DataFrame(listOfRows, columns=COLUMNS_NAMES)
    df.to_csv(save_to + '/twitter_incident(process' + str(processNumber) + ').csv', index=False)


def fixBoundboxTwitter(twitter_data,bound_boxes, section):    #Verifica se os tweets estão dentro do bounding box especificado na coleta
    #Seleciono os dataframes que estão dentro do meu bounding box
    twitter_data = twitter_data[twitter_data['long'] > bound_boxes[0]]
    twitter_data = twitter_data[twitter_data['long'] < bound_boxes[2]]
    twitter_data = twitter_data[twitter_data['lat'] > bound_boxes[1]]
    twitter_data = twitter_data[twitter_data['lat'] < bound_boxes[3]]
    twitter_data['section'] = section
    return twitter_data


def fixBoundboxIncident(incident_data,bound_boxes, section):    #Verifica se os tweets estão dentro do bounding box especificado na coleta
    #Seleciono os dataframes que estão dentro do meu bounding box
    incident_data = incident_data[incident_data['incidentLong_From'] > bound_boxes[0]]
    incident_data = incident_data[incident_data['incidentLong_From'] < bound_boxes[2]]
    incident_data = incident_data[incident_data['incidentLat_From'] > bound_boxes[1]]
    incident_data = incident_data[incident_data['incidentLat_From'] < bound_boxes[3]]
    incident_data['section'] = section
    return incident_data


def concatIncidentandNotIncident(incident_data_folder,incident_col_name):

    incident = pd.DataFrame(columns=incident_col_name)
    #read incident
    for dirName, subdirList, fileList in os.walk(incident_data_folder, topdown=True):
        print('Found directory: %s' % dirName)
        for fname in fileList:
            print(fname)
            incident_data = pd.read_csv(dirName + fname, parse_dates=['incidentStartTime', 'incidentEndTime'],dtype={'incidentID': str})
            incident_data = incident_data[INCIDENT_COL_NAMES]
            # incident = [incident, incident_data]
            # incident = pd.concat(incident)
            incident = incident.append(incident_data)

    incident.reset_index(drop=True, inplace=True)
    return incident


def boundingBoxePartition(bound_boxes, x): #x e o numero de pedaços que irei cortar meu bound
    gap = (bound_boxes[3] - bound_boxes[1])/x
    bounding_list = []
    for i in range(x):
        #increment = gap*i
        bounding_list.append([bound_boxes[0], bound_boxes[1] + (gap *i), bound_boxes[2], bound_boxes[1] + gap * (i+1)])

    return bounding_list


def filterAndLabelSection(incident_data,twitter_data, save_to,bound_boxes, x):

    bounding_list = boundingBoxePartition(bound_boxes, x)


    df_twitter = pd.DataFrame(columns=list(twitter_data.columns))
    df_incident = pd.DataFrame(columns=list(twitter_data.columns))

    for i in range(x):  #atribui secao aos meus tweets
        result = fixBoundboxTwitter(twitter_data,bounding_list[i],i)
        df_twitter = [df_twitter, result]
        df_twitter = pd.concat(df_twitter)

        df_twitter.reset_index(drop=True, inplace=True) #reseto meu index

    df_incident = pd.DataFrame(columns=list(incident_data.columns))

    for i in range(x):  #atribui secao aos meus incidents
        result = fixBoundboxIncident(incident_data,bounding_list[i],i)
        df_incident = [df_incident, result]
        df_incident = pd.concat(df_incident)

    #df_incident = df_incident[df_incident['incidentType'] != 'CONSTRUCTION']
    df_incident.reset_index(drop=True, inplace=True)

    creatingFolders(save_to)
    df_twitter.to_csv(save_to +'twitter_boundbox_filtered.csv', index=False)
    df_incident.to_csv(save_to +'incident_boundbox_filtered.csv', index = False)

    #spatiotemporal_cluster(df_twitter, df_incident, radius, save_to)  #passo o dataframe com as secoes definidas anteriormente


def multiprocessed(df_twitter,df_incident,threads,rad):

    df_t = []
    df_i = []

    df_t.append(df_twitter[(df_twitter['section'] >= 0) & (df_twitter['section'] <= 2)])
    df_t.append(df_twitter[(df_twitter['section'] >= 2) & (df_twitter['section'] <= 3)])
    df_t.append(df_twitter[(df_twitter['section'] >= 3) & (df_twitter['section'] <= 4)])
    df_t.append(df_twitter[(df_twitter['section'] >= 4) & (df_twitter['section'] <= 8)])
    df_t.append(df_twitter[(df_twitter['section'] >= 8) & (df_twitter['section'] <= 12)])
    df_t.append(df_twitter[(df_twitter['section'] >= 12) & (df_twitter['section'] <= 20)])
    df_t.append(df_twitter[(df_twitter['section'] >= 20) & (df_twitter['section'] <= 36)])

    df_i.append(df_incident[(df_incident['section'] >= 0) & (df_incident['section'] <= 2)])
    df_i.append(df_incident[(df_incident['section'] >= 2) & (df_incident['section'] <= 3)])
    df_i.append(df_incident[(df_incident['section'] >= 3) & (df_incident['section'] <= 4)])
    df_i.append(df_incident[(df_incident['section'] >= 4) & (df_incident['section'] <= 8)])
    df_i.append(df_incident[(df_incident['section'] >= 8) & (df_incident['section'] <= 12)])
    df_i.append(df_incident[(df_incident['section'] >= 12) & (df_incident['section'] <= 20)])
    df_i.append(df_incident[(df_incident['section'] >= 20) & (df_incident['section'] <= 36)])

    # df_t.append(df_twitter[(df_twitter['section'] >= 0) & (df_twitter['section'] <= 3)])
    # df_t.append(df_twitter[(df_twitter['section'] >= 3) & (df_twitter['section'] <= 4)])
    # df_t.append(df_twitter[(df_twitter['section'] >= 4) & (df_twitter['section'] <= 11)])
    # df_t.append(df_twitter[(df_twitter['section'] >= 11) & (df_twitter['section'] <= 36)])
    #
    #
    # df_i.append(df_incident[(df_incident['section'] >= 0) & (df_incident['section'] <= 3)])
    # df_i.append(df_incident[(df_incident['section'] >= 3) & (df_incident['section'] <= 4)])
    # df_i.append(df_incident[(df_incident['section'] >= 4) & (df_incident['section'] <= 11)])
    # df_i.append(df_incident[(df_incident['section'] >= 11) & (df_incident['section'] <= 36)])


    processes = []

    for i in range(0, threads):
        p = Process(target=spatiotemporal_cluster,args=(df_t[i], df_i[i], rad, save_to, i))
        processes.append(p)

    # Start the processes
    for p in processes:
        p.start()

    # Ensure all processes have finished execution
    for p in processes:
        p.join()


#   join CSV files, remove the individual ones and verify duplicated rows
def joinCSVRemoveDuplicated(FOLDER, RemoveOriginal,radius):
    for dirName, subdirList, fileList in os.walk(FOLDER):

        df_final = pd.DataFrame()
        print('Found directory: %s' % dirName)
        for fname in fileList:
            if (fname.find("twitter_incident(process") != -1):  # checking name 'twitter_incident'
                df_final = df_final.append(pd.read_csv(dirName + fname))
                if (RemoveOriginal == True):
                    os.remove(dirName + fname)

    #df_final = pd.read_csv(FOLDER + "twitter_incident(Radius" + str(radius) + ").csv", sep=",")

    mask = df_final.duplicated(subset=['id_str','tweet_incident_distance'], keep="first")  # mark duplicated index and distances rows

    df_final = df_final[~mask]  # remove duplicated rows

    mask = df_final.duplicated(subset=['id_str'], keep=False)  # mark duplicated index rows

    df_duplicated = df_final[mask]  # save duplicated rows with different distances
    df_final = df_final[~mask]  # remove duplicated rows from the final df

    df_duplicated_list = [] #list of unique rows

    if len(df_duplicated) != 0: # check if there are duplicated rows with different distances
        for index, row in df_duplicated.iterrows():

            if len(df_duplicated[df_duplicated['id_str'] == row['id_str']]['tweet_incident_distance']) != 0:

                smaller_distance = min(df_duplicated[df_duplicated['id_str'] == row['id_str']]['tweet_incident_distance'])  # save the min distance of duplicated index

                df_duplicated_list.append(df_duplicated[
                    (df_duplicated['id_str'] == row['id_str']) & (df_duplicated['tweet_incident_distance'] == smaller_distance)]) # save row with min distance
                #df_final_duplicated.append(df_duplicated[df_duplicated['tweet_incident_distance'] == smaller_distance])

                df_duplicated.drop(df_duplicated[df_duplicated['id_str'] == row['id_str']].index, inplace=True)  # remove index visited

        df_final = df_final.append(df_duplicated_list)

    df_final.to_csv(FOLDER + "twitter_incident(Radius" + str(radius) + ").csv", sep=",")    # save the final df with no duplicated rows

    print("Great, we joined the csv files into a one csv, removing duplicated rows.")


# =======================================================================================================================
#   INPUT DATA AND DATA SPLIT
# =======================================================================================================================


if __name__ == '__main__':

    COLUMNS_NAMES = ['text','id_str', 'lat','long', 'created_at', 'incident_id', 'incident_type', 'event','tweet_incident_distance']   #Nomes da coluna do CSV que será gerado

    INCIDENT_COL_NAMES = ['incidentID', 'incidentType', 'incidentStartTime', 'incidentEndTime', 'incidentLat_From',
                          'incidentLong_From']

    bound_boxes = [-74.051092, 40.698372, -73.866384, 40.867347]  # bounding box da regiao de coleta dos tweets
    save_to = 'data/data_grouped/20_NOT_INCIDENT/' #pasta com os tweets agrupados
    radiusList = [0.5, 0.4, 0.3, 0.2, 0.1, 0.11, 0.12, 0.15, 0.05]  # raio de procura dos tweets em torno do incident em km
    #radiusList = [0.15]  # raio de procura dos tweets em torno do incident em km



    #CSV contendo os dados de incidents
    twitter_data = pd.read_csv('data/tweets_manhattan/tweets_gerados.csv', parse_dates=['created_at'])#CSV criado na etapa anterior contendo os tweets com seus lat-long

    incident_data = concatIncidentandNotIncident('data/incidents_manhattan/',INCIDENT_COL_NAMES)

    filterAndLabelSection(incident_data, twitter_data,save_to, bound_boxes, 36)  #Função de fusão espaço-temporal

    for radius in radiusList:

        df_twitter = pd.read_csv(save_to +
                                 'twitter_boundbox_filtered.csv')  # CSV criado na etapa anterior contendo os tweets com suas secoes
        df_incident = pd.read_csv(save_to +
                                 'incident_boundbox_filtered.csv')  # CSV criado na etapa anterior contendo os incidentes com suas secoes

        time1 = time.time()
        multiprocessed(df_twitter, df_incident, 7, radius)
        time2 = time.time()
        print('{:s} function took {:.2f} min'.format("multiprocessed", (time2 - time1)/60))



        joinCSVRemoveDuplicated(save_to, True,radius)

    # df_twitter_incident = pd.read_csv(save_to + 'twitter_incident(Radius0.5).csv')
    # df_twitter_incident.loc[df_twitter_incident['incident'] == str(0),'incident'] = 'NOT_INCIDENT'
    # df_twitter_incident.loc[df_twitter_incident['incident'] == str(1),'incident'] = 'INCIDENT'
    # df_twitter_incident.to_csv(save_to + "twitter_incident(Radius0.5).csv", sep=",")



