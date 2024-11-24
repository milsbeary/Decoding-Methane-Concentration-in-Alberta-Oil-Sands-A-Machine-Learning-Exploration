#!/usr/local/bin/python3
#pip3 install <package>
#
import pandas as pd
import seaborn as sns
import numpy as np
import matplotlib.pyplot as plt

params = {'axes.labelsize': 40,
          'axes.titlesize': 20}
plt.rcParams.update(params)

import scipy
import csv
import string
import sklearn
import shap
#import warnings
#warnings.filterwarnings('ignore')

from sklearn.preprocessing import LabelEncoder, StandardScaler
from sklearn.impute import KNNImputer
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from sklearn.metrics import f1_score, mean_squared_error, r2_score, accuracy_score
from sklearn.ensemble import RandomForestRegressor

from sklearn.model_selection import train_test_split
from sklearn import model_selection
from sklearn.model_selection import KFold, cross_val_score

#from sklearn.metrics import classification_report 
#from sklearn.ensemble import RandomForestClassifier 
from sklearn.model_selection import RandomizedSearchCV #GridSearchCV

from PIL import Image, ImageEnhance, ImageDraw, ImageFont



File_names=['Anzac_data_all.csv','Athabasca_data_all.csv','Berta_data_all.csv','Buffalo_data_all.csv','Conklin_data_all.csv','Janvier_data_all.csv','Lower_camp_data_all.csv','Mannix_data_all.csv','Mildred_data_all.csv','Patricia_data_all.csv','Stony_mountain_data_all.csv']

Station_names=['Anzac','Athabasca','Berta','Buffalo','Conklin','Janvier','Lower_camp','Mannix','Mildred','Patricia','Stony_mountain']

###########################################################################
### Pure parameters from Miles without NOx and O3 but with WD and WS
###########################################################################

Parameters=[['CH4 (ppm)','AT (C)','TRS (ppb)','WS (km/h)','WD (deg)','PAR_16 (umol m-2 s-1)','RH (%)','SO2 (ppb)','PM2.5 (ug/m3)','BP_2 (mB)'],['CH4 (ppm)','WS (km/h)','WD (deg)','SR*O3','TRS (ppb)','AT (C)','PM2.5 (ug/m3)','SO2 (ppb)','WD_STD (deg)','BP_2 (mB)'],['CH4 (ppm)','WS (km/h)','WD (deg)','CO2 (ppm)','AT (C)','SR_29 (W/m2)','VW20 (km/h)','PM2.5 (ug/m3)','SO2 (ppb)','WD_STD (deg)'],['CH4 (ppm)','WS (km/h)','WD (deg)','H2S (ppb)','AT (C)','SR*O3','PM2.5 (ug/m3)','SO2 (ppb)','WD_STD (deg)'],['CH4 (ppm)','WS (km/h)','PAR_16 (umol m-2 s-1)','PM2.5 (ug/m3)','AT (C)','TRS (ppb)','VW20 (km/h)','WD (deg)'],['CH4 (ppm)','WS (km/h)','WD (deg)','TRS (ppb)','AT (C)','PAR_16 (umol m-2 s-1)','PM2.5 (ug/m3)','SO2 (ppb)','VW20 (km/h)'],['CH4 (ppm)','RH (%)','PAR_16 (umol m-2 s-1)','WD_STD (deg)','AT (C)','diluent','SO2 (ppb)','BP_2 (mB)','VW20 (km/h)','WD (deg)','WS (km/h)'],['CH4 (ppm)','AT (C)','RH (%)','PAR_16 (umol m-2 s-1)','WD (deg)','WD_STD (deg)','SKY_COVER_SUM_STATE','diluent','SO2 (ppb)','BP_2 (mB)','WS (km/h)'],['CH4 (ppm)','H2S (ppb)','PAR_16 (umol m-2 s-1)','RH (%)','AT (C)','diluent','VW20 (km/h)','WD (deg)','BP_2 (mB)','SKY_COVER_SUM_STATE','WS (km/h)'],['CH4 (ppm)','WS (km/h)','WD (deg)','RH (%)','SR*O3','AT (C)','SO2 (ppb)','PM2.5 (ug/m3)','VW20 (km/h)','WD_STD (deg)'],['CH4 (ppm)','WS (km/h)','WD (deg)','CO2 (ppm)','TRS (ppb)','PM2.5 (ug/m3)','AT (C)','PAR_16 (umol m-2 s-1)','VW20 (km/h)']]


####################################
### Pure parameters from Miles
####################################
#Parameters=[['CH4 (ppm)','NOX (ppb)','O3 (ppb)','AT (C)','TRS (ppb)','WS_STD (km/h)','PAR_16 (umol m-2 s-1)','RH (%)','SO2 (ppb)','PM2.5 (ug/m3)','BP_2 (mB)'],['CH4 (ppm)','NOX (ppb)','O3 (ppb)','VW20_STD (km/h)','SR*O3','TRS (ppb)','AT (C)','PM2.5 (ug/m3)','SO2 (ppb)','WD_STD (deg)','BP_2 (mB)'],['CH4 (ppm)','NOX (ppb)','O3 (ppb)','CO2 (ppm)','AT (C)','WS_STD (km/h)','SR_29 (W/m2)','VW20 (km/h)','PM2.5 (ug/m3)','SO2 (ppb)','WD_STD (deg)'],['CH4 (ppm)','NOX (ppb)','O3 (ppb)','H2S (ppb)','VW20_STD (km/h)','AT (C)','SR*O3','PM2.5 (ug/m3)','WD (deg)','SO2 (ppb)','WD_STD (deg)'],['CH4 (ppm)','O3 (ppb)','WS_STD (km/h)','PAR_16 (umol m-2 s-1)','PM2.5 (ug/m3)','NOX (ppb)','VW20_STD (km/h)','AT (C)','TRS (ppb)','VW20 (km/h)','WD (deg)'],['CH4 (ppm)','NOX (ppb)','O3 (ppb)','TRS (ppb)','AT (C)','PAR_16 (umol m-2 s-1)','WS_STD (km/h)','VW20_STD (km/h)','PM2.5 (ug/m3)','SO2 (ppb)','VW20 (km/h)'],['CH4 (ppm)','VW20_STD (km/h)','RH (%)','PAR_16 (umol m-2 s-1)','WD_STD (deg)','AT (C)','diluent','SO2 (ppb)','BP_2 (mB)','VW20 (km/h)','WD (deg)'],['CH4 (ppm)','VW20_STD (km/h)','AT (C)','RH (%)','PAR_16 (umol m-2 s-1)','WD (deg)','WD_STD (deg)','SKY_COVER_SUM_STATE','diluent','SO2 (ppb)','BP_2 (mB)'],['CH4 (ppm)','VW20_STD (km/h)','H2S (ppb)','PAR_16 (umol m-2 s-1)','RH (%)','AT (C)','diluent','VW20 (km/h)','WD (deg)','BP_2 (mB)','SKY_COVER_SUM_STATE'],['CH4 (ppm)','NOX (ppb)','O3 (ppb)','RH (%)','WS_STD (km/h)','SR*O3','AT (C)','SO2 (ppb)','PM2.5 (ug/m3)','VW20 (km/h)','WD_STD (deg)'],['CH4 (ppm)','NOX (ppb)','CO2 (ppm)','TRS (ppb)','PM2.5 (ug/m3)','O3 (ppb)','AT (C)','PAR_16 (umol m-2 s-1)','WS (km/h)','VW20 (km/h)','VW20_STD (km/h)']]


for i in range(len(Station_names)):

    print(File_names[i])
    print(Station_names[i])


    ##############################
    # Shapley values
    ##############################

    X_test = pd.read_csv('NEW_File'+Station_names[i]+'X_test_WD_WS_Without_STD.csv', low_memory=False)
    X_test.drop(columns=X_test.columns[0], axis=1, inplace=True)

    f = open('NEW_File'+Station_names[i]+'SHAP_values_WD_WS_Without_STD.csv', "r") 
    text = f.read() 
    f.close()
    shap_values = text.split(",")

    if Station_names[i] in set(['Anzac','Athabasca','Berta','Patricia']):
        reshape_parameter=9
    if Station_names[i] in set(['Buffalo','Janvier','Stony_mountain']):
        reshape_parameter=8
    if Station_names[i] in set(['Conklin']):
        reshape_parameter=7
    if Station_names[i] in set(['Lower_camp','Mannix','Mildred']):
        reshape_parameter=10

    shap_values=np.array(np.array(shap_values).reshape(-1, reshape_parameter),dtype=float)



#    plt.title(Station_names[i],fontsize=20)
    shap.summary_plot(shap_values, X_test,show=False)

    ###############
    #x and y axes
    ###############
    fig, ax = plt.gcf(), plt.gca()
    #plt.xlim(0,10)
    #plt.ylim(0,2)
    #ax.set_xlabel('Time',fontsize=100)
    ax.tick_params(axis='y', labelsize=20)
    ax.tick_params(axis='x', labelsize=20)
    ax.xaxis.label.set_size(20)

    ###############
    #color_bar
    ###############
    fig.axes[-1].tick_params(axis='y', labelsize=20)
    fig.axes[-1].labelsize=20
    fig.axes[-1].yaxis.label.set_size(20)
    #fig.axes[-1].set_ylabel('Name',fontsize=100)
    #fig.axes[-1].set_ylim([10,20])


    plt.savefig('TEST_SHAP_'+Station_names[i]+'_Without_STD_WD_WS',bbox_inches="tight")
    
    plt.clf()

    plt.title(Station_names[i])
    shap.dependence_plot("WD (deg)", shap_values, X_test, interaction_index="WS (km/h)",show=False)



    font_size=30
    ###############
    #x and y axes
    ###############
    fig, ax = plt.gcf(), plt.gca()
    #plt.xlim(0,10)
    #plt.ylim(0,2)
    #ax.set_xlabel('Time',fontsize=100)
    ax.tick_params(axis='y', labelsize=font_size)
    ax.tick_params(axis='x', labelsize=font_size)
    ax.xaxis.label.set_size(font_size)
    ax.yaxis.label.set_size(font_size)

    ###############
    #color_bar
    ###############
    fig.axes[-1].tick_params(axis='y', labelsize=font_size)
    fig.axes[-1].labelsize=font_size
    fig.axes[-1].yaxis.label.set_size(font_size)
    #fig.axes[-1].set_ylabel('Name',fontsize=100)
    #fig.axes[-1].set_ylim([10,20])

    plt.savefig('TEST_SHAP_'+Station_names[i]+'_WD_WS_Without_STD_WD_WS',bbox_inches="tight")
    plt.clf()


Chart_names=['', '_WD_WS']


new = Image.new("RGBA", (2400,2000))

for i in range(len(Chart_names)):
    img_1 = Image.open("TEST_SHAP_"+Station_names[0]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_1 = img_1.resize((600,500))
    img_2 = Image.open("TEST_SHAP_"+Station_names[1]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_2 = img_2.resize((600,500))
    img_3 = Image.open("TEST_SHAP_"+Station_names[2]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_3 = img_3.resize((600,500))
    img_4 = Image.open("TEST_SHAP_"+Station_names[3]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_4 = img_4.resize((600,500))
    img_5 = Image.open("TEST_SHAP_"+Station_names[4]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_5 = img_5.resize((600,500))
    img_6 = Image.open("TEST_SHAP_"+Station_names[5]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_6 = img_6.resize((600,500))
    img_7 = Image.open("TEST_SHAP_"+Station_names[6]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_7 = img_7.resize((600,500))
    img_8 = Image.open("TEST_SHAP_"+Station_names[7]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_8 = img_8.resize((600,500))
    img_9 = Image.open("TEST_SHAP_"+Station_names[8]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_9 = img_9.resize((600,500))
    img_10 = Image.open("TEST_SHAP_"+Station_names[9]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_10 = img_10.resize((600,500))
    img_11 = Image.open("TEST_SHAP_"+Station_names[10]+Chart_names[i]+'_Without_STD_WD_WS'+".png")
    img_11 = img_11.resize((600,500))


    new.paste(img_1, (0,180))
    new.paste(img_2, (600,180))
    new.paste(img_3, (1200,180))
    new.paste(img_4, (1800,180))

    new.paste(img_5, (0,770))

    new.paste(img_6, (1200,770))
    new.paste(img_7, (1800,770))

    new.paste(img_8, (0,1360))
    new.paste(img_9, (600,1360))
    new.paste(img_10, (1200,1360))
    new.paste(img_11, (1800,1360))



    draw_text = ImageDraw.Draw(new)

    font = ImageFont.truetype(font="~/Library/Fonts Disabled/Arial.ttf", size=50)
    draw_text.text((100,110),'Buffalo Viewpoint',font=font, fill='#1C0606')
    draw_text.text((700,110),'Mannix',font=font, fill='#1C0606')
    draw_text.text((1300,110),'Mildred Lake',font=font, fill='#1C0606')
    draw_text.text((1900,110),'Lower Camp',font=font, fill='#1C0606')

    draw_text.text((100,700),'Bertha Ganter - Fort McKay',font=font, fill='#1C0606')
    draw_text.text((1300,700),'Athabasca Valley',font=font, fill='#1C0606')
    draw_text.text((1900,700),'Patricia McInnes',font=font, fill='#1C0606')

    draw_text.text((100,1290),'Anzac',font=font, fill='#1C0606')
    draw_text.text((700,1290),'Conklin',font=font, fill='#1C0606')
    draw_text.text((1300,1290),'Janvier',font=font, fill='#1C0606')
    draw_text.text((1900,1290),'Stony Mountain',font=font, fill='#1C0606')


#    new.save('test.png')
    new.save('Test_Collage_SHAP_'+Chart_names[i]+'.png')

#    plt.title(Chart_names[i])
#    plt.savefig('Collage_SHAP_'+Chart_names[i],bbox_inches="tight")
#    plt.clf()




plt.close('all')

