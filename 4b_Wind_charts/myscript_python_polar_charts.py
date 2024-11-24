#!/usr/local/bin/python3
#pip3 install <package>
#
import pandas as pd
import seaborn as sb
import numpy as np
import matplotlib.pyplot as plt
import scipy
import csv
import string
import holidays 
from bdateutil import isbday
import plotly.graph_objects as go
from PIL import Image, ImageEnhance, ImageDraw, ImageFont
import plotly.express as px



Station_names=['Anzac','Athabasca','Berta','Buffalo','Conklin','Janvier','Lower_camp','Mannix','Mildred','Patricia','Stony_mountain']

df=pd.read_csv('averages_CH4.csv',low_memory=False)
df['angle_start'] = df['angle_start'].astype(float)
df['angle_end'] = df['angle_end'].astype(float)
df['angle']=np.where((df['angle_start']+df['angle_end'])/2 <=90, 90-(df['angle_start']+df['angle_end'])/2, 450-(df['angle_start']+df['angle_end'])/2)

seasons=['all_seasons','winter','no_winter']

######################################################
### Winters
######################################################


for i in range(len(Station_names)):
    df_station=df[df['station'] == Station_names[i]]
    df_station=df_station[df_station['season'] == seasons[1]]
    df_station=df_station[df_station['type'] == 'mean']
    df_station['value'] = df_station['value'].astype(float)

    r=df_station['value'].to_numpy()
    theta=df_station['angle'].to_numpy()
    width=[]
    marker_color=[]
    radius_mean=[]
    theta_mean=[]
    
    for j in range(len(df_station)-1):
        if (r[j+1] < r[0]-0.02): marker_color.append('#6666FF')
        if ((r[j+1] > r[0]-0.02) and (r[j+1] < r[0]+0.02)): marker_color.append('#9966FF')
        if (r[j+1] > r[0]+0.02): marker_color.append('#6600CC')
        width.append(20)

    fig = go.Figure(go.Barpolar(
        r=r[1:],
        theta=theta[1:],
        width=width,
        marker_color=marker_color,
#        marker_line_color="black",
#        marker_line_width=2,
        opacity=0.8
    ))

#for j in range(360):
#    radius_mean.append(r[0])
#    theta_mean.append(j)

#fig.add_trace(go.Scatterpolar(
#        r = radius_mean,
#        theta=theta_mean,
#        mode = 'lines',
#        line_color = 'blue'
#    ))

    if (Station_names[i] in set(['Lower_camp','Mannix','Mildred','Buffalo'])):

        fig.update_layout(
            template=None,
            width=1500, 
            height=1500,
            polar = dict(
                radialaxis = dict(range=[1.96, 2.5], showticklabels=True, dtick = 0.2, ticks='', visible=True, color="#000", gridcolor="#000", showline=False, tickfont_size=100),    #NOx [0,20]
                angularaxis = dict(showticklabels=False, ticks='', visible=False)
            )
        )

        fig.write_image(Station_names[i]+'_'+seasons[1]+'_CH4_wind_directions.png')


    if (Station_names[i] in set(['Athabasca','Berta','Conklin','Patricia'])):

        fig.update_layout(
            template=None,
            width=1500, 
            height=1500,
            polar = dict(
                radialaxis = dict(range=[1.96, 2.25], showticklabels=True, dtick = 0.1, ticks='', visible=True, color="#000", gridcolor="#000", showline=False, tickfont_size=100),    #NOx [0,20]
                angularaxis = dict(showticklabels=False, ticks='', visible=False)
            )
        )

        fig.write_image(Station_names[i]+'_'+seasons[1]+'_CH4_wind_directions.png')


    if (Station_names[i] in set(['Anzac','Janvier','Stony_mountain'])):

        fig.update_layout(
            template=None,
            width=1500, 
            height=1500,
            polar = dict(
                radialaxis = dict(range=[1.96, 2.07], showticklabels=True, dtick = 0.05, ticks='', visible=True, color="#000", gridcolor="#000", showline=False, tickfont_size=100),    #NOx [0,20]
                angularaxis = dict(showticklabels=False, ticks='', visible=False)
            )
        )

        fig.write_image(Station_names[i]+'_'+seasons[1]+'_CH4_wind_directions.png')




########################################
### white -> transparent
########################################

    threshold=100
    dist=20 

    img=Image.open(Station_names[i]+'_'+seasons[1]+'_CH4_wind_directions.png').convert('RGBA')
    # np.asarray(img) is read only. Wrap it in np.array to make it modifiable.
    arr=np.array(np.asarray(img))
    r,g,b,a=np.rollaxis(arr,axis=-1)
    mask=((r>threshold)
          & (g>threshold)
          & (b>threshold)
          & (np.abs(r-g)<dist)
          & (np.abs(r-b)<dist)
          & (np.abs(g-b)<dist)
          )
    mask2=((r<threshold)
          | (g<threshold)
          | (b<threshold)
          | (np.abs(r-g)>dist)
          | (np.abs(r-b)>dist)
          | (np.abs(g-b)>dist)
          )
    #print(arr)
    #arr[::0]=255
    arr[mask2,3]=160
    #print(arr[mask2,2])
    arr[mask,3]=0
    img=Image.fromarray(arr,mode='RGBA')
    img = img.resize((2500, 2500), Image.LANCZOS)
    #img.thumbnail([15000,15000], Image.Resampling.LANCZOS)
    img.save(Station_names[i]+'_'+seasons[1]+'_CH4_wind_directions.png')





######################################################
### No Winters
######################################################


for i in range(len(Station_names)):
    df_station=df[df['station'] == Station_names[i]]
    df_station=df_station[df_station['season'] == seasons[2]]
    df_station=df_station[df_station['type'] == 'mean']
    df_station['value'] = df_station['value'].astype(float)

    r=df_station['value'].to_numpy()
    theta=df_station['angle'].to_numpy()
    width=[]
    marker_color=[]
    radius_mean=[]
    theta_mean=[]
    
    for j in range(len(df_station)-1):
        if (r[j+1] < r[0]-0.02): marker_color.append('#FF6347')
        if ((r[j+1] > r[0]-0.02) and (r[j+1] < r[0]+0.02)): marker_color.append('#DC143C')
        if (r[j+1] > r[0]+0.02): marker_color.append('#800000')
        width.append(20)

    fig = go.Figure(go.Barpolar(
        r=r[1:],
        theta=theta[1:],
        width=width,
        marker_color=marker_color,
#        marker_line_color="black",
#        marker_line_width=2,
        opacity=0.8
    ))



    if (Station_names[i] in set(['Lower_camp','Mannix','Mildred','Buffalo'])):

        fig.update_layout(
            template=None,
            width=1500, 
            height=1500,
            polar = dict(
                radialaxis = dict(range=[1.96, 2.5], showticklabels=True, dtick = 0.2, ticks='', visible=True, color="#000", gridcolor="#000", showline=False, tickfont_size=100),    #NOx [0,20]
                angularaxis = dict(gridcolor="#000",showticklabels=False, ticks='', visible=False)
            )
        )

        fig.write_image(Station_names[i]+'_'+seasons[2]+'_CH4_wind_directions.png')


    if (Station_names[i] in set(['Athabasca','Berta','Conklin','Patricia'])):

        fig.update_layout(
            template=None,
            width=1500, 
            height=1500,
            polar = dict(
                radialaxis = dict(range=[1.96, 2.25], showticklabels=True, dtick = 0.1, ticks='', visible=True, color="#000", gridcolor="#000", showline=False, tickfont_size=100),    #NOx [0,20]
                angularaxis = dict(gridcolor="#000",showticklabels=False, ticks='', visible=False)
            )
        )

        fig.write_image(Station_names[i]+'_'+seasons[2]+'_CH4_wind_directions.png')


    if (Station_names[i] in set(['Anzac','Janvier','Stony_mountain'])):

        fig.update_layout(
            template=None,
            width=1500, 
            height=1500,
            polar = dict(
                radialaxis = dict(range=[1.96, 2.07], showticklabels=True, dtick = 0.05, ticks='', visible=True, color="#000", gridcolor="#000", showline=False, tickfont_size=100),    #NOx [0,20]
                angularaxis = dict(gridcolor="#000",showticklabels=False, ticks='', visible=False)
            )
        )

        fig.write_image(Station_names[i]+'_'+seasons[2]+'_CH4_wind_directions.png')


########################################
### white -> transparent
########################################

    threshold=100
    dist=20 

    img=Image.open(Station_names[i]+'_'+seasons[2]+'_CH4_wind_directions.png').convert('RGBA')
    # np.asarray(img) is read only. Wrap it in np.array to make it modifiable.
    arr=np.array(np.asarray(img))
    r,g,b,a=np.rollaxis(arr,axis=-1)
    mask=((r>threshold)
          & (g>threshold)
          & (b>threshold)
          & (np.abs(r-g)<dist)
          & (np.abs(r-b)<dist)
          & (np.abs(g-b)<dist)
          )
    mask2=((r<threshold)
          | (g<threshold)
          | (b<threshold)
          | (np.abs(r-g)>dist)
          | (np.abs(r-b)>dist)
          | (np.abs(g-b)>dist)
          )
    #print(arr)
    #arr[::0]=255
    arr[mask2,3]=160
    #print(arr[mask2,2])
    arr[mask,3]=0
    img=Image.fromarray(arr,mode='RGBA')
    img = img.resize((2500, 2500), Image.LANCZOS)
    #img.thumbnail([15000,15000], Image.Resampling.LANCZOS)
    img.save(Station_names[i]+'_'+seasons[2]+'_CH4_wind_directions.png')




###################################################
### Wind directions
###################################################


direction = ['North', 'NNE', 'NE', 'ENE', 'East', 'ESE', 'SE', 'SSE', 'South', 'SSW', 'SW', 'WSW', 'West', 'WNW', 'NW', 'NNW']
frequency = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

fig = go.Figure(go.Barpolar(
    r=frequency,
    theta=direction,
    width=width,
    marker_color=marker_color,
    #marker_line_color="black",
    #marker_line_width=2,
    opacity=0.8
    ))


fig.update_layout(
    margin_b=250,
    margin_t=250,
    margin_r=250,
    margin_l=250,

    template=None,
#    paper_bgcolor='rgba(255,0,0,0)',
#    plot_bgcolor='rgba(255,0,0,0)',
    width=1600, 
    height=1500,
    polar = dict(
    radialaxis = dict(range=[0,0.1], showticklabels=False, dtick = 0.1, ticks='', visible=False, color="#000", gridcolor="#000", showline=False, tickfont_size=50),    #NOx [0,20]
    angularaxis = dict(categoryorder="array", categoryarray=['North', 'NNE', 'NE', 'ENE', 'East', 'ESE', 'SE', 'SSE', 'South', 'SSW', 'SW', 'WSW', 'West', 'WNW', 'NW', 'NNW'], direction="clockwise", showticklabels=True, ticks='', visible=True, tickfont_size=100, color="#000")
#, dtick=45)
        )
    )

fig.write_image('wind_rose.png')


threshold=250
dist=0.5 

img=Image.open('wind_rose.png').convert('RGBA')
# np.asarray(img) is read only. Wrap it in np.array to make it modifiable.
arr=np.array(np.asarray(img))
r,g,b,a=np.rollaxis(arr,axis=-1)
mask=((r>threshold)
      & (g>threshold)
      & (b>threshold)
      & (np.abs(r-g)<dist)
      & (np.abs(r-b)<dist)
      & (np.abs(g-b)<dist)
      )
mask2=((r<threshold)
      | (g<threshold)
      | (b<threshold)
      | (np.abs(r-g)>dist)
      | (np.abs(r-b)>dist)
      | (np.abs(g-b)>dist)
          )

#arr[mask2,3]=160
arr[mask,3]=0
img=Image.fromarray(arr,mode='RGBA')
img = img.resize((1500, 1600), Image.LANCZOS)
#img.thumbnail([15000,15000], Image.Resampling.LANCZOS)
img.save('wind_rose.png')



###################################################
### Collage
###################################################

Station_names=['Buffalo','Mannix','Mildred','Lower_camp','Berta','Athabasca','Patricia','Anzac','Conklin','Janvier','Stony_mountain']

#new = Image.new("RGBA", (2400,3610))

img_1 = Image.open(Station_names[0]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_1 = img_1.resize((600,600))
img_2 = Image.open(Station_names[1]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_2 = img_2.resize((600,600))
img_3 = Image.open(Station_names[2]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_3 = img_3.resize((600,600))
img_4 = Image.open(Station_names[3]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_4 = img_4.resize((600,600))
img_5 = Image.open(Station_names[4]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_5 = img_5.resize((600,600))
img_6 = Image.open(Station_names[5]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_6 = img_6.resize((600,600))
img_7 = Image.open(Station_names[6]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_7 = img_7.resize((600,600))
img_8 = Image.open(Station_names[7]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_8 = img_8.resize((600,600))
img_9 = Image.open(Station_names[8]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_9 = img_9.resize((600,600))
img_10 = Image.open(Station_names[9]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_10 = img_10.resize((600,600))
img_11 = Image.open(Station_names[10]+"_"+seasons[1]+"_CH4_wind_directions.png")
img_11 = img_11.resize((600,600))
img_wind_rose = Image.open("wind_rose.png")
img_wind_rose = img_wind_rose.resize((400,427))


img_1_no_winter = Image.open(Station_names[0]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_1_no_winter = img_1_no_winter.resize((600,600))
img_2_no_winter = Image.open(Station_names[1]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_2_no_winter = img_2_no_winter.resize((600,600))
img_3_no_winter = Image.open(Station_names[2]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_3_no_winter = img_3_no_winter.resize((600,600))
img_4_no_winter = Image.open(Station_names[3]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_4_no_winter = img_4_no_winter.resize((600,600))
img_5_no_winter = Image.open(Station_names[4]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_5_no_winter = img_5_no_winter.resize((600,600))
img_6_no_winter = Image.open(Station_names[5]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_6_no_winter = img_6_no_winter.resize((600,600))
img_7_no_winter = Image.open(Station_names[6]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_7_no_winter = img_7_no_winter.resize((600,600))
img_8_no_winter = Image.open(Station_names[7]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_8_no_winter = img_8_no_winter.resize((600,600))
img_9_no_winter = Image.open(Station_names[8]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_9_no_winter = img_9_no_winter.resize((600,600))
img_10_no_winter = Image.open(Station_names[9]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_10_no_winter = img_10_no_winter.resize((600,600))
img_11_no_winter = Image.open(Station_names[10]+"_"+seasons[2]+"_CH4_wind_directions.png")
img_11_no_winter = img_11_no_winter.resize((600,600))




new = Image.new("RGBA", (2550,1220))

new.paste(img_8, (650,590))
new.paste(img_10, (1250,590))
new.paste(img_11, (1850,590))

new.paste(img_8_no_winter, (650,70))
new.paste(img_10_no_winter, (1250,70))
new.paste(img_11_no_winter, (1850,70))

new.paste(img_wind_rose, (120,300))

draw_text = ImageDraw.Draw(new)

font = ImageFont.truetype(font="~/Library/Fonts Disabled/Arial.ttf", size=100)

#draw_text.text((0,40),'A.',font=font, fill='#1C0606')

font = ImageFont.truetype(font="~/Library/Fonts Disabled/Arial.ttf", size=50)

draw_text.text((860,50),'Anzac',font=font, fill='#1C0606')
draw_text.text((1450,50),'Janvier',font=font, fill='#1C0606')
draw_text.text((1990,50),'Stony Mountain',font=font, fill='#1C0606')


new.save('Collage_Stations_A_CH4_wind_directions.png')

plt.close('all')





new = Image.new("RGBA", (2550,1220))

new.paste(img_5, (50,590))
new.paste(img_7, (650,590))
new.paste(img_6, (1250,590))
new.paste(img_9, (1850,590))

new.paste(img_5_no_winter, (50,70))
new.paste(img_7_no_winter, (650,70))
new.paste(img_6_no_winter, (1250,70))
new.paste(img_9_no_winter, (1850,70))


draw_text = ImageDraw.Draw(new)

font = ImageFont.truetype(font="~/Library/Fonts Disabled/Arial.ttf", size=100)

#draw_text.text((0,40),'B.',font=font, fill='#1C0606')

font = ImageFont.truetype(font="~/Library/Fonts Disabled/Arial.ttf", size=50)

draw_text.text((170,50),'Bertha Ganter',font=font, fill='#1C0606')
draw_text.text((780,50),'Patricia McInnes',font=font, fill='#1C0606')
draw_text.text((1370,50),'Athabasca Valley',font=font, fill='#1C0606')
draw_text.text((2020,70),'Conklin',font=font, fill='#1C0606')


new.save('Collage_Stations_B_CH4_wind_directions.png')

plt.close('all')



new = Image.new("RGBA", (2550,1220))

new.paste(img_1, (50,590))
new.paste(img_2, (650,590))
new.paste(img_3, (1250,590))
new.paste(img_4, (1850,590))

new.paste(img_1_no_winter, (50,70))
new.paste(img_2_no_winter, (650,70))
new.paste(img_3_no_winter, (1250,70))
new.paste(img_4_no_winter, (1850,70))


draw_text = ImageDraw.Draw(new)

font = ImageFont.truetype(font="~/Library/Fonts Disabled/Arial.ttf", size=100)

#draw_text.text((0,40),'C.',font=font, fill='#1C0606')

font = ImageFont.truetype(font="~/Library/Fonts Disabled/Arial.ttf", size=50)

draw_text.text((170,50),'Buffalo Viewpoint',font=font, fill='#1C0606')
draw_text.text((860,50),'Mannix',font=font, fill='#1C0606')
draw_text.text((1410,50),'Mildred Lake',font=font, fill='#1C0606')
draw_text.text((2010,50),'Lower Camp',font=font, fill='#1C0606')


new.save('Collage_Stations_C_CH4_wind_directions.png')

plt.close('all')

