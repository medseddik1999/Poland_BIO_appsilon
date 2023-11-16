#pip install geopy
import pandas as pd 
from geopy.geocoders import Nominatim  
from tqdm import tqdm

geolocator = Nominatim(user_agent="geoapiExercises") 

data=pd.read_csv('Poland_data.csv')
data=data[['longitudeDecimal','latitudeDecimal']] 
data=data.drop_duplicates() 

 


locations=[] 
for index ,row in tqdm(data.iterrows()):
  location = geolocator.reverse(str(row['latitudeDecimal'])+","+str(row['longitudeDecimal'])) 
  locations.append(location.address)
  

