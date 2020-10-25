import requests
from requests.models import Response

get_countries_url = "https://api.openaq.org/v1/countries"
get_cities_url = "https://api.openaq.org/v1/cities"
get_measurement_url = "https://api.openaq.org/v1/measurements"
parameter = {
    "country": "United States",
    "city": "Denver",
    "parameter[]": "pm25",
    "parameter[]": "no2",
    "parameter[]": "o3",
    "has_geo": "true",
    "date_from": "2018-06-01",
    "date_to": "2018-10-30",
    "format": "json"
}
cities_parameter = {
    "country": "United States"
    }

response = requests.get(get_countries_url)
# response = requests.get(get_cities_url, params=cities_parameter)
print(response.json())
