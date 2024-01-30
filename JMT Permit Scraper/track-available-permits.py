import requests
from bs4 import BeautifulSoup

def scrape_recreation_info(url):
    # Send a GET request to the URL
    response = requests.get(url)

    # Check if the request was successful
    if response.status_code == 200:
        # Parse the HTML content using BeautifulSoup
        soup = BeautifulSoup(response.content, 'html.parser')

        # Now, you can inspect the HTML structure of the page and extract the information you need using BeautifulSoup's find and find_all methods.

        # Example:
        # Find all elements with a specific class (use Developer Tools in your browser to find the class names)
        # data_elements = soup.find_all('div', class_='your-class-name')

        # Loop through the elements and extract relevant information
        # for element in data_elements:
        #     info = element.text.strip()
        #     print(info)

    else:
        print("Failed to fetch the page. Status Code:", response.status_code)

if __name__ == "__main__":
    url = "https://www.recreation.gov/permits/445859/registration/detailed-availability?type=overnight-permit&date=2023-07-25"
    scrape_recreation_info(url)