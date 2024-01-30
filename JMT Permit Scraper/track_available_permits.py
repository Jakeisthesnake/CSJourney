from ctypes import sizeof
from selenium import webdriver
import time
from selenium.webdriver.support.ui import Select

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select
from selenium.webdriver.common.keys import Keys

# Path to the GeckoDriver executable
geckodriver_path = '/home/jake/Desktop/CSJourney/CSJourney/JMT Permit Scraper/geckodriver-v0.33.0-linux64/geckodriver'

# URL to the desired website
url = 'https://www.recreation.gov/permits/445859/registration/detailed-availability?type=overnight-permit&date=2023-07-25'

# Configure Firefox options (optional)
firefox_options = webdriver.FirefoxOptions()
# You can add more options as needed, e.g., to run the browser in headless mode:
# firefox_options.headless = True

# Create a Firefox Service object with the executable path
service = webdriver.firefox.service.Service(geckodriver_path)

# Launch Firefox using the service and options
driver = webdriver.Firefox(service=service, options=firefox_options)
try:
    
    
    # Open the URL
    driver.get(url)


    signin_button_element = driver.find_element(By.ID, "ga-global-nav-log-in-link")
    signin_button_element.click()
    

    email_field = driver.find_element(By.ID, 'email')
    email_field.clear()
    email_field.send_keys("jakeisthesnake@gmail.com")
    
    password_field = driver.find_element(By.ID, 'rec-acct-sign-in-password')
    password_field.clear()
    password_field.send_keys("Tsruhkrap1853!")
  

    login_button_element = driver.find_element(By.CSS_SELECTOR, "button.sarsa-button:nth-child(4)")
    login_button_element.click()
    
    # Add any further actions you want to perform on the page (e.g., clicking buttons, filling forms, etc.)
    # ...
    # Find the drop-down menu element by its ID, name, or other locator method
    time.sleep(1)
    

    # Get the current time
    current_time = datetime.now().time()

# Define the desired time (hour, minute, and second)
    desired_time = dtime(3, 19, 0)  # This example sets the desired time to 3:30 PM

# Calculate the time difference (in seconds) between current time and desired time
    time_difference = (datetime.combine(datetime.today(), desired_time) - datetime.combine(datetime.today(), current_time)).total_seconds()

# If the desired time is in the past for today, set it for tomorrow
    if time_difference < 0:
        time_difference += 86400  # Add 24 hours (86400 seconds) to set it for tomorrow

# Sleep until the desired time
    time.sleep(time_difference)




    guest_button = driver.find_element(By.ID, 'guest-counter')
    guest_button.click()
    input_field = driver.find_element(By.ID, 'guest-counter-number-field-People')
    input_field.send_keys(Keys.BACKSPACE)
    input_field.send_keys("2")
    input_field.send_keys(Keys.TAB)
    
    time.sleep(21)

    # Press the "Tab" key to move to the next input field
    calendar_button = driver.find_element(By.CLASS_NAME, 'SingleDatePickerInput_calendarIcon SingleDatePickerInput_calendarIcon_1')
    calendar_button.click()
    calendar_arrow_button = driver.find_element(By.CLASS_NAME, 'arsa-icon rec-icon-arrow-forward md')
    calendar_arrow_button.click()
    calendar_arrow_button = driver.find_element(By.CLASS_NAME, 'arsa-icon rec-icon-arrow-forward md')
    calendar_arrow_button.click()

    date_field = driver.find_element(By.ID, 'SingleDatePicker1')
    driver.execute_script("""
    var target = arguments[0];
    var clickEvent = new MouseEvent('click', {
        'bubbles': true,
        'cancelable': true,
        'button': 0,
        'detail': 3  // Simulate triple click (3 clicks in quick succession)
        });
        target.dispatchEvent(clickEvent);
        """, date_field)
    date_field.send_keys("8/4/23")




    elements = driver.find_elements(By.CLASS_NAME, "rec-grid-row")
    donohue_row = 0
    
    for element in elements:
        if (element.find_element(By.XPATH, "//*[contains(text(), 'Happy Isles->Past LYV (Donohue Pass Eligible)')]")):
            print((element.find_element(By.XPATH, "//*[contains(text(), 'Happy Isles->Past LYV (Donohue Pass Eligible)')]").text))
            donohue_row = element
            break

    print(donohue_row.get_attribute('class')) 
    print(donohue_row.tag_name)  
    #donohue_row_boxes = donohue_row.find_elements(By.XPATH, "//*[contains(text(), 'rec-grid-grid-cell available')]")
    #donohue_row_boxes = donohue_row.find_elements(By.CLASS_NAME, "rec-grid-grid-cell")
    donohue_row_boxes = donohue_row.find_elements(By.XPATH, './/*')
    #for element in donohue_row_boxes:
        #print(element.get_attribute('class')) 


    match_using_text = driver.find_element(By.XPATH, "//*[contains(text(), 'Donohue Pass Eligible')]")
    #print(match_using_text.text)
    time.sleep(50)

    happy_button_element = driver.find_element(By.XPATH, "/html/body/div[1]/div/div[4]/div[2]/div/div/div[1]/div[2]/div/div/div[3]/div[2]/div[19]/div[4]/div/button")
    happy_button_element.click()

    



    # Optionally, you can get the page source or take a screenshot for further analysis:
    # page_source = driver.page_source
    # driver.save_screenshot("screenshot.png")
    time.sleep(10)

finally:
    # Close the browser after you're done
    driver.quit()

    #print(elements)