from selenium import webdriver
import time
from selenium.webdriver.common.keys import Keys
from datetime import datetime


what_game = input('What game are you recording: ')
num_vids = int(input('How many videos did you record today: '))
today = datetime.date(datetime.now())
card_date = today.strftime("%b%d")


#String For Loop
def card_info():
    alpha_conv = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']

    for i in range(num_vids):
        #alpha_conv[i]
        print(what_game, card_date + alpha_conv[i])
        global card_text
        card_text = what_game, card_date + alpha_conv[i]
        add_card()



#SELENIUM TRELLO ACCESS
PATH = "C:\Program Files (x86)\chromedriver.exe"
driver = webdriver.Chrome(PATH)
#Navigate to Trello Website
driver.get('https://trello.com/')
#select login button
login_button = driver.find_element_by_link_text('Log in')
login_button.click()
#Username
email_blank = driver.find_element_by_xpath('//*[@id="user"]')
email_blank.click()
email = 'your email here'
email_blank.send_keys(email)
time.sleep(3)
#Password
pwd_blank = driver.find_element_by_xpath('//*[@id="password"]')
pwd = 'your password here'
pwd_blank.click()
pwd_blank.send_keys(pwd)
login_button = driver.find_element_by_xpath('//*[@id="login"]')
login_button.click()
time.sleep(3)
#Home Page
dd_sidebar_button = driver.find_element_by_xpath('//*[@id="content"]/div/div[2]/div/div/div/div/div[1]/nav/div[2]/div/ul/li[2]/a/span[1]')
dd_sidebar_button.click()
boards_button = driver.find_element_by_xpath('//*[@id="content"]/div/div[2]/div/div/div/div/div[1]/nav/div[2]/div/ul/li[2]/ul/li[1]/a/span[2]')
boards_button.click()
#Second Home Page - Inside Dork Duo - Board Selection
time.sleep(3)
youtube_board = driver.find_element_by_link_text('YouTube')
youtube_board.click()
time.sleep(3)
#from add_card Function
add_card_fst_button = driver.find_element_by_xpath('//*[@id="board"]/div[4]/div/div[3]/a/span[3]')
add_card_fst_button.click()
#Add Card Function
def add_card():
    time.sleep(3)
    card_title = driver.find_element_by_xpath('//*[@id="board"]/div[4]/div/div[2]/div/div[1]/div/textarea')
    card_title.click()
    card_title.send_keys(card_text)
    add_card_scd_button = driver.find_element_by_xpath('//*[@id="board"]/div[4]/div/div[2]/div/div[2]/div[1]/input')
    add_card_scd_button.click()


card_info()
