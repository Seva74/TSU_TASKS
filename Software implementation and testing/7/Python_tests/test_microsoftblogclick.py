import pytest
import time
import json
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities

class TestMicrosoftblogclick():
    def setup_method(self, method):
        self.driver = webdriver.Chrome()
        self.vars = {}
  
    def teardown_method(self, method):
        self.driver.quit()
  
    def test_microsoftblogclick(self, step_tracker):
        # Step 1: Открытие сайта
        step_tracker.step(lambda: self.driver.get("https://learn.microsoft.com/ru-ru/"))
        
        # Step 2: Клик по кнопке "Блог"
        step_tracker.step(lambda: self.driver.find_element(By.LINK_TEXT, "Блог").click())
        
        def check_blog_header():
            WebDriverWait(self.driver, 10).until(
                EC.presence_of_element_located((By.CSS_SELECTOR, ".HeroBanner_lia-title__J66ah"))
            )
            elements = self.driver.find_elements(By.CSS_SELECTOR, ".HeroBanner_lia-title__J66ah")
            assert len(elements) > 0, "Заголовок блога не найден на странице"
            return elements
        # Step 3: Проверка наличия заголовка на странице блога
        step_tracker.step(check_blog_header)