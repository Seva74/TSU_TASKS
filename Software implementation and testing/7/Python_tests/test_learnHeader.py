import pytest
import time
import json
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities

class TestLearnHeader():
    def setup_method(self, method):
        self.driver = webdriver.Chrome()
        self.vars = {}
  
    def teardown_method(self, method):
        self.driver.quit()
  
    def test_learnHeader(self, step_tracker):
        # Step 1: Открытие сайта
        step_tracker.step(lambda: self.driver.get("https://learn.microsoft.com/ru-ru/"))
        time.sleep(2)  # Оставляем sleep вне шага
        
        def check_header():
            elements = self.driver.find_elements(By.CSS_SELECTOR, ".site-header-brand:nth-child(3) > span")
            assert len(elements) > 0, "Заголовок 'Learn' не найден"
            return elements
        # Step 2: Проверка заголовка Learn
        step_tracker.step(check_header)