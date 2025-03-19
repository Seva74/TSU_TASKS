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

class TestFooter():
    def setup_method(self, method):
        self.driver = webdriver.Chrome()  # создание экземпляра веб-драйвера
        self.vars = {}
  
    def teardown_method(self, method):
        self.driver.quit()  # завершение работы веб-драйвера
  
    def test_footer(self, step_tracker):
        # Step 1: Открытие сайта
        step_tracker.step(lambda: self.driver.get("https://learn.microsoft.com/ru-ru/"))
        
        def check_footer():
            elements = self.driver.find_elements(By.ID, "footer")
            assert len(elements) > 0, "Футер не найден"
            return elements
        # Step 2: Проверка футера
        step_tracker.step(check_footer)