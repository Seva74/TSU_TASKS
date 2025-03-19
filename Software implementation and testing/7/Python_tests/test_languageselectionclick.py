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

class TestLanguageselectionclick():
    def setup_method(self, method):
        self.driver = webdriver.Chrome()
        self.vars = {}
  
    def teardown_method(self, method):
        self.driver.quit()
  
    def test_languageselectionclick(self, step_tracker):
        # Step 1: Открытие сайта
        step_tracker.step(lambda: self.driver.get("https://learn.microsoft.com/ru-ru/"))
        
        # Step 2: Клик по меню выбора языка
        step_tracker.step(lambda: self.driver.find_element(By.CSS_SELECTOR, "#footer .local-selector-link-text").click())
        
        def check_language_selector():
            elements = self.driver.find_elements(By.ID, "select-a-language")
            assert len(elements) > 0, "Заголовок 'Выбор языка' не найден"
            return elements
        # Step 3: Проверка заголовка "Выбор языка"
        step_tracker.step(check_language_selector)