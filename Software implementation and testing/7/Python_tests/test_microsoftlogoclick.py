import pytest
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait

class TestMicrosoftlogoclick():
    def setup_method(self, method):
        self.driver = webdriver.Chrome()
        self.vars = {}
    
    def teardown_method(self, method):
        self.driver.quit()
    
    def test_microsoftlogoclick(self, step_tracker):
        # Step 1: Открытие сайта
        step_tracker.step(lambda: self.driver.get("https://learn.microsoft.com/ru-ru/"))
        
        # Step 2: Клик по логотипу Microsoft
        step_tracker.step(lambda: self.driver.find_element(By.CSS_SELECTOR, ".site-header-logo:nth-child(1) path:nth-child(1)").click())
        
        def check_logo():
            WebDriverWait(self.driver, 10).until(
                EC.presence_of_element_located((By.CSS_SELECTOR, ".c-image"))
            )
            elements = self.driver.find_elements(By.CSS_SELECTOR, ".c-image")
            assert len(elements) > 0, "Элемент с логотипом (.c-image) не найден на странице"
            return elements
        # Step 3: Проверка наличия элемента с логотипом
        step_tracker.step(check_logo)