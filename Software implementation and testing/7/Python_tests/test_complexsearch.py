import pytest
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait

class TestComplexsearchUpdated():
    def setup_method(self, method):
        self.driver = webdriver.Chrome()
        self.vars = {}
    
    def teardown_method(self, method):
        self.driver.quit()
    
    def test_complexsearch_updated(self, step_tracker):
        # Step 1: Открытие главной страницы
        step_tracker.step(lambda: self.driver.get("https://learn.microsoft.com/ru-ru/"))
        
        # Step 2: Клик по кнопке поиска для активации поля
        step_tracker.step(lambda: WebDriverWait(self.driver, 10).until(
            EC.element_to_be_clickable((By.CSS_SELECTOR, ".margin-left-xxs > .button .docon"))).click())
        
        # Step 3: Ввод запроса "Python" в поле поиска
        step_tracker.step(lambda: [WebDriverWait(self.driver, 10).until(
            EC.element_to_be_clickable((By.ID, "site-header-search-autocomplete-input"))).send_keys("python"),
            WebDriverWait(self.driver, 10).until(
            EC.element_to_be_clickable((By.ID, "site-header-search-autocomplete-input"))).submit()])
        
        # Step 4: Переход по первой ссылке в результатах
        step_tracker.step(lambda: WebDriverWait(self.driver, 10).until(
            EC.element_to_be_clickable((By.LINK_TEXT, "Руководство по Python в Visual Studio 1. Создание проекта"))).click())
        
        def check_element():
            header = WebDriverWait(self.driver, 10).until(
                EC.presence_of_element_located((By.ID, "tutorial-work-with-python-in-visual-studio"))
            )
            assert header.is_displayed(), "Уникальный элемент страницы (id=tutorial-work-with-python-in-visual-studio) не найден"
            return header
        # Step 5: Проверка уникального элемента на странице
        step_tracker.step(check_element)