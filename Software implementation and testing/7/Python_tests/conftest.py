import pytest
import inspect
from datetime import datetime

# Фикстура для отслеживания шагов и записи в файл
@pytest.fixture
def step_tracker():
    log_file = "test_results.log"
    
    class StepTracker:
        def step(self, action, step_name=None):
            # Если step_name не передан, берём его из комментария
            if step_name is None:
                frame = inspect.currentframe().f_back
                code = frame.f_code
                lines, line_num = inspect.getsourcelines(code)
                current_line = frame.f_lineno - code.co_firstlineno
                if current_line > 0 and lines[current_line - 1].strip().startswith("#"):
                    step_name = lines[current_line - 1].strip().lstrip("# ").strip()
                else:
                    step_name = f"Unnamed step at line {frame.f_lineno}"
            
            try:
                result = action()
                with open(log_file, "a", encoding="utf-8") as f:
                    f.write(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] ✅ {step_name} - пройден\n")
                return result
            except Exception as e:
                with open(log_file, "a", encoding="utf-8") as f:
                    f.write(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] ❌ {step_name} - не пройден (ошибка: {e})\n")
                raise
    
    return StepTracker()

# Хук для записи общего результата теста
def pytest_runtest_makereport(item, call):
    log_file = "test_results.log"
    if call.when == "call":
        test_name = item.nodeid.split("::")[-1]
        with open(log_file, "a", encoding="utf-8") as f:
            if call.excinfo is None:
                f.write(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] Тест «{test_name}» - пройден\n\n")
            else:
                f.write(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] Тест «{test_name}» - не пройден (ошибка: {call.excinfo.value})\n\n")