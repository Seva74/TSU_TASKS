import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats

loc = 1.5    # Теоретическое мат. ожидание
scale = 4.0  # Теоретическое СКО
alf = 0.05   # Уровень значимости

size_m = [100, 500, 1000]
mean_m = []
intervalLeft_m = []        # Левая граница при известной дисперсии
intervalRight_m = []       # Правая граница при известной дисперсии
intervalWVLeft_m = []      # Левая граница при неизвестной дисперсии
intervalWVRight_m = []     # Правая граница при неизвестной дисперсии

for size in size_m:
    # Генерация выборки
    data = np.random.normal(loc, scale, size)
    print(f"РАЗМЕР ВЫБОРКИ: {size}")

    # Построение графиков
    fig, axes = plt.subplots(1, 2, figsize=(10, 4))
    fig.suptitle(f'Нормальное распределение (N={size}, μ={loc}, σ={scale})')

    # Квантильный график (QQ-plot)
    stats.probplot(data, dist="norm", plot=axes[0])
    axes[0].set_title('QQ-plot')

    # Гистограмма
    axes[1].hist(data, bins=10, edgecolor='black', density=True)
    axes[1].set_title('Гистограмма')

    # Числовые характеристики
    mean = np.mean(data)
    variance = np.var(data)
    standartDeviation = np.std(data)

    print("Мат. ожидание: ", mean)
    print("Дисперсия: ", variance)
    print("СКО: ", standartDeviation)

    # Доверительный интервал при известной дисперсии (sigma = scale)
    z = stats.norm.ppf(1 - alf / 2)  # Критическое значение для нормального распределения
    se_known = scale / np.sqrt(size)  # Стандартная ошибка при известной sigma
    interval_left = mean - z * se_known
    interval_right = mean + z * se_known
    print(f"Доверительный интервал (известная дисперсия): ({interval_left:.4f}, {interval_right:.4f})")

    # Доверительный интервал при неизвестной дисперсии (t-распределение)
    interval_without_var = stats.t.interval(1 - alf, size - 1, mean, stats.sem(data))
    print(f"Доверительный интервал (неизвестная дисперсия): {interval_without_var}")

    print(" ")

    # Сохранение результатов для графика зависимости
    mean_m.append(mean)
    intervalLeft_m.append(interval_left)
    intervalRight_m.append(interval_right)
    intervalWVLeft_m.append(interval_without_var[0])
    intervalWVRight_m.append(interval_without_var[1])

    plt.tight_layout(rect=[0, 0, 1, 0.95])  # Корректировка расположения заголовка
    plt.show()

# График зависимости от размера выборки
plt.figure(figsize=(8, 6))
plt.plot(size_m, mean_m, 'bo-', label='Точечная оценка мат. ожидания')
plt.plot(size_m, intervalLeft_m, 'g--', label='Левая граница (изв. дисперсия)')
plt.plot(size_m, intervalRight_m, 'g-', label='Правая граница (изв. дисперсия)')
plt.plot(size_m, intervalWVLeft_m, 'r--', label='Левая граница (неизв. дисперсия)')
plt.plot(size_m, intervalWVRight_m, 'r-', label='Правая граница (неизв. дисперсия)')
plt.xlabel('Размер выборки (N)')
plt.ylabel('Значение')
plt.title('Зависимость оценок и доверительных интервалов от размера выборки')
plt.legend()
plt.grid(True)
plt.show()