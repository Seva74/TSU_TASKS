import statistics
import scipy
from numpy import random
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import expon, gamma

N = random.randint(100, 200)  # Размер выборки
p = 0.7  # Вероятность успеха
n = 10   # Кол. экспериментов для бином. распределения
alfa = 2  # Параметр формы для гамма-распределения
beta = 1  # Параметр масштаба для гамма-распределения
lamb = 3  # Параметр интенсивности для экспоненциального распределения

for i in range(4):
    if i == 0:
        print('БИНОМИНАЛЬНОЕ РАСПРЕДЕЛЕНИЕ:')
        data = random.binomial(n, p, N)
        expected = random.binomial(n, p, N)

        sampleAvT = n * p  # Теоретическое мат. ожидание
        varianceT = n * p * (1 - p)  # Теоретическая дисперсия
        est_p = np.mean(data) / n

    elif i == 1:
        print('ГЕОМЕТРИЧЕСКОЕ РАСПРЕДЕЛЕНИЕ:')
        data = random.geometric(p, N)
        expected = random.geometric(p, N)

        sampleAvT = (1 - p) / p
        varianceT = (1 - p) / p**2
        est_p = 1 / (np.mean(data) + 1)

    elif i == 2:
        print('ЭКСПОНЕНЦИАЛЬНОЕ РАСПРЕДЕЛЕНИЕ:')
        data = random.exponential(1 / lamb, N)
        expected = random.exponential(1 / lamb, N)

        sampleAvT = 1 / lamb
        varianceT = 1 / lamb ** 2
        lamb_est = 1 / np.mean(data)

    else:
        print('ГАММА РАСПРЕДЕЛЕНИЕ:')
        data = scipy.stats.gamma.rvs(a=alfa, scale=1 / beta, size=N)
        expected = scipy.stats.gamma.rvs(a=alfa, scale=1 / beta, size=N)

        sampleAvT = alfa / beta
        varianceT = alfa / beta ** 2
        beta_est = statistics.variance(data) / np.mean(data)
        alfa_est = np.mean(data) * beta_est

    fig = plt.figure(figsize=(10, 5))
    axs = fig.subplots(nrows=1, ncols=2)

    if i == 0 or i == 1:
        # Полигон частот
        axs[0].hist(data, edgecolor='black')
        axs[0].set_title('Полигон частот')
        
        # Эмпирическая функция распределения
        bin_dt, bin_gr = np.histogram(data, bins=len(data))
        Y = bin_dt.cumsum()
        for j in range(len(Y)):
            axs[1].plot([bin_gr[j], bin_gr[j + 1]], [Y[j], Y[j]], color='black')
        axs[1].set_title('Эмпирическая функция распределения')

        b_dt, b_gr = np.histogram(data, bins=5)
        exp_bin_dt, exp_bin_gr = np.histogram(expected, bins=5)
        print(b_dt)
        print(exp_bin_dt)
        print('Заданная вероятность: ', p, ' Оценка вероятности: ', est_p)

        if i == 0:
            fig.suptitle(f'Биномиальное распределение (N={N}, n={n}, p={p})')
        else:
            fig.suptitle(f'Геометрическое распределение (N={N}, p={p})')
    else:
        # Гистограмма относительных частот
        axs[0].hist(data, edgecolor='black', weights=np.ones_like(data) / len(data))
        axs[0].set_title('Гистограмма относительных частот')

        # График плотности (оценка + теоретическая)
        sns.kdeplot(data, ax=axs[1], label='Оценка плотности')
        x = np.linspace(0, max(data), 100)
        if i == 2:
            axs[1].plot(x, expon.pdf(x, scale=1/lamb), 'r-', label='Теоретическая плотность')
            fig.suptitle(f'Экспоненциальное распределение (N={N}, λ={lamb})')
        else:
            axs[1].plot(x, gamma.pdf(x, a=alfa, scale=1/beta), 'r-', label='Теоретическая плотность')
            fig.suptitle(f'Гамма-распределение (N={N}, α={alfa}, β={beta})')
        axs[1].set_title('Плотность распределения')
        axs[1].legend()

        b_dt, b_gr = np.histogram(data, bins=5)
        exp_bin_dt, exp_bin_gr = np.histogram(expected, bins=5)

        if i == 2:
            print('Заданная лямбда: ', lamb, ' Оценка лямбда: ', lamb_est)
        else:
            print('Заданная альфа: ', alfa, ' Оценка альфа: ', alfa_est)
            print('Заданная бета: ', beta, ' Оценка бета: ', beta_est)

    # Оценки числовых характеристик
    sampleAv = np.mean(data)
    variance = statistics.variance(data)
    standartDeviation = np.std(data)
    mode = statistics.mode(data)
    median = statistics.median(data)
    asymmetry = scipy.stats.skew(data, axis=0, bias=True)
    excesses = scipy.stats.kurtosis(data, axis=0, bias=True)

    print('Выборочное среднее: ', sampleAv, ' Теоретически: ', sampleAvT)
    print('Дисперсия: ', variance, ' Теоретически: ', varianceT)
    print('СКО: ', standartDeviation)
    print('Мода: ', mode)
    print('Медиана: ', median)
    print('Коэф. асимметрии: ', asymmetry)
    print('Коэф. эксцесса: ', excesses)

    chistat, chip = scipy.stats.chisquare(b_dt, exp_bin_dt)
    print('p-value:', chip)
    print(' ')

    # Показываем графики
    plt.tight_layout(rect=[0, 0, 1, 0.95])  # Корректируем расположение заголовка
    plt.show()

