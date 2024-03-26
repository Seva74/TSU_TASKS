#include <iostream>
#include <cmath>
#include <iomanip>
using namespace std;

int n = 7; // степень полинома
double a = 1.0;
double b = 2.0;
double t[10];
double result;

// Функция, вычисляющая значение интегрируемой функции
double integral(double x) {
    //return x * log(x);
    //return pow(x, 2);
    //return (1 / (1 + pow(x, 3)));
    //return 1 / (1 + pow(x, 2));
    return log(x) / x;
    //return 1 / (2 + x);
}
double* nodes(int n, double* t)
{
    if (n == 2)
    {
        t[1] = 0.577350;
        t[2] = -t[1];
    }
    if (n == 3)
    {
        t[1] = 0.707107;
        t[2] = 0;
        t[3] = -t[1];
    }
    if (n == 4)
    {
        t[1] = 0.794654;
        t[2] = 0.187592;
        t[3] = -t[2];
        t[4] = -t[1];
    }
    if (n == 5)
    {
        t[1] = 0.832498;
        t[2] = 0.374541;
        t[3] = 0;
        t[4] = -t[2];
        t[5] = -t[1];
    }
    if (n == 6)
    {
        t[1] = 0.866247;
        t[2] = 0.422519;
        t[3] = 0.266635;
        t[4] = -t[3];
        t[5] = -t[2];
        t[6] = -t[1];
    }
    if (n == 7)
    {
        t[1] = 0.883862;
        t[2] = 0.529657;
        t[3] = 0.323912;
        t[4] = 0;
        t[5] = -t[3];
        t[6] = -t[2];
        t[7] = -t[1];
    }
    
    return t;
}

double cheb(int n) {
    result = 0.0;
    nodes(n, t);
    for (int i = 1; i <= n; i++) {
        double xi = (b + a) / 2 + (b - a) * t[i] / 2;
        result += integral(xi);
    }
    result *= (b-a)/n;
    for (int i = 1; i <= n; i++)
    {
        //cout << "t" << i << " = " << t[i] << endl;
    }
    return result;
}

double trapez() {
    int intervals = 1000; // количество интервалов
    double h = (b - a) / intervals;
    double sum = 0.5 * (integral(a) + integral(b));

    for (int i = 1; i < intervals; i++) {
        double x = a + i * h;
        sum += integral(x);
    }

    return sum * h;
}

double simpson(int intervals) {
    //int intervals = 1000; // количество интервалов (должно быть четным)
    double h = (b - a) / intervals;
    double sum = integral(a) + integral(b);

    for (int i = 1; i < intervals; i += 2) {
        double x = a + i * h;
        sum += 4 * integral(x);
    }

    for (int i = 2; i < intervals - 1; i += 2) {
        double x = a + i * h;
        sum += 2 * integral(x);
    }

    return sum * h / 3;
}

int main() {
    setlocale(LC_ALL, "");

    cout << setprecision(7) << "Результат вычисления интеграла для " << n << " ординат: " << cheb(n) << endl;

    cout << setprecision(10) << "Исследование точности вычисления интеграла в зависимости от количества узлов разбиения" << endl;
    int custom_n[] = { 2,3,4,5,6,7 };
    for (double n : custom_n) {
        double custom_root = cheb(n);
        result /= n;
        cout << "for n = " << n << ", root = " << custom_root << endl;
    }

    double epsilon_simpson = 1e-6; // Заданная точность для Симпсона
    int intervals_simpson = 2; // Начальное количество интервалов
    double prevResultSimpson;
    double ResultSimpson = simpson(intervals_simpson);

    do {
        prevResultSimpson = ResultSimpson;
        intervals_simpson *= 2;
        ResultSimpson = simpson(intervals_simpson); 
        
    } while (abs(ResultSimpson - prevResultSimpson) >= epsilon_simpson);

    cout << setprecision(7) << "Результат вычисления интеграла методом Симпсона: " << prevResultSimpson <<" был достигнут при "<< intervals_simpson<<" интервалах для eps = "<< epsilon_simpson << endl;

    cout << setprecision(7) << "Результат вычисления интеграла методом Симпсона: " << simpson(1000) <<" при 1000 интервалах"<< endl;

    cout << setprecision(7) << "Результат вычисления интеграла методом трапеций: " << trapez() << " при 1000 интервалах" << endl;
    cout << endl << "Для завершения введите любое число." << endl;
    cin >> a;
    return 0;
}
//0,636294
//0.6361494733  (n=2)
//0.6362943686  (n=7)
