#include <iostream>
#include <iostream>
#include <iomanip>
using namespace std;
int iter;
const double eps = 1e-5;
double a[2];
double y;

double fx(double x) { return (x + cos(x)-2); } 
double dfx(double x) { return (1 - sin(x)); } 
typedef double (*function)(double x); // задание типа function
double* solve(function fx, function dfx, double x0, double eps) {
    iter = 0;
    double x1 = x0 - fx(x0) / dfx(x0); 
    while (abs(x1 - x0) > eps) { 
        iter++;
        x0 = x1;
        x1 = x0 - fx(x0) / dfx(x0); 
        //cout << setprecision(10) << eps << endl;
    }
    a[0] = x1;
    a[1] = iter;
    return a;
}
double* solveSecant(function fx, double x0, double x1, double eps) {
    iter = 0;
    double x2 = x1 - fx(x1) * (x1 - x0) / (fx(x1) - fx(x0)); 
    while (abs(x2 - x1) > eps) {                            
        iter++;
        x0 = x1;
        x1 = x2;
        x2 = x1 - fx(x1) * (x1 - x0) / (fx(x1) - fx(x0)); 
    }
    a[0] = x2;
    a[1] = iter;
    return a;
}
int main() {
    setlocale(LC_ALL, "");

    // 1. Поиск интервалов, содержащих корень
    cout << "1. Поиск интервалов, содержащих корень" << endl;
    double a = -10.0; // начальная точка интервала
    double b = 10.0; // конечная точка интервала
    double step = 2; // шаг для дихотомии
    double interval_start = a;
    double interval_end = a + step;

    while (interval_end <= b) {
        if (fx(interval_start) * fx(interval_end) < 0) {
            // Найден интервал, на котором функция меняет знак
            cout << "Found root between " << interval_start << " and " << interval_end << " at " << solve(fx, dfx, interval_start, eps)[0] << endl;
        }
        interval_start = interval_end;
        interval_end = interval_start + step;
    }

    // 2. Проверка условия сходимости выполнена

    // 3. Основной алгоритм
    cout << "3. Основной алгоритм" << endl;
    double root = solve(fx, dfx, 4, eps)[0];
    cout << "Root found at x = " << root << endl;

    // 4. Оценка точности решения
    cout << "4. Оценка точности решения" << endl;
    cout << "For x = " << solve(fx, dfx, 4, eps)[0] << ", f(x) = " << (solve(fx, dfx, 4, eps)[0] + cos(solve(fx, dfx, 4, eps)[0])-2) << endl;

    // 5. График в mathcad и VS2022

    // 6. Исследование скорости сходимости в зависимости от заданной точности
    cout << setprecision(10)<<"6,7. Исследование скорости сходимости в зависимости от заданной точности и альтернативного алгоритма" << endl;
    double custom_epsilons[] = { 1e+0, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5 };
    for (double epsilon : custom_epsilons) {
        double custom_root0 = solve(fx, dfx, 4, epsilon)[0];
        double custom_root1 = solve(fx, dfx, 4, epsilon)[1];
        cout << "For epsilon = " << epsilon << ", number of iterations using Newton's method = " << custom_root1 << ", root = " << custom_root0 << endl;
    // 7. Исследование скорости сходимости в зависимости от алгоритма
        double custom_rootSecant = solveSecant(fx, 4, 4.5, epsilon)[0];
        double custom_iterSecant = solveSecant(fx, 4, 4.5, epsilon)[1];
        cout << setprecision(10)<<"For epsilon = " << epsilon << ", number of iterations using Secant method = " << custom_iterSecant << ", root = " << custom_rootSecant << endl;
    }
    return 0;
}
//<< setprecision(10) - кол-во знаков после запятой(еслм потребуется)