#include <iostream>
#include <cmath>
#include <iomanip>
#include <cstdlib>
#include <ctime>
using namespace std;

//void relaxMethod(double A[10][10], double B[10], double x[10], double xn[10], int n, double eps, double w) {
//    double ans[4];
//    int i, j, iter = 0;
//    double norma[4];
//    norma[0] = 100;
//    norma[1] = 100;
//	norma[2] = 100; 
//	norma[3] = 100;
//
//    // Инициализация начальных приближений
//    for (i = 0; i < n; i++) {
//        xn[i] = 0;
//        x[i] = xn[i];
//    }
//
//    // Решение методом релаксаций
//    while (max(max(norma[0],norma[1]),norma[2]) > eps) {
//        iter++;
//        norma[0] = 0;
//        norma[1] = 0;
//        norma[2] = 0;
//
//        for (i = 0; i < n; i++) {
//            x[i] = B[i];
//            for (j = 0; j < n; j++) {
//                if (i != j) {
//                    x[i] -= A[i][j] * x[j];
//                }
//                //cout << "x[j] = " << x[j];
//            }
//            x[i] /= A[i][i];
//            x[i] = w * x[i] + (1 - w) * xn[i];
//            if (abs(x[i] - xn[i]) > norma[i]) {
//                norma[i] = abs(x[i] - xn[i]);
//            }
//
//            xn[i] = x[i];
//        }
//    } 
//    // Вывод результата
//    for (i = 0; i < n; i++) ans[i] = x[i];
//    ans[3] = iter;
//    for (i = 0; i <= 3; i++) {
//        if(i<3)cout << "x" << i + 1 << " = " << ans[i] << endl;
//        else cout << "Number of iterations: " << iter << endl;
//    }
//}

void relaxMethod(double A[10][10], double B[10], double x[10], int n, double eps) {
    int i, j, iter = 0;
    double nev[10];
    double diag[10];
    double maxnev;
    int pos = 0;
    double C[10][10];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            C[i][j] = A[i][j];
        }
    }

    for (int i = 0; i < n; i++)diag[i] = A[i][i];
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {

            A[i][j] /= diag[i];
        }
        B[i] /= diag[i];
    }
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            A[i][j] *= (-1);
        }
    }
    for (int i = 0; i < n; i++)
    {
        nev[i] = B[i];
    }


    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            cout << A[i][j] << " ";
        }
        cout << B[i] << endl;
    }
    cout << endl;

    for (int i = 0; i < n; i++)
    {
        cout << nev[i] << endl;
    }
    //cout << endl;
 //Решение методом релаксаций
    for (int i = 0; i < n; i++)
    {
        x[i] = 0;
    }
    do {
        iter++;

        maxnev = nev[0];
        pos = 0;

        for (int i = 0; i < n; i++)
        {
            if (abs(maxnev) < abs(nev[i]))
            {
                pos = i;
                maxnev = nev[i];
            }
        }
        cout << "pos = " << pos + 1 << " maxnev = " << maxnev << endl;
        for (int i = 0; i < n; i++)
        {
            if (i != pos)
            {
                nev[i] = nev[i] + A[i][pos] * maxnev;
            }
            else nev[i] = 0;
        }
        cout << endl;
        for (int i = 0; i < n; i++) {
            cout << nev[i] << endl;
        }

        x[pos] += maxnev;
        //cout << maxnev<<endl;
        //cout << maxnev << endl;

    } while (abs(maxnev) > eps);
    // Вывод результата
    for (i = 0; i < n; i++) {
        cout << "x" << i << " = " << x[i] << endl;
    }
    cout << "Number of iterations: " << iter << endl;
}

void relaxMethod2(double A[3][3], double B[3], double x[3], int n, double eps) {
    int i, j, iter = 0;
    double nev[3];
    double diag[3];
    double maxnev;
    int pos = 0;
    double C[3][3];
    double D[3];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            C[i][j] = A[i][j];
        }
        D[i] = B[i];
    }
    cout << "Сгенерированная матрица:" << endl;
    for (int i = 0; i < n; i++) {
        cout << i << "  ";
        for (int j = 0; j < n; j++) {
            cout << C[i][j] << " ";
        }
        //cout << D[i];
        cout << endl;
    }
    for (int i = 0; i < n; i++)diag[i] = A[i][i];
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {

            A[i][j] /= diag[i];
        }
        B[i] /= diag[i];
    }
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            A[i][j] *= (-1);
        }
    }
    for (int i = 0; i < n; i++)
    {
        nev[i] = B[i];
    }


    //for (i = 0; i < n; i++) {
    //    for (j = 0; j < n; j++) {
    //        cout << A[i][j] << " ";
    //    }
    //    cout << B[i] << endl;
    //}
    cout << endl;

    //for (int i = 0; i < n; i++)
    //{
    //    cout << nev[i] << endl;
    //}
    //cout << endl;
 //Решение методом релаксаций
    for (int i = 0; i < n; i++)
    {
        x[i] = 0;
    }
    do {
        iter++;

        maxnev = nev[0];
        pos = 0;

        for (int i = 0; i < n; i++)
        {
            if (maxnev < nev[i])
            {
                pos = i;
                maxnev = nev[i];
            }
        }
        //cout << "pos = " << pos + 1 << " maxnev = " << maxnev << endl;
        for (int i = 0; i < n; i++)
        {
            if (i != pos)
            {
                nev[i] = nev[i] + A[i][pos] * maxnev;
            }
            else nev[i] = 0;
        }
        //cout << endl;
        //for (int i = 0; i < n; i++) {
        //    cout << nev[i] << endl;
        //}

        x[pos] += maxnev;
        //cout << maxnev<<endl;
        //cout << maxnev << endl;

    } while (maxnev > eps);
    // Вывод результата
    //for (i = 0; i < n; i++) {
    //    cout << "x" << i << " = " << x[i] << endl;
    //}
    //cout << "Number of iterations: " << iter << endl;
    bool l;
    l = 0;
    for (int i = 0; i < n; i++) {
        double diagonal = abs(A[i][i]);
        double sum = 0;
        for (int j = 0; j < n; j++)
            if (i != j) sum += abs(A[i][j]);
        if (diagonal <= sum) l = 1;
    }
    if (l == 0) cout << "Условие сходимости выполняется" << endl;
    else cout << "Условие сходимости не выполняется" << endl;


    for (int i = 0; i < n; i++) {
        double sum = 0;
        for (int j = 0; j < n; j++) {
            sum += C[i][j] * x[j];
        }
        if (abs(sum - D[i]) >= 1e-3) {
            //cout << "Решение неверное" << endl;
            //break;
            //cout << abs(sum - (B[i] * diag[i])) << " " << 1e-3<<endl;
            cout <<"x = "<<x[i]<< " sum = " << sum << " D = " << D[i] << endl;
            cout << endl << " n = " << n;
            //cout << diag[i]<<" ";
        }
    }
    cout << "Решение верное" << endl;

}



//void relaxMethod2(double A[50][50], double B[50], double x[50], double xn[50], int n, double eps, double w) {
//    int i, j, iter = 0;
//    double norma[50];
//    for (int i = 0; i < 50; i++)
//    {
//        norma[i] = 100;
//    }
//
//
//    // Инициализация начальных приближений
//    for (i = 0; i < n; i++) {
//        xn[i] = 0;
//        x[i] = xn[i];
//    }
//
//    // Решение методом релаксаций
//    while (max(max(norma[0], norma[1]), norma[2]) > eps) {
//        iter++;
//        norma[0] = 0;
//        norma[1] = 0;
//        norma[2] = 0;
//
//        for (i = 0; i < n; i++) {
//            x[i] = B[i];
//            for (j = 0; j < n; j++) {
//                if (i != j) {
//                    x[i] -= A[i][j] * x[j];
//                }
//                //cout << "x[j] = " << x[j];
//            }
//            x[i] /= A[i][i];
//            x[i] = w * x[i] + (1 - w) * xn[i];
//            if (abs(x[i] - xn[i]) > norma[i]) {
//                norma[i] = abs(x[i] - xn[i]);
//            }
//
//            xn[i] = x[i];
//        }
//    }
//    for (i = 0; i < n; i++) {
//        cout << "x" << i + 1 << " = " << x[i] << endl;
//    }
//    cout << "Number of iterations: " << iter << endl;
//}

bool checkSolution(double A[10][10], double B[10], double x[10], int n) {
    for (int i = 0; i < n; i++) {
        double sum = 0;
        for (int j = 0; j < n; j++) {
            sum += A[i][j] * x[j];
        }
        if (abs(sum - B[i]) > 1e-3) {
            cout << "Решение неверное" << endl;
            return 0;
        }
    }
    cout << "Решение верное" << endl;
}


void jacobiIterationMethod(double A[10][10], double B[10], double x[10], double xn[10], int n, double eps) {
    int i, j, iter = 0;
    double norma[4];
    norma[0] = 100;
    norma[1] = 100;
    norma[2] = 100;
    norma[3] = 100;

    // Инициализация начальных приближений
    for (i = 0; i < n; i++) {
        xn[i] = 0;
        x[i] = xn[i];
    }

    // Решение методом простых итераций (Якоби)
    while (max(max(norma[0], norma[1]), norma[2]) > eps) {
        iter++;
        norma[0] = 0;
        norma[1] = 0;
        norma[2] = 0;

        for (i = 0; i < n; i++) {
            x[i] = B[i];
            for (j = 0; j < n; j++) {
                if (i != j) {
                    x[i] -= A[i][j] * xn[j];
                }
            }

            x[i] /= A[i][i];

            if (abs(x[i] - xn[i]) > norma[i]) {
                norma[i] = abs(x[i] - xn[i]);
            }

            xn[i] = x[i];
        }
    }
    // Вывод результата
    for (i = 0; i < n; i++) {
        cout << "x" << i + 1 << " = " << x[i] << endl;
    }
    cout << "Number of iterations: " << iter << endl;
}
void jacobiIterationMethod2(double A[50][50], double B[50], double x[50], double xn[50], int n, double eps) {
    eps = 1e-5;
    int i, j, iter = 0;
    double norma[50];
    double C[50][50], D[50];
    for (int i = 0; i < n; i++) norma[i] = 100;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            C[i][j] = A[i][j];
        }
        D[i] = B[i];
    }
    //norma[0] = 100;
    //norma[1] = 100;
    //norma[2] = 100;
    //norma[3] = 100;

    // Инициализация начальных приближений
    for (i = 0; i < n; i++) {
        xn[i] = 0;
        x[i] = xn[i];
    }

    // Решение методом простых итераций (Якоби)
    while (max(max(norma[0], norma[1]), norma[2]) > eps) {
        iter++;
        for (int i = 0; i < n; i++) norma[i] = 0;
        //norma[0] = 0;
        //norma[1] = 0;
        //norma[2] = 0;

        for (i = 0; i < n; i++) {
            x[i] = B[i];
            for (j = 0; j < n; j++) {
                if (i != j) {
                    x[i] -= A[i][j] * xn[j];
                }
            }

            x[i] /= A[i][i];

            if (abs(x[i] - xn[i]) > norma[i]) {
                norma[i] = abs(x[i] - xn[i]);
            }

            xn[i] = x[i];
        }
    }
    // Вывод результата
    for (i = 0; i < n; i++) {
        cout << "x" << i << " = " << x[i] << endl;
    }
    cout << "Number of iterations: " << iter << endl;
    for (int i = 0; i < n; i++) {
        double sum = 0;
        for (int j = 0; j < n; j++) {
            sum += C[i][j] * x[j];
        }
        if (abs(sum - D[i]) >= 1e-3) {
            cout << "Решение неверное" << endl;
            //break;
            //cout << abs(sum - (B[i] * diag[i])) << " " << 1e-3<<endl;
            cout << "x = " << x[i] << ", sum = " << sum << ", D = " << D[i] <<", abs(sum - D[i]) = "<< abs(sum - D[i])<< endl;
            //cout << diag[i]<<" ";
        }
        else cout << "x = " << x[i] << ", sum = " << sum << ", D = " << D[i] << ", abs(sum - D[i]) = " << abs(sum - D[i]) << endl;
    }
    cout << "Решение верное" << endl;
}

//void generateMatrix(double C[50][50], int n) {
//    srand(time(0)); // Инициализация генератора случайных чисел
//
//    // Генерация случайной матрицы
//    for (int i = 0; i < n; i++) {
//        double sum = 0;
//        for (int j = 0; j < n; j++) {
//            if (i != j) {
//                C[i][j] = rand() % 10 + 1; // Пример случайного числа от 1 до 10
//                sum += abs(C[i][j]);
//            }
//        }
//        C[i][i] = sum + rand() % 10 + 1; // Пример случайного числа для диагонального элемента
//    }
//
//    bool l = 0;
//    for (int i = 0; i < n; i++) {
//        double diagonal = abs(C[i][i]);
//        double sum = 0;
//        for (int j = 0; j < n; j++)
//            if (i != j) sum += abs(C[i][j]);
//        if (diagonal <= sum) l = 1;
//    }
//    if (l == 0) cout << "Условие сходимости выполняется" << endl;
//    else cout << "Условие сходимости не выполняется" << endl;
//
//    //Вывод матрицы (если захочется)
//
//    cout << "Сгенерированная матрица:" << endl;
//    for (int i = 0; i < n; i++) {
//        for (int j = 0; j < n; j++) {
//            cout << C[i][j] << " ";
//        }
//        cout << endl;
//    }
//
//}

int main() {
    setlocale(LC_ALL, "");
    int n = 3;
    bool l = 0;
    double A[10][10] = { {-10, -2, -2}, {-1, 10, -2}, {-1, -1, 10} };
    double B[10] = { 6, 7, 8 };

    double x[10], xn[10], x2[50],xn2[50];
    double eps = 1e-3;
    double w = 1.1;

    // 2 exercise
    relaxMethod(A, B, x, n, eps);

    // 1 execrcise
    for (int i = 0; i < n; i++) {
        double diagonal = abs(A[i][i]);
        double sum = 0;
        for (int j = 0; j < n; j++)
            if (i != j) sum += abs(A[i][j]);
        if (diagonal <= sum) l = 1;
    }
    if (l == 0) cout << "Условие сходимости выполняется" << endl;
    else cout << "Условие сходимости не выполняется" << endl;

    //// 6 exercise
 //   checkSolution(A, B, x, n); cout << endl;

 //   // 7 exercise
 //   double A2[10][10] = { {10, -2, -2}, {-2, 10, -2}, {-2, -2, 10} };
 //   double B2[10] = { 6, 7, 8 };
 //   relaxMethod(A2, B2, x, xn, n, eps, w);
 //   checkSolution(A2, B2, x, n); cout << endl;


    // 8-9. исследование скорости сходимости в зависимости от заданной точности
    cout << setprecision(8) << "8-9. исследование скорости сходимости в зависимости от заданной точности и альтернативного алгоритма" << endl;
    double custom_epsilons[] = { 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7 };
    for (double epsilon : custom_epsilons) {
        cout << setprecision(10) << "for epsilon = " << epsilon << " using method relax method:" << endl; relaxMethod(A2, B2, x, xn, n, epsilon, w);
    // 9. исследование скорости сходимости в зависимости от алгоритма
        cout << setprecision(10) << "for epsilon = " << epsilon << " using iter method:" << endl; jacobiIterationMethod(A2, B2, x, xn, n, epsilon); cout << endl;
    }

     //10 exercise
    int ngenerated = 3;
    double C[3][3];
    cout << "генерируется матрица 50x50... результат генерации:" << endl;
    //generateMatrix(c, ngenerated);

    n = ngenerated;
    srand(time(0)); // Инициализация генератора случайных чисел

    // Генерация случайной матрицы
    for (int i = 0; i < n; i++) {
        double sum = 0;
        for (int j = 0; j < n; j++) {
            if (i != j) {
                C[i][j] = rand() % 10 + 1; // Пример случайного числа от 1 до 10
                sum += abs(C[i][j]);
            }
        }
        C[i][i] = sum*2; // Пример случайного числа для диагонального элемента
    }

    l = 0;
    for (int i = 0; i < n; i++) {
        double diagonal = abs(C[i][i]);
        double sum = 0;
        for (int j = 0; j < n; j++)
            if (i != j) sum += abs(C[i][j]);
        if (diagonal <= sum) l = 1;
    }
    if (l == 0) cout << "Условие сходимости выполняется" << endl;
    else cout << "Условие сходимости не выполняется" << endl;

    //Вывод матрицы (если захочется)

    cout << "Сгенерированная матрица:" << endl;
    for (int i = 0; i < n; i++) {
        cout << i << "  ";
        for (int j = 0; j < n; j++) {
            cout << C[i][j] << " ";
        }
        cout << endl;
    }
    double b4[3];
    for (int i = 0; i < 50; i++)
    {
        b4[i] = 1;
    }

    //relaxMethod2(C, b4, x2, n, eps);
    //jacobiIterationMethod2(C, b4, x2, xn2, n, eps);
    relaxMethod2(C, b4, x2, n, eps);


    return 0;
}
//1 -0.910331                                                                                                             
//2 0.766082                                                                                                              
//3 0.785575