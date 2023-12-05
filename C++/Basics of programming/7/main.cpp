#include <iostream>
#include <cmath>

using namespace std;

int main() {
    double x, f;
    double step = (3.99) / 9.0; // шаг
    for (int i = 0; i <= 9; i++) {

        if(i==0) x=0.01;
        else x += step;
        f = sin(x) / x;
        cout << "x = " << x << ", f = " << f << endl;
    }

    cout<< '\n';
    int MAX, n = 3;
    double S = 2, fib = 1, prev = 1,g;

    cout << "Enter a large number MAX: ";
    cin >> MAX;
    double fib1;
    while (S <= MAX) {
        g=fib;
        fib += prev;
        prev=g;
        S+=fib;
        n++;
    }

    cout << "The sum of the first " << n - 1 << " Fibonacci numbers is " << S - fib << endl;
    cout << "The " << n - 1 << "th Fibonacci number is " << prev << endl;


}
