#include <iostream>
using namespace std;
int main() {
    int n, h = 0, m, s;
    cin >> n;
    s = n % 60;
    h = n / 3600;
    n = n - h * 3600;
    h = h % 24;
    m = n / 60 % 60;
    cout << h << ':';
    cout << m / 10 << m % 10 << ':';
    cout << s / 10 << s % 10;
}