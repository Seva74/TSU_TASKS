#include <iostream>
using namespace std;
int main() {
    int a, b, c;
    cin >> a >> b >> c;
    if (a > b) {
        if (a >= c) {
            cout << a;
        }
    }
    if (b > c) {
        if (b >= a) {
            cout << b;
        }
    }
    if (c > a) {
        if (c >= b) {
            cout << c;
        }
    }
    if (a == b && b == c) {
        cout << a;
    }
    return 0;
}