#include <iostream>
using namespace std;
int main() {
    int a, b, c, d;
    cin >> d;
    a = d / 100;
    b = d % 10;
    c = d / 10 % 10;
    cout << a + b + c;
    return 0;
}