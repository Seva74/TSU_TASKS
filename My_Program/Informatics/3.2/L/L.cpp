#include <iostream>

using namespace std;
int a, b, c, d;
int main()
{
    d = 1;
    cin >> a;
    while (a>=1) {
        b = a % 10;
        c += + b*d;
        d *= 2;
        a /= 10;
    }
    cout << c;
}
