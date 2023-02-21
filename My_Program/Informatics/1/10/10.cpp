#include <iostream>
using namespace std;
int main() {
    int a, b, c, d, e, f;
    cin >> a >> b >> c;
    d = a * c * 100 + b * c;
    e = d % 100;
    f = d / 100;
    cout << f << " " << e;
    return 0;
}