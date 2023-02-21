#include <iostream>
using namespace std;
int main() {
    int a, b, c, x, y;
    cin >> a >> b >> c;
    x = min(a, b, c);
    y = max(a, b, c);
    if ((a == x || a == y) && (b == x || b == y)) cout << x << c << y;
    else if ((b == x || b == y) && (c == x || c == y)) cout << x << a << y;
    else if ((a == x || a == y) && (c == x || c == y)) cout << x << b << y;
    return 0;
}