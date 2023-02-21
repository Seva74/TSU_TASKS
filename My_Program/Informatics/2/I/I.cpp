#include <iostream>
using namespace std;
int main() {
    int a, b, c, d;
    cin >> a >> b >> c >> d;
    if (a == c || b == d || a - c == b - d || c - a == d - b || -(a - c) == b - d || -(c - a) == d - b)cout << "YES";
    else cout << "NO";
    return 0;
}