#include <iostream>
using namespace std;
int main() {
    int a, b, c, d;
    cin >> a >> b >> c >> d;
    if ((a + 1 == c || a - 1 == c || a == c) && (b + 1 == d || b - 1 == d || b == d) && 1) cout << "YES";
    else cout << "NO";
    return 0;
}