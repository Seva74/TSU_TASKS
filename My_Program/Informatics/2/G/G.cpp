#include <iostream>
using namespace std;
int main() {
    int d, a, b, c;
    cin >> a >> b >> c >> d;
    if (a == c || b == d) cout << "YES";
    else cout << "NO";
    return 0;
}