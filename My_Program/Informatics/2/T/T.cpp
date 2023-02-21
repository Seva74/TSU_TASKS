#include <iostream>
using namespace std;
int main() {
    int a, b, c;
    cin >> a >> b >> c;
    if (a + b <= c || a >= b + c || b >= a + c) {
        cout << "NO";
    }
    else cout << "YES";
    return 0;
}