#include <iostream>
using namespace std;
int main() {
    int a, b, c, d;
    cin >> a >> b >> c >> d;
    if ((a + 2 == c || a - 2 == c) && c > 0 && c < 9) {
        if ((b + 1 == d || b - 1 == d) && d > 0 && d < 9) {
            cout << "YES";
        }
        else cout << "NO";
    }

    else
        if ((b + 2 == d || b - 2 == d) && d > 0 && d < 9) {
            if ((a + 1 == c || a - 1 == c) && c > 0 && c < 9) {
                cout << "YES";
            }
        }
        else cout << "NO";
    return 0;
}