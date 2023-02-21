#include <iostream>
using namespace std;
int main() {
    int a, b;
    cin >> a >> b;
    if (a > b) {
        cout << 1;
    }
    else if (b > a) {
        cout << 2;
    }
    else cout << 0;
    return 0;
}