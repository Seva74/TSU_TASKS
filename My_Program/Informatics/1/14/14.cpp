#include <iostream>
using namespace std;
int main() {
    int h, a, b;
    cin >> h >> a >> b;
    cout << (1 + (h - b - 1) / (a - b));
    return 0;
}