#include <iostream>
using namespace std;
int main() {
    int a, b, c;
    c = 109;
    cin >> a >> b;
    cout << (a * b % c + c) % c;
    return 0;
}