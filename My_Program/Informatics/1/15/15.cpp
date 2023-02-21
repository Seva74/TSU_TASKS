#include <iostream>
using namespace std;
int main() {
    int a, b, c;
    cin >> c;
    a = (c % (60 * 24) / 60);
    b = (c % 60);
    cout << a <<" "<< b;
}