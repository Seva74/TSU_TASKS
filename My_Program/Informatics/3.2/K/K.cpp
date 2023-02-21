#include <iostream>

using namespace std;
int x, b, c;
int main()
{
    cin >> c;
    for (int i = 1; i <= c; i++) {
        cin >> x;
        b = b + x;
    }
    cout << b;
}