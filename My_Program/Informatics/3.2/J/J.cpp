#include <iostream>

using namespace std;
int x,b;
int main()
{
    for (int i = 1; i <= 100; i++) {
        cin >> x;
        b = b + x;
    }
    cout << b;
}