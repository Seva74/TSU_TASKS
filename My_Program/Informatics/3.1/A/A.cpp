#include <iostream>

using namespace std;
int a, b;
int main()
{
    cin >> a;
    for (int i = 1; i <= a; i++) {
        b = b + i * i;
    }
    cout << b;
}
