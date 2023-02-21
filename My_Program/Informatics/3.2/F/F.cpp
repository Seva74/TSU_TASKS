#include <iostream>

using namespace std;
int x;
int main()
{
    cin >> x;
    if (x % 10 == 0) x=x/10;
    while (x >= 1) {
        cout << x % 10;
            x /= 10;
    }
    return 0;
}

