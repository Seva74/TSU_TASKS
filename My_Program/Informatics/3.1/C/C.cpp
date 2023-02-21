#include <iostream>

using namespace std;
int a, n;
int main()
{
    cin >> n;
    a = 1;
    for (int i = 1; i <= n; i++) {
        a = a * 2;
    }

    cout << a;

    return 0;
}