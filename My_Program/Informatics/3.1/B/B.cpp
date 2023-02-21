#include <iostream>

using namespace std;
int n, a;
int main()
{
    cin >> n;
    a = 1;
    for (int i = 1; i <= n; i++) {
        a=a*i;
    }

    cout << a;

    return 0;
}