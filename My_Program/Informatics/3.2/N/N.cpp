#include <iostream>

using namespace std;
int a, b, c, d, n;
int main()
{
    cin >> n;
    for (int i = 1; i <= n; i++) {
        cin >> a;
        if (a == 0)b++;
        else if (a > 0)c++;
        else if (a < 0)d++;
    }
    cout << b << " " << c << " " << d;
}