#include <iostream>

using namespace std;
int n;
double a, s;
int main()
{
    cin >> n;
    s = 1;
    a = 1;
    for (int i = 1; i <= n; i++) {
        s = s / i;
        a = a + s;
    }
    cout << a;
}
