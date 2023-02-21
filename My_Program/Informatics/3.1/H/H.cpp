#include <iostream>

using namespace std;
int a, n, s;
int main()
{
    cin >> n;
    s = 1;
    a = 1;
    for (int i=1; i <= n; i++) {
        s = s*2;
        a = a + s;
    }
    cout << a;
    return 0;

}
