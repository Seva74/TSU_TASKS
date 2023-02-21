#include <iostream>

using namespace std;
int n, m[10000], a,b;
int main()
{
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> m[i];
    }
    a = -1;
    for (int i = 0; i < n; i++) {
        for (int k = 0; k < i; k++)
        {
            if (m[i] != m[k]) a++;
        }
        if (a > 0) {
            b++;
            a = -1;
        }
    }
    cout << b+2;
    return 0;
}