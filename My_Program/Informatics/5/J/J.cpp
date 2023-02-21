#include <iostream>

using namespace std;
int n, m[10000],a;
int main()
{
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> m[i];
    }
    a = 0;
    for (int i = 0; i < n; i++) {
        if (m[i] > a) a = m[i];
    }
    cout << a;
    return 0;
}