#include <iostream>

using namespace std;
int n, m[10000],a,b;
int main()
{
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> m[i];
    }
    cin >> a;
    for (int i = 0; i < n; i++) {
        if (a <= m[i])b++;
    }
    cout << b+1;
}