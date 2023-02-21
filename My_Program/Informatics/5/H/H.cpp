#include <iostream>

using namespace std;
int n, m[10000];
int main()
{
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> m[i];
    }
    for (int i = 0; i < n; i += 2) {
        if ((n - i) != 1) {
            cout << m[i + 1];
            cout << " ";
        }
        cout << m[i];
        cout << " ";
    }
    return 0;
}