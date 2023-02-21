#include <iostream>

using namespace std;
int a, b, n;
int main()
{
    cin >> n;
    for (int i=1; i<=n; i++){
        cin >> a;
        if (a == 0)b++;
    }
    cout << b;
}