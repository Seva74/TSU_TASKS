#include <iostream>

using namespace std;
double a, b, s;
int n;
int main()
{
    cin >> a >> n;
    b = 1;
    s = 1;
    for (int i = 1; i <= n; i++) {
        b = double(b) * double(a);
        s = s + b;
    }

    cout <<s;

    return 0;
}