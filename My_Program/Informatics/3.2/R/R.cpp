#include <iostream>

using namespace std;
long long a, b, c, d, e, s;
int main()
{
    s = 0;
    cin >> a >> b >> c >> d >> e;
    for (int i = 0; i <= 1000; i++) {
        if (i != e) {
            if (((a * i * i * i + b * i * i + c * i + d)) / (i - e) == 0)s++;
        }
    }
    cout << s;
}