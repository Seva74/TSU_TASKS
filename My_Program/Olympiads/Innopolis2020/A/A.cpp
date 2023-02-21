#include <iostream>

using namespace std;
long long a, b;
int main()
{
    long long c = 0, d = 0, f = 0, s1 = 0, s2 = 0;
    cin >> a >> b;
    if (a > 0 && b > 0 && a < 1000000000 && b < 1000000000) {
        s1 = a;
        s2 = b;
        if (a > b) {
            while (a > b) {

                while (b > 2) {
                    b -= 3;
                    d++;
                }

                a -= 3;
                c++;
            }
            while (a != 0) {
                a -= 1;
                b -= 1;
                f++;
            }
            if (c * 3 + d * 3 + f * 2 == s1 + s2)cout << c << " " << f << " " << d;
            else cout << "-1";
        }
        else if (a < b) {
            while (a < b) {

                while (a > 2) {
                    a -= 3;
                    d++;
                }

                b -= 3;
                c++;
            }
            while (b != 0) {
                a -= 1;
                b -= 1;
                f++;
            }
            if (c * 3 + d * 3 + f * 2 == s1 + s2)cout << d << " " << f << " " << c;
            else cout << "-1";
        }
        else if (a == b) {
            while (a > 2) {
                a -= 3;
                c++;
            }
            while (a > 0) {
                a -= 1;
                d++;
            }
            cout << c << " " << d << " " << c;
        }
    }
    else cout << "-1";
    return 0;
}
