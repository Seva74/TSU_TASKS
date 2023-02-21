#include <iostream>

using namespace std;
int a, b, c;
int main()
{
    b = 1;
    c = 0;
    cin >> a;
        for (int i = 1; i <= a; i++) {
                cout << b << " ";
                c++;
                if (b == c) {
                    b++;
                    c = 0;
                }
        }
}
