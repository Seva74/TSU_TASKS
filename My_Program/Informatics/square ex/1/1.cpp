#include <iostream>

using namespace std;
double a, b, c, d, x12;
int main()
{
    int e = 1;
    cout << "Nu davai, vvodi svoi koefficienti..." << '\n';
    cin >> a >> b >> c;
    d = b * b - 4 * a * c;
    cout << "Tak, tak, tak..... nu pohoje Diskriminant = " << d << "\n";
    if (d < 0) cout << "Nu i ladno, hot' men'she reshat' pridetsa :3";
    else if (d > 0) {
        cout << "Nu a nujnie tebe korni sootvetstvenno ravni:";
        for (int i = -500; i <= 500; i++) {
            if (a * i * i + b * i + c == 0) cout << i;
            if (a == 1) {
                a = 0;
                cout << " и ";
            }
        }
    }
    else if (d == 0) {
        cout << "Nu a nujniy tebe koren' sootvetstvenno raven:";
        for (int i = -500; i <= 500; i++) {
            if (a * i * i + b * i + c == 0) cout << i;
        }
    }
}
