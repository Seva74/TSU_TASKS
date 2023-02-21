#include <iostream>
using namespace std;

int a, b, c, first, second, third, n, chet, nechet, big, small;
int main()
{
    cin >> n;
    for (int i = 0; i < n; i++)
    {
        cin >> a >> b >> c;
        if (a % 2 == 0)chet++;
        else nechet++;
        if (b % 2 == 0)chet++;
        else nechet++;
        if (c % 2 == 0)chet++;
        else nechet++;
        if (chet > nechet)
        {
            if (a % 2 == 0)small = a;
            else if (b % 2 == 1)second += b;
            if (b % 2 == 0 && small == 0) small = b;
            else if (b % 2 == 0 && small != 0) big = b;
            else if (b % 2 == 1) second += b;
            if (c % 2 == 0) big = c;
            else if (c % 2 == 1) second += b;
            if (small > big)                                        //прога сказала, что ответ 458 
            {                                                       //на полный и 10 на короткое
                small = small + big;
                big = small - big;
                small = small - big;
            }
            third += big;
        }
        else if (chet < nechet)
        {
            if (a % 2 == 1)small = a;
            else if (b % 2 == 0)second += b;
            if (b % 2 == 0 && small == 1) small = b;
            else if (b % 2 == 1 && small != 0) big = b;
            else if (b % 2 == 0) second += b;
            if (c % 2 == 1) big = c;
            else if (c % 2 == 0) second += b;
            if (small > big)
            {
                small = small + big;
                big = small - big;
                small = small - big;
            }
            third += big;


        }
    }
    cout << third;
}