#include <iostream>
using namespace std;

int a, b, c, third, n, chet, nechet, big, small;
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
                if (b % 2 == 0 && small == 0) small = b;
                else big = b;
                if (c % 2 == 0) big = c;
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
                if (a % 2 != 0)small = a;
                if (b % 2 != 0 && small == 0) small = b;
                else big = b;
                if (c % 2 != 0) big = c;
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
