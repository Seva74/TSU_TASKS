#include <iostream>
using namespace std;
int x[6], a;
int main()
{
    for (int i = 0; i < 6; i++) {
        cin >> x[i];
    }
    cin >> a;
    cout <<"! "<< x[0] + a;
    for (int i = 1; i < 6; i++) {
        cout << " " << x[i];
    }

}
