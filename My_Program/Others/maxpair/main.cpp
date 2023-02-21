#include <iostream>

using namespace std;
int x[1000000],ma,n;

int main()

{
    cout<<"kakova dlinna massiva?";

    cin>>n;
    for(int i=1;i<=n;i++)
    {
        cin>>x[i];
    }
    for(int i=1;i<=n-1;i++)
    {
        if(x[i]+x[i+1]>ma) ma=x[i]+x[i+1];
    }

    cout<<ma;
}
