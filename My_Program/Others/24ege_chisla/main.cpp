#include <iostream>
#include <fstream>

using namespace std;
int a,M[27];

char ch1,ch2;
int main()
{
    ifstream in ("t24.txt");
    in>>ch1;
    while(in>>ch2)
        {
        if(ch1=='A')
            {
            M[ch2-'A']++;
            ch1=ch2;
            }
        }
        for(int i=0;i<=25;i++)
        {
        if(M[i] > a){
        a=M[i];
        ch1='A'+i;
        }
        }
        cout<<"THE MOST POPULAR LETTER IS - "<<ch1;
}
