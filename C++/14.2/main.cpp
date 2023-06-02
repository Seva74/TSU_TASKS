#include <iostream>
#include <cstring>
#include <fstream>
using namespace std;
    string q1,q2;
    int a,k,n;
    bool l;
void base(string s[])
{
    for(int i=0;i<k;i++)
    {
        l=0;
        if(s[i].find(q1)!=-1 && s[i].find(q2)==-1)
        {
            s[i]+="-";
            s[i]+=q2;
            l=1;
        }
        else if(s[i].find(q2)!=-1 && s[i].find(q1)==-1)
        {
            s[i]+="-";
            s[i]+=q1;
            l=1;
        }
        else if(s[i].find(q1)!=-1 && s[i].find(q2)!=-1)l=1;
    }
    if(l!=1)
    {
        s[k]+=q1;
        s[k]+="-";
        s[k]+=q2;
        k++;
        l=0;
    }
}
int main()
{
    l=0;
    k=0;
    ifstream in("input.txt");
    in>>n;
    string s[n];
    bool population[n];
    for(int i=0;i<n;i++) population[i]=1;
    while(in>>q1>>q2)
    {
        if(q1=="0" && q2=="0")break;
        population[stoi(q1)]=0;             // вместо -1 сделать boolean
        population[stoi(q2)]=0;
        base(s);
    }
    for(int i = 0; i< k; i++) cout<<s[i]<<endl;
    for(int i = 0; i< n; i++) if (population[i] == 1)cout<<i<<endl;
}
