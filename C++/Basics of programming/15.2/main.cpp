#include <iostream>
#include <fstream>
#include <cstring>
#include <cmath>

using namespace std;

char c;
string s,s1;
int RecordLength, Shift, amount_parts, a, k, z;
bool l;

string encryption()
{
    if(Shift<0 && l==0)
    {
        for(int i=0;i<amount_parts * RecordLength * 8;i++)
            s1[i] = s[(amount_parts * RecordLength * 8)-i-1];
        s=s1;
        Shift=abs(Shift);
        l=1;
    }

        for(int i=0;i<Shift;i++)
        {

            for(int j=0;j<(amount_parts*RecordLength*8);j++)
            {
                if((j%(RecordLength*8)) == 0) s1[j]=s[j+RecordLength*8-1];
                else s1[j]=s[j-1];
            }
            s=s1;
        }
        for(int q=0;q < (8 * amount_parts * RecordLength); q++)
            if(l==1)
                s[q]=s1[(amount_parts * RecordLength * 8)-q-1];

    for(int j=0;j< ( 8 * amount_parts * RecordLength);j++)
    {
        if(j%8==0) cout<<" ";
        cout<<s[j];
    }
    return s;
}
string decryption()
{
    if(l==0)Shift *= -1;
    else l=0;
    encryption();
}
int main()
{
    ifstream in("input.txt");
    in >>RecordLength;
    in>>Shift;
    while(in>>c)
    {
    k++;
    int z = 128;
    while(z>0) {
        a=(bool(c&z));
        s+=to_string(a);
        z=z>>1;
    }   }
    if(k%RecordLength != 0)
        for(int i=0;i<(RecordLength-(k%RecordLength));i++) s+="00100000";
    amount_parts = k/RecordLength;
    if (k%RecordLength != 0)amount_parts++;
    cout<<"initial: ";
    for(int j=0;j<(amount_parts*RecordLength*8);j++)
        {
            if(j%8==0)cout<<" ";
            cout<<s[j];
        }
    s1.resize(amount_parts * RecordLength * 8);
    cout<<endl<<"encrypted: ";
    encryption();
    cout<<endl<<"decrypted: ";
    decryption();
}
