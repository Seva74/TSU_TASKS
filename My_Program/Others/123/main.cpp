in>>n;
for(int i=1;i<=n;i++)
{
       in>>a[i]>>b[i];
       if(b[i]%28 == 0)b[i]/=28;
       else b[i]=b[i]/28 + 1;
       s+=b[i];
       z+=a[i]*b[i];
   }
   mi=z;
   s-=a[1];
   s2=b[1];
   s1=s-b[1];
   for(int i=2;i<=n;i++)
   {
       l=(a[i]-a[i-1]);
       z=z-(l*s1) + l*s2;
       if(z<mi)mi=i;
       s2+=b[i];
       s1-=b[i];
   }
   cout<<mi;
}
