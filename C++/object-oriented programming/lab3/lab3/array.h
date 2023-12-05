#pragma once
#include <iostream>

using namespace std;

template <class T>
class Array
{
	T* mas;
	int size;
public:
	Array(T*, int);
	Array(int = 1);
	Array(const Array<T>&);
	~Array();
	Array<T>& operator=(const Array<T>&);

	void Print();
	void Scan();
	int getSize() const { return size; };

	Array<T>& DelPosTh(int);
	Array<T> DelPosNew(int);

	int Find(T);
	T& operator[](int);

	Array<T> operator+(T);
	Array<T>& operator+=(T);

	Array<T> operator+(Array<T>&);
	Array<T>& operator+=(Array<T>&);

	Array<T> operator-(T);
	Array<T>& operator-=(T);

	bool operator==(Array<T>&);
	bool operator!=(Array<T>&);

	friend istream& operator>>(istream& is, Array<T>& X)
	{
		int n;
		is >> n;
		X.size = n;
		X.mas = new T[n];
		for (int i = 0; i < n; i++)
			is >> X[i];
		return is;
	}

	friend ostream& operator<<(ostream& os, Array<T>& X)
	{
		for (int i = 0; i < X.size; i++)
			cout << X[i] << " ";
		cout << endl;
		return os;
	}
};