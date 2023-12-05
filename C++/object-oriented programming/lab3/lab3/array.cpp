#include "Array.h"
#include <iostream>

using namespace std;

template<class T>
Array<T>::Array(T* K, int n)
{
	mas = new T[n];
	for (int i = 0; i < n; i++)
		mas[i] = K[i];
	size = n;
}

template<class T>
Array<T>::Array(int n)
{
	mas = new T[n]; size = n;
}

template<class T>
Array<T>::Array(const Array& B)
{
	size = B.size;
	mas = new T[size];
	for (int i = 0; i < size; i++)
		mas[i] = B.mas[i];
}

template<class T>
Array<T>::~Array()
{
	delete[] mas;
}

template<class T>
Array<T>& Array<T>::operator=(const Array& B)
{
	if (this != &B)
	{
		delete[] mas;
		size = B.size;
		mas = new T[size];
		for (int i = 0; i < size; i++)
			mas[i] = B.mas[i];
	}
	return *this;
}

template<class T>
void Array<T>::Print()
{
	for (int i = 0; i < size; i++)
		cout << mas[i] << " ";
	cout << endl;
}

template<class T>
void Array<T>::Scan()
{
	cout << "Enter size of Array: ";
	cin >> size;
	cout << "Enter " << size << " elements: ";
	mas = new T[size];
	for (int i = 0; i < size; i++) cin >> mas[i];
}

template<class T>
Array<T>& Array<T>::DelPosTh(int m)
{
	for (int i = m; i < size - 1; i++)
		mas[i] = mas[i + 1];
	size--;
	return *this;
}

template<class T>
Array<T> Array<T>::DelPosNew(int m)
{
	Array<T> B(*this);
	B.DelPosTh(m);
	return B;
}

template<class Cont>
int Array<Cont>::Find(Cont a)
{
	for (int i = 0; i < size; i++) {
		if (mas[i] == a) return i;
	}
	return -1;
}

template<class T>
T& Array<T>::operator[](int n)
{
	return mas[n];
}

template<class T>
Array<T> Array<T>::operator+(T a)
{
	Array<T> B(size + 1);
	for (int i = 0; i < size; i++)
		B.mas[i] = mas[i];
	B.mas[size] = a;
	return B;
}

template<class T>
Array<T>& Array<T>::operator+=(T b)
{
	T* t;
	int i;
	t = new T[size + 1];
	for (i = 0; i < size; i++) t[i] = mas[i];
	t[size] = b;
	delete[] mas;
	mas = t;
	size++;
	return *this;
}

template<class T>
Array<T> Array<T>::operator+(Array<T>& B)
{
	Array<T> C(size + B.size);
	for (int i = 0; i < size; i++)
		C.mas[i] = mas[i];
	for (int i = 0; i < B.size; i++)
		C.mas[size + i] = B.mas[i];
	return C;
}

template<class T>
Array<T>& Array<T>::operator+=(Array<T>& B)
{
	T* t;
	int i;
	t = new T[size + B.size];
	for (i = 0; i < size; i++) t[i] = mas[i];
	for (i = 0; i < B.size; i++) t[size + i] = B.mas[i];
	delete[] mas;
	mas = t;
	size += B.size;
	return *this;
}

template<class T>
Array<T> Array<T>::operator-(T a)
{
	return DelPosNew(Find(a));
}

template<class T>
Array<T>& Array<T>::operator-=(T a)
{
	DelPosTh(Find(a));
	return *this;
}

template<class T>
bool Array<T>::operator==(Array<T>& B)
{
	if (size != B.size) return false;
	for (int i = 0; i < size; i++)
		if (mas[i] != B.mas[i]) return false;
	return true;
}

template<class T>
bool Array<T>::operator!=(Array<T>& B)
{
	return!(*this == B);
}

template class Array<int>;
template class Array<char>;
template class Array<double>;