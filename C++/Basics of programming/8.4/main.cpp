#include <iostream>

using namespace std;

struct Element
{
    int data;
    Element* next;
};

int main() {
    Element* head = NULL;

    for (int i = 1; i <= 10; i++)
        {
        Element* newElement = new Element;
        newElement->data = i;
        newElement->next = NULL;

        if (head == NULL) head = newElement;
        else
            {
            Element* temp = head;
            while (temp->next != NULL) temp = temp->next;
            temp->next = newElement;
            }
        }
    Element* temp = head;
    while (temp != NULL)
        {
        cout << temp->data << " ";
        temp = temp->next;
        }
}
