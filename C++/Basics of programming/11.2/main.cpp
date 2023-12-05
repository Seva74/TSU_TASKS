#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

int main()
{
    ifstream input("Test2.txt");

    vector<string> words;
    string word;
    while (input >> word)
    {
        words.push_back(word);
    }

    input.close();

    string longest_root = "NO";
    for (int i = 0; i < words.size(); i++)
    {
        for (int j = 0; j < words.size(); j++)
        {
            string root = words[i];
            string word = words[j];
            if (word.find(root) == 0 && root.length() > longest_root.length())
            {
                longest_root = root;
            }
        }
    }

    ofstream longest_root_file("longest_root.txt");

    longest_root_file << "Longest root word: " << longest_root << endl;
    longest_root_file << "Words for which it is the root:" << endl;
    for (int i = 0; i < words.size(); i++) {
        string word = words[i];
        if (word.find(longest_root) == 0) {
            longest_root_file << word << endl;
        }
    }

    longest_root_file.close();

    string most_common_root = "NO";
    int most_common_count = 0;
    for (int i = 0; i < words.size(); i++)
    {
        int count = 0;
        for (int j = 0; j < words.size(); j++)
        {
            if (words[j].find(words[i]) == 0)
            {
                count++;
            }
        }
        if (count > most_common_count)
        {
            most_common_root = words[i];
            most_common_count = count;
        }
    }


    ofstream most_common_root_file("most_common_root.txt");


    most_common_root_file << "Most common root: " << most_common_root << endl;
    most_common_root_file << "Number of words with most common root: " << most_common_count << endl;

    most_common_root_file.close();

    cout << "Program completed successfully. Check files most_common_root.txt and longest_root.txt"<< endl;

    return 0;
}
