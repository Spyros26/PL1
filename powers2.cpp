#include <iostream>
#include <bits/stdc++.h>
#include <fstream>


using namespace std;

//returns the sum of the vector's elements.
int sum(vector<int> v){
        int s = 0;
        for(vector<int>::size_type i = 0; i < v.size(); i++){
                s += v[i];
        }
        return s;
}

//returns the place from which all the remaining vector elements are zeros.
int last_digit_place(vector<int> v){
        int place = 0;
        int sum = 1;
        while(sum != 0){
                sum = 0;
                place++;
                for(vector<int>::size_type i = place; i < v.size(); i++){
                        sum += v[i];
                }
        }
        return place;
}

//https://www.geeksforgeeks.org/powers-2-required-sum/
// the code from the link above was used for the vector implemantation to convert the decimal number N into its binary equivalent.

void powers2(long int N, int K){
        vector<int> v;
        long int M = N;
        while (N > 0)  {
                v.push_back(N % 2);
                N = N / 2;
        }
        if(K < sum(v) || K > M) cout << "[]\n";
        else {
                while(sum(v) != K){
                        int i = 1;
                        while(v[i] == 0){i++;}
                        v[i] --;
                        v[i-1] = v[i-1] + 2;
                }
                for(int i = 0; i < last_digit_place(v); i++){
                        if(i == 0) cout << "[";
                        cout << v[i];
                        if(i != last_digit_place(v) - 1) cout << ",";
                        if (i == last_digit_place(v) - 1) cout << "]";
                }
                cout << "\n";
        }
}


int main(int argc, char *argv[]){
        int T, K;
        long int N;
        ifstream myfile (argv[1]);
        if(myfile.is_open()){
                myfile >> T;
                for(int i = 0; i < T; i++){
                        myfile >> N;
                        myfile >> K;
                        powers2(N,K);
                }
                myfile.close();
        }
        else cout << "Unable to open file";

        return 0;
}
