#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;

struct message {
    char *encrypted = new char[10000];
    char *decrypted = new char[10000];
    float f[26];
    float fn[26];
    
    message() {
        f[0] = 0.08167; f[1] = 0.01492; f[2] = 0.02782; f[3] = 0.04253; f[4] = 0.12702; f[5] = 0.02228; f[6] = 0.02015; 
        f[7] = 0.06094; f[8] = 0.06966; f[9] = 0.00153; f[10] = 0.00772; f[11] = 0.04025; f[12] = 0.02406; f[13] = 0.06749; 
        f[14] = 0.07507; f[15] = 0.01929; f[16] = 0.00095; f[17] = 0.05987; f[18] = 0.06327; f[19] = 0.09056; 
        f[20] = 0.02758; f[21] = 0.00978; f[22] = 0.02360; f[23] = 0.00150; f[24] = 0.01974; f[25] = 0.00074;
    }

    char *decrypt(char *encrypted, char *decrypted, int len, int N) {
        for(int i = 0; i < len; i++) {
            if(encrypted[i] > 96 && encrypted[i] < 123)
                decrypted[i] = ((encrypted[i]-97+N)%26)+97;

            else if(encrypted[i] > 64 && encrypted[i] < 91)
                decrypted[i] = ((encrypted[i]-65+N)%26)+65;

            else 
                decrypted[i] = encrypted[i];
        }
        return decrypted;
    }

    float entropy(char *decrypted, int len) {
        float hn = 0.0, char_count = 0.0;
        for(int i = 0; i < 26; i++)
            fn[i] = 0.0;
        for(int i = 0; i < len; i++) {
            if(decrypted[i] > 96 && decrypted[i] < 123) {
                int j = decrypted[i]-97;
                fn[j]++; 
                char_count++;
            }

            else if(decrypted[i] > 64 && decrypted[i] < 91) {
                int j = decrypted[i]-65;
                fn[j]++;
                char_count++;
            }
        }
        for(int i = 0; i < 26; i++) {
            fn[i] /= char_count;
            hn += fn[i]*log(f[i]);
        }
        return (-hn);
    }
};

int main(int argc, char** argv) {
    int N, len = 0;
    float prev_entropy, entropy;
    char c;
    message *msg = new message;

    ifstream input(argv[1]);
    if (!input.is_open())
		return 0;

    while (input >> noskipws >> c) {
  		msg->encrypted[len] = c;
        len++;
	}
	input.close();

    msg->decrypt(msg->encrypted, msg->decrypted, len, 1);
    prev_entropy = msg->entropy(msg->decrypted, len);
    N = 1;
    for(int i = 2; i < 26; i++) {
        msg->decrypt(msg->encrypted, msg->decrypted, len, i);
        entropy = msg->entropy(msg->decrypted, len);
        if(prev_entropy > entropy) {
            prev_entropy = entropy;
            N = i;
        }
    }

    cout << msg->decrypt(msg->encrypted, msg->decrypted, len, N) << endl;
    delete(msg);
	return 0;
}