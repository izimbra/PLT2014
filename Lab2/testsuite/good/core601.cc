int main () {


foo (3,4,5);

return 0;

}


void foo (int x, int y,int z) {

printInt (6);

printInt (x);  //fails because it cannot lookup x
//printInt (x);

}
