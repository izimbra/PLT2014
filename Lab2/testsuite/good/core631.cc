int main() {
bool apa = true || false;
bool bepa = false;


if (apa) { printInt(1);} else {printInt (0);}
if (apa  || apa)  {printInt (1);} else {printInt(0);}
if (bepa || apa)  {printInt (1);} else {printInt(0);}
if (apa  || bepa) {printInt (1);} else {printInt(0);}
if (bepa || bepa) {printInt (1);} else {printInt(0);}

if ( (bepa || bepa) || apa) {printInt (1);} else { printInt (0);}






  return 0 ;

}

