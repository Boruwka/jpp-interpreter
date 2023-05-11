Mój język jest oparty na Latte, gramatyka jest również oparta na przykładowej gramatyce Latte https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2011/Latte/Latte.cf. 

-- Struktura języka i widoczność -----------------------

Język składa się z ciągu instrukcji, przy czym każda z nich może być deklaracją funkcji. Ciało funkcji, pętli i ifa to bloki - wewnątrz bloku widać zmienne zadeklarowane na zewnątrz, ale zmienne zadeklarowane wewnątrz bloku nie wychodzą poza niego. Wewnątrz funkcji natomiast widoczne są jedynie zmienne, które zostały wewnątrz niej utworzone, lub przekazane do niej przez referencję. W obu przypadkach jest przesłanianie. 

-- Typy zmiennych ------------------------------------
Zmienne mogą być czterech typów - bool, string, int i pomocniczy typ void, który nie niesie wartości. Funkcje jak na razie nie mają return i zwracają tylko void, za to mogą przyjmować argumenty dowolnych typów i dowolnie wiele. Typów nie można castować, próba implicite użycia inta jako boola lub odwrotnie, kończy się błędem wykonania. 

-- Deklaracja punktowa -----------------------------------
  Na 15 punktów
  01 (trzy typy) - int, bool, string
  02 (literały, arytmetyka, porównania) - standardowe
  03 (zmienne, przypisanie)
  04 (print) - dla każdego typu jedna funkcja
  05 (while, if) - standardowe
  06 (funkcje lub procedury, rekurencja) - globalne funkcje, które mogą być rekurencyjne
  07 (przekazywanie przez zmienną) - jak referencja w C++
  Na 20 punktów
  09 (przesłanianie i statyczne wiązanie) *
  10 (obsługa błędów wykonania) (ok 5/3 pkt) - obsługa błędu dzielenia przez 0, wyświetlenie komunikatu o błędzie i zatrzymanie programu   
  11 (funkcje zwracające wartość) *
  Na 30 punktów
  12 (4) (statyczne typowanie) *
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe) *
  15 (2) (krotki z przypisaniem)
  16 (1) (break, continue) *
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
  18 (3) (generatory)

Razem: 16.6

Gwiazdką * oznaczam punkty, które orientacyjnie mam nadzieję dorobić w drugiej iteracji - ale nie wiem, czy mi się uda. 
