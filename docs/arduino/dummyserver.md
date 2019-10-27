 

# Arduino Dummy Server

## Fungsi dan Fitur

Mereplikasi fungsi Arduino asli yang sudah dilengkapi *shield* tambahan terutama untuk Wi-Fi agar dapat menyediakan HTTP *endpoint* untuk mengendalikan dan memonitor sensor.

## HTTP *Endpoint*

Hanya ada 1 *endpoint*, yaitu /, namun terdapat beberapa parameter module yang dapat diberikan untuk melakukan fungsi berbeda, yaitu:

* buka-kran: membuka kran
* tutup-kran: menutup kran
* status-kran: membaca status kran (tertutup/terbuka)
* suhu: membaca suhu saat ini
* kelembaban: membaca kelembaban saat ini

Selain module^2^ di atas, terdapat pula module^2^ tambahan untuk membantu simulasi perubahan kondisi karena tidak ada sensor asli di dalam program ini, yaitu:

* set-suhu: mengatur nilai suhu
* set-kelembaban: mengatur nilai kelembaban

## Cara Kompilasi

Lakukan seperti mengompilasi program Free Pascal biasa saja:

`$ fpc arduinodummyserver.pas`

opsi optimisasi tidak diperlukan mengingat program ini sangat sederhana dan tidak dipergunakan dalam keadaan sebenarnya.

## Cara Eksekusi

Tidak ada konfigurasi maupun *deployment* yang diperlukan, cukup:

`$ ./arduinodummyserver`

saja. *Port* yang digunakan di-*hardcode* ke 9000, silakan atur config.ini bagian [Arduino] ke port tersebut jika ingin menggunakan program ini sebagai simulator Arduino untuk SmartFarmHub.

