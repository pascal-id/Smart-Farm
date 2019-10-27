#  

# Raspberry Pi Hub

## Fungsi dan Fitur

Secara terus-menerus melakukan sinkronisasi data antara API *server* dengan Arduino dan mengeksekusi penjadwalan buka/tutup keran otomatis manual.

## Legend

`$`: perintah dieksekusi dengan hak akses pengguna normal

`#`: perintah dieksekusi dengan hak akses *root*, i.e. menggunakan `sudo` atau benar^2^ berganti ke *user root*

## Arsitektur dan Graf Kebergantungan

![depgraph](/mnt/Data/Works/Free/Smart-Farm/docs/hub/depgraph.png)

## Fungsi Tiap Unit dan Program

* Utils:
  * Mengabstraksi pencatatan untuk *debugging* maupun *monitoring*
  * Mengabstaksi permintaan HTTP dan menguraikan tanggapan JSON
  * Menyediakan pembantu untuk mendapatkan nilai murni dari sebuah simpul JSON
* SmartFarmArduino:
  * Mengabstraksi pengambilan/pembaruan data dari/ke Arduino, termasuk menangani ketidakstabilan tanggapan terhadap permintaan HTTP ke Arduino
* SmartFarmServer:
  * Mengabstraksi pengambilan/pembaruan data dari/ke API server
* SmartFarmHub:
  * Sekaligus sebagai program utama
  * Membaca konfigurasi dan menyebarkan isinya ke SmartFarmArduino dan SmartFarmServer
  * Menghubungkan fungsionalitas dari SmartFarmArduino dan SmartFarmServer
  * Melakukan penguraian dan pencocokan jadwal sesuai dengan waktu dan kondisi yang diminta

## Cara Kompilasi

1. Buka smartfarmhub.lpi
2. Pilih mode yang akan dibuat via Project Inspector atau Project Options->Build mode, kemudian Run->Build
   1. Debug (PC): buat versi untuk PC dengan debugging info and logging
   2. Release (PC): buat versi untuk PC tanpa debugging info and logging
   3. Debug (RasPi): buat versi untuk Raspberry Pi dengan debugging info and logging
   4. Release (RasPi): buat versi untuk Raspberry Pi tanpa debugging info and logging
3. Atau Run->Compile many modes dan pilih semua mode yang mau dibuat
4. *Executable* yang dihasilkan akan menyesuaikan dengan build mode sehingga namanya tidak bentrok

## Cara Eksekusi (tanpa *service*, cocok untuk *debugging* di PC maupun Raspberry Pi)

1. Pastikan config.ini dan *executable* berada pada 1 *folder*
2. Konfigurasi program bergantung pada nilai2 yang terdapat dalam config.ini, sesuaikan dahulu sebelum menjalankan program
3. Program tidak memerlukan parameter apapun, jalankan saja apa adanya:
   `$ ./smartfarmhub`

## Memindahkan ke Raspberry Pi (OS Raspbian)

Note: Untuk OS selain Raspbian, silakan cari padanannya sendiri

### Via SCP

1. Pastikan Raspberry Pi sudah menyala, SSH server sudah aktif dan berada 1 jaringan dengan PC

2. Buat *folder* /opt/pismartfarm

   [^]: tentu tidak harus di sini, namun berkas *service* secara *default* mengasumsikan lokasi ini

    di Raspbian:
   `$ mkdir -p /opt/pismartfarm`

3. Pindahkan config.ini dan executable (yang berakhiran -raspi, sesuaikan jika Anda ganti namanya)
   `$ scp config.ini pismartfarm.debug-raspi <user>@<ip raspberry pi>:/opt/pismartfarm/ `

4. Pindahkan pismartfarmhub.service:
   `$ scp pismartfarmhub.service /lib/systemd/system/`

### Via SD card

1. Masukkan SD card Raspbian ke PC
2. Sama seperti via SCP di atas, buat folder /opt/pismartfarm
3. Menggunakan fitur copy/move file dari PC, pindahkan:
   1. config.ini dan *executable* ke /opt/pismartfarm
   2. pismartfarmhub.service ke /lib/systemd/system

## Cara Eksekusi (sebagai *service* systemd)

1. Pastikan config.ini, *executable* dan pismartfarmhub.service sudah dipindahkan sesuai cara di atas
2. Pastikan isi config.ini, nama *executable* dan path di pismartfarmhub.service sudah disesuaikan jika anda menggantinya
3. Nama *executable* dalam pismartfarmhub.service diasumsikan sebagai pismartfarmhub saja, tanpa embel^2^ .debug-raspi atau apapun yang lain, ganti jika belum sesuai
4. Masuk ke Raspbian, baik langsung di Raspberry Pi maupun via SSH
5. Aktifkan *service*:
   `# systemctl enable pismartfarmhub`
   *service* akan berjalan setelah *reboot*
6. Jika ingin langsung dijalankan tanpa *reboot* dahulu:
   `# systemctl start pismartfarmhub`

