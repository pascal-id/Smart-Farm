# SmartFarm API & Hub

Repository ini menyimpan source API dan Hub yang digunakan dalam workshop SmartFarm Pascal-ID.

Penekanan pada konsep dan pembelajaran, bukan untuk penggunaan di level produksi.

## Arsitektur

![Arsitektur](docs/api/images/arsitektur.png)

Arsitektur sistem ini menunjukan bahwa terdapat beberapa seksi perangkat:
- _Device controller_, dengan menggunakan arduino. Untuk membaca dan mengendalikan sensor/motor.
- Manajemen Hub, menggunakan Rasberry Pi. Hub ini menghubungkan antara _end-user_ yang menggunakan aplikasi mobile guna membaca dan mengendalikan sensor yang dilayani oleh _Device Controller_.
Proses komunikasi dilakukan melalui API (_Application Programming Interface_)
- API Server, sebagai pusat logic kendali dari keseluruhan sistem.
- Aplikasi (mobile). Antar muka akhir pengguna dalam memantau kondisi **Smart Farm**-nya.

* Belum termasuk dashboard.

## Instalasi

### Prerequisites

- Free Pascal Compiler, [https://www.freepascal.org/](https://www.freepascal.org/)
- FastPlaz framework, [http://fastplaz.com/](http://fastplaz.com/)

### Kompilasi

- [Build API](docs/Build-API.md)
- [Build Hub (Rasberry Pi)](docs/hub/hub.md)
- [Arduino Dummy Server](docs/arduino/dummyserver.md)

## Detail Penggunaan Endpoint API

- [Station List](docs/api/Station.md)
- [Node](docs/api/Node.md)
- [Node History](docs/api/Node-History.md)
- [Schedule](docs/api/Schedule.md)
- [Konvensi Transmisi Data](docs/api/Convention.md)

## Kontributor

- [Tigor Manurung](https://www.facebook.com/einsthonk)
- [Mario Ray Mahardhika](https://www.facebook.com/leledumbo2503)
- [Maskofa](https://www.facebook.com/profile.php?id=100009038417471)
- [Iwan Cahyadi Sugeng](https://www.facebook.com/iwan.c.sugeng)
- [Luri Darmawan](https://www.facebook.com/luridarmawan)


Materi [Presentasi](https://docs.google.com/presentation/d/1PJFVdCUmg1W3Jqlzk9VGxM3l-TUkNQhChRoK5CxJ0HM) tersedia di google-docs.

Thanks

