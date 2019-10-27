# Konvensi Transmisi Data

Setiap pengambilan/pengiriman data harus memberikan informasi **Token** melalui Header. Token didapatkan melalui [autentifikasi user login](Auth.md).
Berikut contoh jika dilakukan melalui Curl dari konsole/terminal:

```bash
curl "smartfarm.pascal-id.test/station/" -H 'Token: 084B4BFBE691F3DA3A99EA702F7D10C6CEB4A04E982F55F7C3AD9A0AA65A65CB'
```

## Update informasi device di tiap node

Pengiriman informasi ini dilakukan oleh tiap node (Rasberry) melalui API.

Format:

```bash
{
  "devices": {
    "device_name": {
      "state": [state_in_integer],
      "value": [value_in_float]
    },
    .
    .
    .
  }
}
```

Contoh:

```bash
{
  "devices": {
    "suhu": {
      "state": 0,
      "value": 31
    },
    "kelembaban": {
      "state": 0,
      "value": 55
    },
    "sprinkler": {
      "state": 1
    }
  }
}
```

## Penjadwalan

Setiap alat yang mempunyai fungsi penjadwalan otomatis bisa menggunakan data berikut:

```
[
  {
    "type": "0",
    "mode": "0",
    "value": "40",
    "active": "0"
  },
  {
    "type": "1",
    "mode": "0",
    "days": [
      "0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6"
    ],
    "value": "14:00",
    "active": "0"
  },
  {
    "type": "1",
    "mode": "1",
    "value": "2019-09-31 14:00",
    "active": "0"
  }
]
```



| Type  | Mode | Description |
|--|---|---|
| 0 | 0 | When humidity = x  |
| 0 | 1 | When Temperature = x  |
| 1 | 0 | Days  |
| 1 | 1 | One time  |



## Informasi Node

```
{
  "code": 0,
  "count": 1,
  "data": [
    {
      "slug": "[deviceId]",
      "name": "[deviceName]",
      "type": [type],
      "location": "[deviceLocation]",
      "state": [state],
      "value": [value],
      "options": {[see_above_information]},
      "schedules": [see_above_information],
      "temperature_average": [value],
      "humidity_average": [value]
    }
  ]
}
```

