
# Autentikasi User

### Login

Methode: POST

Parameters:

| Name  |   | Type  |  Description |
|---|---|---|---|
| username | required | string  | Username/Email |
| password | required | string  | Password |


Format output:


```bash
{
  "code": 0,
  "msg": "OK",
  "user_email": "[email]",
  "token": "[token]",
  "expired": "[date]"
}

```

**Token** hasil dari aktivitas login ini harus dilewatkan ke *Header* dari setiap request API

### Penggunaan

Contoh melalui curl:

```bash
  curl -X POST "smartfarm.pascal-id.test/auth/" \
  -d 'username=tigor&password=123456'

```

Example result:

```bash
{
  "code": 0,
  "msg": "OK",
  "user_email": "user@email.tld",
  "token": "EAC5EC35EF239D3C13769E1078023A01581CD9FF947718CF9E9719000EBF53DA",
  "expired": "2019-10-10 17:07:13"
}

```

