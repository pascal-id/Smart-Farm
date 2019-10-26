# Station


Endpoint: /station/

Endpoint Alias: /user/


Methode: GET

Parameters:

| Name  |   | Type  |  Description |
|---|---|---|---|
| id | optional | string  | use this parameter for station search details |



## USAGE


### Station List

with curl


```bash
curl "http://smartfarm.pascal-id.test/station/"

```

Example result (depend on vendor):

```bash
{
  "code" : 0,
  "count" : 5,
  "fieldName" : "slug, name",
  "data" : [
    [
      "tgr123",
      "Cengkareng"
    ],
    [
      "abcDef12Q",
      "Semarang"
    ],
    [
      "Wsrt351X",
      "Joglo"
    ],
    [
      "9DePx53q",
      "Cilacap"
    ],
    [
      "m4RIoXL",
      "Kebun Digital Mario"
    ]
  ]
}

```


### Station Detail

with curl


```bash
curl "http://smartfarm.pascal-id.test/station/?id=abcDef12Q"

```

Example result (depend on vendor):

```bash
{
  "code" : 0,
  "count" : 1,
  "fieldName" : "slug, name",
  "data" : [
    {
      "slug" : "tgr123",
      "name" : "Cengkareng"
    }
  ]
}
```
