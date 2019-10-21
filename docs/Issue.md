# Issue

Endpoint: /issue/

Methode: GET/POST

Parameters:

| Name  |   | Type  |  Description |
|---|---|---|---|
| stationId | required | string  | stations's slug id |
| nodeId | optional | string  | specific node id |


## USAGE


### Get Issue

with curl


```bash
curl "smartfarm.pascal-id.test/issue/?stationId=tgr123&nodeId=qsw345sxP"

```

Example result :

```bash
{
  "code": 0,
  "count": 2,
  "data": [
    {
      "date": "",
      "station_name": "Cengkareng",
      "station_slug": "tgr123",
      "node_name": "Bata Merah",
      "node_slug": "qsw345sxP",
      "activity": "entah",
      "type": "1",
      "level": "1",
      "description": "ini deskripsi"
    },
    {
      "date": "",
      "station_name": "Cengkareng",
      "station_slug": "tgr123",
      "node_name": "Pasir Putih",
      "node_slug": "735Xaw2",
      "activity": "mbuh",
      "type": "0",
      "level": "1",
      "description": "sama"
    }
  ]
}
```


### Set Issue

Methode: POST

Parameters:

| Name  |   | Type  |  Description |
|---|---|---|---|
| stationId | required | string  | stations's slug id |
| nodeId | required | string  | specific node id |
| type | optional | integer  | type of issue |
| level | optional | integer  | level of issue |
| activity | required | string  | issue activit |
| description | required | string  | short description of issue |


Set value:

```bash
  curl -X POST "smartfarm.pascal-id.test/issue/" \
  -d 'stationId=tgr123&nodeId=qsw345sxP&type=1&level=2&activity=[activity]&description=[description]'

```

Example result:

```bash
{"code":200,"msg":"OK"}
```

