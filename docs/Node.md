# Node


Endpoint: /node/

Methode: GET

Parameters:

| Name  |   | Type  |  Description |
|---|---|---|---|
| stationId | required | string  | station's slug id |
| id | optional | string  | specific device id |


Device Type:

- 0: Generic
- 1: switch
- 2: variable generic
- 3: device with timer

Device Options (as json format), ex:
```
{"time": {"08:00":5000,"13:00":10000,"16:00":5000}}
```

## USAGE


### Get Node List

with curl


```bash
curl "smartfarm.pascal-id.test/node/?stationId=tgr123"

```

Example result (depend on vendor):

```bash
{
  "code" : 0,
  "count" : 2,
  "data" : [
    {
      "slug" : "SWwxW23",
      "name" : "Farm #1",
      "type" : "0",
      "state" : "0",
      "value" : "0"
    },
    {
      "slug" : "735Xaw2",
      "name" : "Farm #2",
      "type" : "1",
      "state" : "0",
      "value" : "34"
    }
  ]
}
```


### Get Sensor Values / Node Detail

with curl


```bash
curl "smartfarm.pascal-id.test/node/?stationId=tgr123&id=qsw345sxP"

```

Example result :

```bash
{
  "code": 0,
  "count": 1,
  "data": [
    {
      "slug": "qsw345sxP",
      "name": "Multi Censor",
      "type": "3",
      "location": "Farm #3",
      "state": "0",
      "value": "0",
      "options": {
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
    }
  ]
}
```

### Set Sensor State/Values

Methode: POST

Parameters:

| Name  |   | Type  |  Description |
|---|---|---|---|
| id | required | string  | node ID |
| state | optional | integer  | device state in numeric. 0:on; 1:off |
| value | optional | float  | device value in float number. ex for termometer |
| options | optional | json-string  | device options |


Set value:

```bash
curl -X POST "smartfarm.pascal-id.test/node/" -d "id=735Xaw2&value=45"

```

Set custom variable via Options:

```bash
curl -X POST "smartfarm.pascal-id.test/node/" \
  -d 'id=qsw345sxP&state=1&value=3&options={"devices":{"suhu":{"state":0,"value":31},"kelembaban":{"state":0,"value":55},"sprinkler":{"state":1}}}'

```

Example result:

```bash
{"code":200,"msg":"OK"}
```


### Set Specific Sensor State/Value

Methode: POST

Parameters:

| Name  |   | Type  |  Description |
|---|---|---|---|
| id | required | string  | Node ID |
| device | required | string  | censor name |
| state | optional | integer  | device state in integer<br />0: on; 1:off| state | optional | integer  | device state in numeric. 0:on; 1:off |
| value | optional | float  | device value in float number. ex for termometer |


Set value:

```bash
curl -X POST "smartfarm.pascal-id.test/node/" -d 'id=qsw345sxP&device=suhu&value=31'

curl -X POST "smartfarm.pascal-id.test/node/" -d 'id=qsw345sxP&device=springkler&state=0'
```

Example result

```bash
{"code":200,"msg":"OK"}
```


