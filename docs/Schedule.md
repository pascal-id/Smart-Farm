# Schedule


Endpoint: /schedule/

Methode: GET/POST

Parameters:

| Name  |   | Type  |  Description |
|---|---|---|---|
| stationId | required | string  | stations's slug id |
| nodeId | required | string  | specific node id |


## USAGE


### Get Schedule

with curl


```bash
curl "smartfarm.pascal-id.test/schedule/?stationId=tgr123&nodeId=qsw345sxP"

```

Example result :

```bash
{
  "code": 0,
  "count": 1,
  "data": [
    {
      "slug": "qsw345sxP",
      "name": "Bata Merah",
      "schedules": [
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
            "-1",
            "0",
            "1",
            "2",
            "3",
            "4",
            "5",
            "6",
            "7"
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
    }
  ]
}
```


### Set Schedule

Methode: POST

Parameters:

| Name  |   | Type  |  Description |
|---|---|---|---|
| stationId | required | string  | stations's slug id |
| nodeId | required | string  | specific node id |
| schedule | required | json  | schedule in json-string format |


Set value:

```bash
  curl -X POST "smartfarm.pascal-id.test/schedule/" \
    -d 'stationId=tgr123&nodeId=qsw345sxP&schedule="{yourScheduleAsJsonString}"'

```

Example result (depend on vendor):

```bash
{"code":200,"msg":"OK"}
```

