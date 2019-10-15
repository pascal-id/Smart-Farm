# Node History


Endpoint: /node/history/

Methode: GET

Parameters:

| Name  |   | Type  |  Description |
|---|---|---|---|
| id | required | string  | specific node id |
| limit | optional | integer  | number of data, default = 10|


## USAGE


### Get Node History

with curl


```bash
curl "smartfarm.pascal-id.test/node/history/?id=735Xaw2"

```

Example result:

```bash
{
  "code" : 0,
  "count" : 12,
  "data" : [
    {
      "slug" : "735Xaw2",
      "date" : "2019-10-01 16:49:31",
      "state" : "",
      "value" : "5"
    },
    
    .
    .
    .

    {
      "slug" : "735Xaw2",
      "date" : "2019-10-01 16:35:18",
      "state" : "2",
      "value" : "4"
    },
    {
      "slug" : "735Xaw2",
      "date" : "2019-10-01 16:35:18",
      "state" : "1",
      "value" : "4"
    }
  ]
}
```
