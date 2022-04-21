const data = [
  {
    "History": [
      {
        "ClientId": 0,
        "Start": 0,
        "End": 5,
        "Description": "put('100')"
      },
      {
        "ClientId": 1,
        "Start": 1,
        "End": 4,
        "Description": "get() -> '100'"
      },
      {
        "ClientId": 2,
        "Start": 2,
        "End": 3,
        "Description": "get() -> '0'"
      }
    ],
    "PartialLinearizations": [
      [
        {
          "Index": 2,
          "StateDescription": "0"
        },
        {
          "Index": 0,
          "StateDescription": "100"
        },
        {
          "Index": 1,
          "StateDescription": "100"
        }
      ]
    ],
    "Largest": {
      "0": 0,
      "1": 0,
      "2": 0
    }
  }
]
