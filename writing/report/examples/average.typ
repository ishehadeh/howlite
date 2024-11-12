
#import "../templates/example.typ": *

#code-example(caption: "Average Three Numbers",
  ```
  func average(x : 0..10, y : 0..10, z : 0..10) : 0..10 {
    (x + y + z) / 3
  }
  ```)