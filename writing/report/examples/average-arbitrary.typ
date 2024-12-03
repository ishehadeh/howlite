#import "../templates/example.typ": *

#code-example(caption: "Average an Arbirary Array",
  ```
func average(nums: &[Uint32; 1..1024]): Uint32 {
  let i: Uint32 = 0;
  let acc: Uint32 = 0;
  while i < nums.len {
    acc = acc + nums[i];
    i = i + 1;
  };

  acc / i
}
  ```)