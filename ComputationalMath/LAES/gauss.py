from typing import List


class GaussMatrix:
  m: List[List[int]] = [
    [1, 2, 3, 2, 8],
    [2, 1, 1, 1, 5],
    [1, -1, 2, 1, -1],
    [1, 1, -1, 3, 10]
  ]
    
  def trianglize(self):
    for col in range(len(self.m[0]) - 2):
      print(f"Selected column {col + 1}")

      for i in range(col, len(self.m) - 1):
        print(f"Substituting row {i + 2} with row {col + 1} mulitplied by {-(self.m[i + 1][col] / self.m[col][col])}")
        self.substitute_row(i + 1, col, -(self.m[i + 1][col] / self.m[col][col]))
      
        for t in self.m:
          print(t)
      
        print()
  
  def solve(self):
    sols = [None] * len(self.m)

    for i in range(len(self.m) - 1, -1, -1):
      if i == len(self.m) - 1:
        sols[i] = self.m[i][len(self.m[i]) - 1] / self.m[i][i]
      
      else:
        accum = list(map(lambda q,x: q*x, self.m[i][len(self.m[i]) - 1 - len(list(filter(None, sols))):len(self.m[i]) - 1], filter(None, sols)))
        sols[i] = (self.m[i][len(self.m[i]) - 1] - sum(accum)) / self.m[i][i]
    
    for i, t in enumerate(sols, 1):
      print(f"x{i}={t}")

  def substitute_row(self, target: int, source: int, mul_by: int):
    self.m[target] = list(map(lambda a,b: a+b, self.m[target], map(lambda x: x*mul_by, self.m[source])))


gm = GaussMatrix()
gm.trianglize()

try:
  gm.solve()

except ZeroDivisionError:
  print("No solutions")
