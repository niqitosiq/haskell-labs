main :: IO ()
main = do
  let t1 = (('a', 1), "Hello", [1.0, 2.0, 3.0])

  let t2 = [(1.0, True, ("world", 2)), (3.0, False, ("hello", 4))]

  let t3 = ([1, 2, 3], [1.0, 2.0, 3.0], [(True, 'a'), (False, 'b')])

  let t4 = [[[1, True], [2, False]], [[3, True], [4, False]]]

  let t5 = ((('a', 'b'), 'c'), ["Hello", "World"])

  let t6 = (([1.0, 2.0, 3.0], [True, False]), [1, 2, 3])

  let l1 = [1, (2, [True, False]), 3]

  let t7 = (True, ([False, True], [1, 2, 3]))

  let l2 = [([True, False], [1.0, 2.0, 3.0])]

  let l3 = [([1, 2, 3], ['a', 'b', 'c'])]
