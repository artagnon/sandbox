import accommodation
import unittest

class Tests(unittest.TestCase):
  def test(self):
    friends = [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]
    distances = {(1, 1): 0, (1, 2): 3.2, (1, 3): 2.4, (1, 4): 7.2,
                (2, 1): 3.2, (2, 2): 0, (2, 3): 11.2, (2, 4): 22.3,
                (3, 1): 2.4, (3, 2): 11.2, (3, 3): 0, (3, 4): 1.1,
                (4, 1): 7.2, (4, 2): 22.3, (4, 3): 1.1, (4, 4): 0}
    self.assertEqual(accommodation.arrangement(friends, distances), {1: 3, 2: 4, 3: 4})

if __name__ == "__main__":
  unittest.main()
