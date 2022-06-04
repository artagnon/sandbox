#include <iostream>
#include <map>
#include <vector>

// [O(nrand) time, O(nrand) space]
class Deck {
  std::map<unsigned, unsigned> deck;
  std::vector<unsigned> rands;
  int rand_max, nrand;

public:
  // Initialize [O(1) time]
  Deck(unsigned rand_max, unsigned nrand) : rand_max(rand_max), nrand(nrand) {}

  // Knuth shuffle [O(nrand) time]
  void shuffle() {
    for (int i = 0; i < nrand; ++i) {
      rands.emplace_back(rand() % rand_max);
      deck[i] = i;
      deck[rands[i]] = rands[i];
    }
    for (int i = 0; i < nrand; ++i)
      std::swap(deck[i], deck[rands[i]]);
  }

  // deal from the deck: [O(nrand) time]
  operator std::vector<unsigned>() {
    std::vector<unsigned> ret;
    for (int i = 0; i < nrand; ++i)
      ret.emplace_back(deck[i]);
    return ret;
  }
};

std::vector<unsigned> deal(unsigned rand_max, unsigned nrand) {
  assert(nrand <= rand_max);
  srand(time(nullptr));

  // initalize the deck
  Deck deck{rand_max, nrand};

  // shuffle the deck
  deck.shuffle();

  // deal from the deck
  return deck;
}

int main(void) {
  for (auto &el : deal(8, 5))
    std::cout << el << std::endl;
  return 0;
}
