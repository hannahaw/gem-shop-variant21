package variant21

import org.scalatest.funsuite.AnyFunSuite

class GemShopTest extends AnyFunSuite {

  test("сценарій: купівля, конвертація, покращення, подарунок, ще покращення") {
    val events = List(
      BuyGems(100, 4.99),
      ConvertGemsToCoins(50),          // 50 гемів -> 500 монет
      UpgradeCard("Лицар", 200),
      DailyGift(coins = 100, gems = 5),
      UpgradeCard("Маг", 300),
      ConvertGemsToCoins(20),          // 20 гемів -> 200 монет
      UpgradeCard("Лучник", 150),
      UpgradeCard("Гоблін", 50)
    )

    val result = GemShop.replay(events)
    assert(result.isRight)

    val state = result.toOption.get
    assert(state.gems == 35)            // 100 - 50 + 5 - 20
    assert(state.coins == 100)          // 500 - 200 + 100 - 300 + 200 - 150 - 50
    assert(state.totalGemsSpent == 70)  // 50 + 20
    assert(state.upgrades.length == 4)

    assert(
      GemShop.top3Upgrades(state) == List(
        Upgrade("Маг", 300),
        Upgrade("Лицар", 200),
        Upgrade("Лучник", 150)
      )
    )
  }

  test("UpgradeCard без достатньої кількості монет повертає Left і не змінює стан") {
    val state = State(gems = 10, coins = 50, totalGemsSpent = 0, upgrades = List())

    val result = GemShop.apply(state, UpgradeCard("Дракон", 100))

    assert(result.isLeft)
    // Початковий стан незмінний — case class іммутабельний
    assert(state == State(10, 50, 0, List()))
  }
}
