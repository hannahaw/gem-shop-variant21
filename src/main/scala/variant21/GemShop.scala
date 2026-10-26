package variant21

// Варіант 21: магазин мобільної гри — геми та монети

// Події, які можуть статися в магазині
sealed trait Event
case class BuyGems(gems: Int, realMoney: Double) extends Event
case class ConvertGemsToCoins(gems: Int) extends Event
case class UpgradeCard(cardName: String, costCoins: Int) extends Event
case class DailyGift(coins: Int, gems: Int) extends Event

// Одне куплене покращення карти
case class Upgrade(cardName: String, costCoins: Int)

// Стан гравця
case class State(
    gems: Int,
    coins: Int,
    totalGemsSpent: Int,
    upgrades: List[Upgrade]
)

object GemShop {

  // Курс конвертації: 1 гем = 10 монет
  val CoinsPerGem: Int = 10

  val emptyState: State = State(gems = 0, coins = 0, totalGemsSpent = 0, upgrades = List())

  // Застосувати одну подію до стану
  def apply(state: State, event: Event): Either[String, State] =
    event match {
      case BuyGems(gems, realMoney) =>
        if (gems <= 0) Left("Кількість гемів має бути більшою за 0")
        else if (realMoney <= 0) Left("Сума оплати має бути більшою за 0")
        else Right(state.copy(gems = state.gems + gems))

      case ConvertGemsToCoins(gems) =>
        if (gems <= 0) Left("Кількість гемів має бути більшою за 0")
        else if (gems > state.gems)
          Left(s"Недостатньо гемів: є ${state.gems}, потрібно $gems")
        else
          Right(
            state.copy(
              gems = state.gems - gems,
              coins = state.coins + gems * CoinsPerGem,
              totalGemsSpent = state.totalGemsSpent + gems
            )
          )

      case UpgradeCard(cardName, costCoins) =>
        if (costCoins <= 0) Left("Вартість покращення має бути більшою за 0")
        else if (costCoins > state.coins)
          Left(s"Недостатньо монет для '$cardName': є ${state.coins}, потрібно $costCoins")
        else
          Right(
            state.copy(
              coins = state.coins - costCoins,
              upgrades = state.upgrades :+ Upgrade(cardName, costCoins)
            )
          )

      case DailyGift(coins, gems) =>
        if (coins < 0 || gems < 0) Left("Подарунок не може бути від'ємним")
        else Right(state.copy(coins = state.coins + coins, gems = state.gems + gems))
    }

  // Програти всі події по черзі, починаючи з порожнього стану.
  // Якщо якась подія повертає Left, далі помилка просто передається до кінця.
  def replay(events: List[Event]): Either[String, State] =
    events.foldLeft(Right(emptyState): Either[String, State]) { (acc, event) =>
      acc match {
        case Left(error)  => Left(error)
        case Right(state) => apply(state, event)
      }
    }

  // Три найдорожчі покращення (від найдорожчого)
  def top3Upgrades(state: State): List[Upgrade] =
    state.upgrades.sortBy(u => -u.costCoins).take(3)
}
