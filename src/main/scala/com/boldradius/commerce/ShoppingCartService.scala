package com.boldradius.commerce

case class Bundle(val items: Map[Item, Int])
case class Catalog(val prices: Map[Item, BigDecimal])
case class Deals(val prices: Map[Bundle, BigDecimal])

class ShoppingCartService(catalog:Catalog, deals: Deals) {
  /**
   * true if bundle can be formed from items
   */
  private def isValidBundle(items: Map[Item, Int], bundle: Bundle) = bundle.items.forall {
    case (i, n) => items.contains(i) && items(i) >= n
  }

  /**
   * true if bundle offers savings over regular price
   */
  private def isSavings(bundle: Bundle) = bundle.items.map {case (i, n) => n * catalog.prices(i)}.sum > deals.prices(bundle)

  /**
   * Returns all possible valid combinations of bundle quantities in items
   */
  private def validCombinations(items: Map[Item, Int]): Stream[Map[Bundle, Int]] = {
    /**
     * true if there are sufficient items to form all bundles
     */
    def isValidCombo(bundles: Map[Bundle, Int]) = {
      val bundleItems:Map[Item, Int] = bundles.foldLeft(Map[Item, Int]()) {
        // sum the items in the two maps
        case (acc:Map[Item, Int], (b: Bundle, n: Int)) => {
          acc ++ (b.items map { case (i: Item, v: Int) => (i, acc.getOrElse(i, 0) + v * n) })
        }
      }
      bundleItems forall {case (k, v) => items.contains(k) && items(k) >= v}
    }

    /**
     * given no other bundles, the largest number of this bundle which can be formed from items
     */
    def maxQuantity(bundle: Bundle):Int = bundle.items.map {case (i, n) => items(i) / n} min

    val bundles = deals.prices.keys.filter(b => isValidBundle(items, b) && isSavings(b))

    (bundles map {
      b:Bundle => (0 to maxQuantity(b)) map (n => (b, n))
    }).foldLeft(Stream(Map[Bundle, Int]())){
      (acc, bs) => for (a <- acc; b <- bs) yield if (b._2 > 0) a + b else a    // get all the combinations
    } filter { bs => isValidCombo(bs) }    // filter for the ones which there are enough items for
  }

  /**
   * from a list of items with quantities, applies the best possible deal combination to calculate the lowest total cost
   */
  def calculateLowestCost(items: List[(Item, Int)]): BigDecimal = {
    /**
     * create a new map with item quantities updated by removing toRemove quantities
     */
    def removeItems(items: Map[Item, Int], toRemove: List[(Item, Int)]): Map[Item, Int] = toRemove match {
      case Nil => items
      case (i, n) :: rms => removeItems(items + ((i, items(i) - n)), rms)
    }

    /**
     * create a seq of item quantities from the map
     */
    def bundleItems(bundles: Map[Bundle, Int]) = bundles.toSeq.flatMap {
      case (b, n) => b.items map { case(i, m) => (i, m * n) }
    }

    /**
     * calculate the price of items when discount bundles are applied
     */
    def calculatePrice(bundles: Map[Bundle, Int]) = {
      removeItems(items.toMap, bundleItems(bundles).toList).map {
        case (i:Item, n:Int) => n * catalog.prices(i)
      }.sum + bundles.map {
        case (b:Bundle, n:Int) => n * deals.prices(b)
      }.sum
    }
    validCombinations(items.toMap).map(calculatePrice(_)).min
  }

}
