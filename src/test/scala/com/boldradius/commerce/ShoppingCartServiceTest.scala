package com.boldradius.commerce

import org.scalatest.{PrivateMethodTester, FlatSpec, Matchers}


class ShoppingCartServiceTest  extends FlatSpec with Matchers with PrivateMethodTester {
  private val cart = createCart()
  private val isSavings = PrivateMethod[Boolean]('isSavings)
  private val isValidBundle = PrivateMethod[Boolean]('isValidBundle)
  private val validCombinations = PrivateMethod[Stream[Map[Bundle, Int]]]('validCombinations)

  "calculateLowestCost" should "use regular price for single unbundled item" in {
    assertResult(500.00)(cart.calculateLowestCost(List(("dog", 1))))
  }

  it should "use regular price for a single item which appears in some bundle which does not apply" in {
    assertResult(100.00)(cart.calculateLowestCost(List(("shoes", 1))))
  }

  it should "use bundled price for a single item with q=2 which is then bundled" in {
    assertResult(150.00)(cart.calculateLowestCost(List(("shoes", 2))))
  }

  it should "apply the best discounts when multiple are available" in {
    assertResult(238.0)(cart.calculateLowestCost(List(("shoes", 2), ("socks", 2), ("shirt", 1), ("shorts", 1))))
  }

  it should "use regular price and discounts in combination" in {
    assertResult(155.0)(cart.calculateLowestCost(List(("shoes", 2), ("sunglasses", 1))))
  }

  it should "apply a bundle more than once when required" in {
    assertResult(9.00)(cart.calculateLowestCost(List(("socks", 6))))
  }

  "isSavings" should "return true if the bundled items cost less than the regular priced ones" in {
    val bundle = new Bundle(Map("shoes" -> 2))
    assertResult(true)(cart invokePrivate isSavings(bundle))
  }

  it should "return false if the bundled items cost more than the regular priced ones" in {
    val bundle = new Bundle(Map("hat" -> 2))
    assertResult(false)(cart invokePrivate isSavings(bundle))
  }

  it should "return false if the bundled items cost the same as the regular priced ones" in {
    val bundle = new Bundle(Map("sunglasses" -> 2))
    assertResult(false)(cart invokePrivate isSavings(bundle))
  }

  "isValidBundle" should "return true if the bundle can be formed from the available items" in {
    val items = Map("shoes" -> 2)
    val bundle = new Bundle(Map("shoes" -> 2))
    assertResult(true)(cart invokePrivate isValidBundle(items, bundle))
  }

  it should "return false if it can't be formed from available items" in {
    val items = Map("shoes" -> 1)
    val bundle = new Bundle(Map("shoes" -> 2))
    assertResult(false)(cart invokePrivate isValidBundle(items, bundle))
  }

  "validCombinations" should "return all valid bundle combinations for available items" in {
    val items = Map[Item, Int]("shoes" -> 2, "socks" -> 2)
    val expected = Set(
      Map(),
      Map(new Bundle(Map("shoes" -> 2)) -> 1),
      Map(new Bundle(Map("socks" -> 2)) -> 1),
      Map(new Bundle(Map("shoes" -> 1, "socks" -> 2)) -> 1),
      Map(new Bundle(Map("socks" -> 2)) -> 1, new Bundle(Map("shoes" -> 2)) -> 1)
    )
    assertResult(expected)(cart invokePrivate validCombinations(items) toSet)
  }

  it should "return just one combination plus the empty map if that's all there is" in {
    val items = Map[Item, Int]("socks" -> 2)
    val expected = Set(Map(), Map(new Bundle(Map("socks" -> 2)) -> 1))
    assertResult(expected)(cart invokePrivate validCombinations(items) toSet)
  }

  it should "return stream of a map with no bundles if no valid combinations exist" in {
    val items: Map[Item, Int] = Map("dogs" -> 2)
    assertResult(Set(Map()))(cart invokePrivate validCombinations(items) toSet)
  }

  def createCart() = new ShoppingCartService(
    new Catalog(Map(
      "shoes" -> 100.00,
      "socks" -> 2.00,
      "shirt" -> 50.00,
      "shorts" -> 50.00,
      "hat" -> 5.00,
      "sunglasses" -> 5.00,
      "dog" -> 500.00
    )),
    new Deals(Map(
      new Bundle(Map("shoes" -> 2)) -> 150.00,
      new Bundle(Map("shoes" -> 1, "socks" -> 2)) -> 100.00,
      new Bundle(Map("shirt" -> 1, "shorts" -> 1)) -> 85.00,
      new Bundle(Map("shirt" -> 1, "socks" -> 2)) -> 52.00,
      new Bundle(Map("hat" -> 1, "sunglasses" -> 1)) -> 8.00,
      new Bundle(Map("socks" -> 2)) -> 3.00,
      new Bundle(Map("hat" -> 2)) -> 11.00,
      new Bundle(Map("sunglasses" -> 2)) -> 10.00
    ))
  )

}
