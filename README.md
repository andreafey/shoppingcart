Shopping Cart Service
=====================

This is an API to determine the lowest total cost of a list of items given a catalog of regular prices and bundled 
discounts.

Usage
-----

Instantiate the `ShoppingCartService` with a `Catalog` and `Deals`. Call `calculateLowestCost` with a list of items and their
quantities to determine the total item cost.