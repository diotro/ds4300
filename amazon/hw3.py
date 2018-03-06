from pymongo import MongoClient
client = MongoClient()


# String [Dict String JSON] -> [List of JSON]
# produces all the products from the given category that match
# all the given filter criteria
def find_products(product_category, filter_criteria):
    collection = client.products[product_category]
    return collection.find(filter_criteria)


def test():
    client.drop_database("products")
    db = client.products
    watches = db.watches

    # Create some watches
    bad_watch = {
        "dial_color": "Neon Pink",
        "brand": "Target",
        "diameter": "70mm"
    }

    nice_watch = {
        "diameter": "44mm",
        "brand": "Tommy Hilfiger",
        "dial_color": "Beige"
    }

    some_watches = [bad_watch, nice_watch]
    watches.insert_many(some_watches)

    results = find_products("watches", {"brand":"Tommy Hilfiger"})
    print([x for x in results]) # convert cursor to array


if __name__ == "__main__":
    test()

