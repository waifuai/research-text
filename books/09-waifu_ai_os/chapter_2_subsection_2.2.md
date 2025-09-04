# Compound Data Types


   * **Example:** `(1 2 3 "hello" T)` is a list containing an integer, another integer, a third integer, a string, and a boolean.  We can access these elements using indexing and various list processing functions.


* **Vectors:** Vectors are ordered collections of arbitrary data. They offer more efficient access to elements than lists, particularly useful when dealing with large quantities of numerical data for training AI models or handling sensor data in robot contexts.

   * **Example:** `#(1 2 3)` is a vector containing three integers.  Vector access is fast, unlike lists which must traverse a linked structure.


* **Hash Tables:** Hash tables provide key-value mapping. They are ideal for storing and retrieving data based on unique keys (e.g., associating sensor names with their corresponding driver functions).

   * **Example:** `(make-hash-table :test #'equal)` creates an empty hash table suitable for storing string keys.

* **Arrays:**  Arrays generalize vectors to multiple dimensions.  They offer high performance when dealing with matrix operations in numerical computation and certain AI algorithms (e.g., matrix multiplications for neural network training).

* **Structures:** Structures define custom composite data types.  Structures are essential for modeling complex objects and data relationships, particularly within AI models and device driver representations.

