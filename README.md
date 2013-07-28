## ExpressionFile: given SQL Expression Language, provide file-like CSV object.

<iframe src="http://slid.es/rduplain/lazy-pandas/embed"
  width="576" height="420" scrolling="no" frameborder="0"
  webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>

http://slid.es/rduplain/lazy-pandas


### Motivation

1. Preprocess some dataset into one or more tables.
2. Load the data into pandas.
3. ... without aggressively consuming memory in the process.

For datasets which are too large to fit into memory after the preprocessing:

1. Use a pre-filtering query to slim down the data to feed into pandas.
2. Lazily process all data into pandas, such that only pandas objects have
   allocated the full amount of memory needed to load the data.

This means that all middleware must be lazy.


### Obstacle

 * #2305 - Creating DataFrame from generator/iterator
   yielding records with minimal memory use
 * https://github.com/pydata/pandas/issues/2305

I have tried:

 * **Solr's CSV writer with pysolr, direct urlopen, or requests.**
   Solr consumes significant memory to prepare large result set.
   Most time is spent blocking on the socket.
 * **Direct SQLAlchemy query iteration.** If you are not careful, you will
   consume *a lot* of memory in fetching hundreds of thousands or millions of
   rows in one query.

When attempting to pre-filter my CSV results with a query on either Solr or
SQLAlchemy, I found it too easy to consume 2GB of RAM on my machine when
working with a .csv file that was 40MB on disk.


### Approach

pandas has text parsers which take file-like objects; these **text parsers
stream**. Given SQLAlchemy's SQL Expression Language, fetch a few rows at a
time from the database and yield a few CSV rows at a time through a file-like
object.

It's weird that ExpressionFile is deserializing from the database via
SQLAlchemy and reserializing to CSV, so that it can be deserialized by
pandas. But it works.


### Other Uses

ExpressionFile itself knows nothing about pandas. It provides a file-like
object which *lazily* provides CSV lines when read. If you think this is
useful, let me know.
