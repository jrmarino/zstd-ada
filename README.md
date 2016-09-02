# zstd-ada [![License](http://img.shields.io/badge/license-ISC-green.svg)](https://github.com/jrmarino/libsodium-ada/blob/master/License.txt)
Zstandard for Ada - Fast real-time compression algorithm

Overview
--------

Ada bindings for **Zstd** native library that can highly compress
data rapidly and without loss.

Zstd
----

**Zstd**, short for Zstandard, is a new lossless compression algorithm, which
provides both good compression ratio _and_ speed for your standard compression
needs. "Standard" translates into everyday situations which neither look for
highest possible ratio (which LZMA and ZPAQ cover) nor extreme speeds (which
LZ4 covers).

**Zstd** is developed by Yann Collet and the source is available at:
https://github.com/facebook/zstd

The motivation for development, the algorithm used and its properties are
explained in the blog post that introduces the library:
http://fastcompression.blogspot.com/2015/01/zstd-stronger-compression-algorithm.html

website: http://www.zstd.net

Supported
---------

The API methods in the shared library are supported, including:

 * standard compression
 * standard decompression
 * compression using a dictionary
 * decompression using a dictionary

These work using either providing strings as input (and strings are returned)
or using files as input (and files are produced).  Using helper functions to
read and write entire files, the user could also read files and receive strings
as the result (and vice-versa).

Documentation
-------------

Check the comments in zstandard-functions.ads.  There are several test cases
located in the examples directory that illustrate the simplicity of use 
of the bindings as well.

License
-------

The bindings have been released under the ISC license.
