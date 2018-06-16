# Derive [![Hackage version](https://img.shields.io/hackage/v/derive.svg?label=Hackage)](https://hackage.haskell.org/package/derive) [![Build Status](https://img.shields.io/travis/ndmitchell/derive.svg)](https://travis-ci.org/ndmitchell/derive)

**Warning: This package has no official maintainer anymore. Use at your own risk. You may wish to consider the built-in mechanism [`GHC.Generics`](https://hackage.haskell.org/package/base-4.11.1.0/docs/GHC-Generics.html) or libraries such as [`generic-deriving`](https://hackage.haskell.org/package/generic-deriving).**

Data.Derive is a library and a tool for deriving instances for Haskell programs. It is designed to work with custom derivations, SYB and Template Haskell mechanisms. The tool requires GHC, but the generated code is portable to all compilers. We see this tool as a competitor to <a href="http://repetae.net/computer/haskell/DrIFT/">DrIFT</a>.

This document proceeds as follows:

* Obtaining and Installing Data.Derive
* Supported Derivations
* Using the Derive Program
* Using Template Haskell Derivations
* Writing a New Derivation

### Acknowledgements

Thanks to everyone who has submitted patches and given assistance, including: Twan van Laarhoven, Spencer Janssen, Andrea Vezzosi, Samuel Bronson, Joel Raymont, Benedikt Huber, Stefan O'Rear, Robin Green, Bertram Felgenhauer.


## Obtaining and Installing Data.Derive

Installation follows the standard pattern of any Haskell library or program, type <tt>cabal update</tt> to update your local hackage database, then <tt>cabal install derive</tt> to install Derive.


## Supported Derivations

Data.Derive is not limited to any prebuild set of derivations, see later for howto add your own. Out of the box, we provide instances for the following libraries.

<!--
-- GENERATED START
-->

* **[Arbitrary](http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck.html#t%3AArbitrary)** - from the library [QuickCheck](http://hackage.haskell.org/package/QuickCheck)
* **[ArbitraryOld](http://hackage.haskell.org/packages/archive/QuickCheck/1.2.0.0/doc/html/Test-QuickCheck.html#t%3AArbitraryOld)** - from the library [QuickCheck-1.2.0.0](http://hackage.haskell.org/package/QuickCheck-1.2.0.0)
* **[Arities](http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-Class-Arities.html#t%3AArities)** - from the library [derive](http://hackage.haskell.org/package/derive)
* **[Binary](http://hackage.haskell.org/packages/archive/binary/latest/doc/html/Data-Binary.html#t%3ABinary)** - from the library [binary](http://hackage.haskell.org/package/binary)
* **[BinaryDefer](http://hackage.haskell.org/packages/archive/binarydefer/latest/doc/html/Data-Binary-Defer.html#t%3ABinaryDefer)** - from the library [binarydefer](http://hackage.haskell.org/package/binarydefer)
* **[Bounded](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t%3ABounded)** - from the library [base](http://hackage.haskell.org/package/base)
* **[DataAbstract](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Data.html#t%3ADataAbstract)** - from the library [base](http://hackage.haskell.org/package/base)
* **[Default](http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-Class-Default.html#t%3ADefault)** - from the library [derive](http://hackage.haskell.org/package/derive)
* **[EnumCyclic](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t%3AEnum)** - from the library [base](http://hackage.haskell.org/package/base)
* **[Fold](http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-Fold.html)**
* **[From](http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-From.html)**
* **[Has](http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-Has.html)**
* **[Is](http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-Is.html)**
* **[JSON](http://hackage.haskell.org/packages/archive/json/latest/doc/html/Text-JSON.html#t%3AJSON)** - from the library [json](http://hackage.haskell.org/package/json)
* **[LazySet](http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-LazySet.html)**
* **[Lens](http://hackage.haskell.org/packages/archive/data/lens/doc/html/Data-Lens-Common.html#t%3ALens)** - from the library [data-lens](http://hackage.haskell.org/package/data-lens)
* **[Monoid](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Monoid.html#t%3AMonoid)** - from the library [base](http://hackage.haskell.org/package/base)
* **[NFData](http://hackage.haskell.org/packages/archive/deepseq/latest/doc/html/Control-DeepSeq.html#t%3ANFData)** - from the library [deepseq](http://hackage.haskell.org/package/deepseq)
* **[Ref](http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-Ref.html)**
* **[Serial](http://hackage.haskell.org/packages/archive/smallcheck/latest/doc/html/Test-SmallCheck.html#t%3ASerial)** - from the library [smallcheck](http://hackage.haskell.org/package/smallcheck)
* **[Serialize](http://hackage.haskell.org/packages/archive/cereal/latest/doc/html/Data-Serialize.html#t%3ASerialize)** - from the library [cereal](http://hackage.haskell.org/package/cereal)
* **[Set](http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-Set.html)**
* **[UniplateDirect](http://hackage.haskell.org/packages/archive/uniplate/latest/doc/html/Data-Generics-Uniplate-Direct.html#t%3AUniplateDirect)** - from the library [uniplate](http://hackage.haskell.org/package/uniplate)
* **[UniplateTypeable](http://hackage.haskell.org/packages/archive/uniplate/latest/doc/html/Data-Generics-Uniplate-Typeable.html#t%3AUniplateTypeable)** - from the library [uniplate](http://hackage.haskell.org/package/uniplate)
* **[Update](http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-Update.html)**

<!--
-- GENERATED STOP
-->

## Using the Derive program

Let's imagine we've defined a data type:

    data Color = RGB Int Int Int
               | CMYK Int Int Int Int
               deriving (Eq, Show)

Now we wish to extend this to derive <tt>Binary</tt> and change to defining <tt>Eq</tt> using our library. To do this we simply add to the <tt>deriving</tt> clause.

    data Color = RGB Int Int Int
               | CMYK Int Int Int Int
               deriving (Show {-! Eq, Binary !-})

Or alternatively write:

    {-!
    deriving instance Eq Color
    deriving instance Binary Color
    !-}

Now running <tt>derive</tt> on the program containing this code will generate appropriate instances. How do you combine these instances back into the code? There are various mechanisms supported.

### Appending to the module

One way is to append the text to the bottom of the module, this can be done by passing the <tt>--append</tt> flag. If this is done, Derive will generate the required instances and place them at the bottom of the file, along with a checksum. Do not modify these instances.

### As a GHC preprocessor

To use Derive as a GHC preprocessor, add the following line at the top of the source file:

    {-# OPTIONS_GHC -F -pgmFderive -optF-F #-}

This instructs GHC to apply a preprocessor (<tt>-F</tt>), and to use the preprocessor <tt>derive -F</tt>.

### Using CPP

One way is to use CPP. Ensure your compiler is set up for compiling with the C Pre Processor. For example:

    {-# LANGUAGE CPP #-}
    {-# OPTIONS_DERIVE --output=file.h #-}

    module ModuleName where
    
    #include "file.h"

### Side-by-side Modules

If you had Colour.Type, and wished to place the Binary instance in Colour.Binary, this can be done with:

    {-# OPTIONS_DERIVE --output=Binary.hs --module=Colour.Binary --import #-}

Here you ask for the output to go to a particular file, give a specific module name and import this module. This will only work if the data structure is exported non-abstractly.

## Using Template Haskell Derivations</h2>

One of Derive's advantages over DrIFT is support for <a href="http://www.haskell.org/th/">Template Haskell</a> (abbreviated TH).  Derive can be invoked automatically during the compilation process, and transparently supports deriving across module boundaries. The main disadvantage of TH-based deriving is that it is only portable to compilers that support TH; currently that is GHC only.

To use the TH deriving system, with the same example as before:

    {-# LANGUAGE TemplateHaskell #-}
    import Data.DeriveTH
    import Data.Binary
    
    data Color = RGB Int Int Int
               | CMYK Int Int Int Int
               deriving (Show)
    
    $( derive makeEq ''Color )
    $( derive makeBinary ''Color )

We need to tell the compiler to insert the instance using the TH <em>splice</em> construct, <tt>$( ... )</tt> (the spaces are optional).  The splice causes the compiler to run the function <tt>derive</tt> (exported from <tt>Data.DeriveTH</tt>), passing arguments <tt>makeFooBar</tt> and <tt>''Color</tt>.  The second argument deserves more explanation; it is a quoted symbol, somewhat like a quoted symbol in Lisp and with deliberately similar syntax.  (Two apostrophes are used to specify that this name is to be resolved as a type constructor; just <tt>'Color</tt> would look for a <i>data</i> constructor named <tt>Color</tt>.)

## Writing a New Derivation

There are two methods for writing a new derivation, guessing or coding. The guessing method is substantially easier if it will work for you, but is limited to derivations with the following properties:

* Inductive - each derivation must be similar to the previous one. <tt>Binary</tt> does not have this property as a 1 item derivation does not have a tag, but a 2 item derivation does.
* Not inductive on the type - it must be an instance for the constructors, not for the type. <tt>Typeable</tt> violates this property by inducting on the free variables in the data type.
* Not type based - the derivation must not change based on the types of the fields. <tt>Play</tt> and <tt>Functor</tt> both behave differently given differently typed fields.
* Not record based - the derivation must not change on record fields. <tt>Show</tt> outputs the fields, so this is not allowed.

If however your instance does meet these properties, you can use derivation by guess. Many instances do meet these conditions, for examples see: <tt>Eq</tt>, <tt>Ord</tt>, <tt>Data</tt>, <tt>Serial</tt> etc. If however you need to code the derivation manually see examples such as <tt>Update</tt> and <tt>Functor</tt>.

### Modifying Derive

The standard sequence for testing Derive is:

    $ ghci Main.hs
    :main --generate
    :reload
    :main --test

The `--generate` option will automatically generate DSL's for derivations derived by example. The `--test` option runs all test comparisons and then loads the file with Template Haskell.

### Coding a new derivation

My best suggestion, start with a similar instance, i.e. to make `Eq2` from `Eq` do:

* Copy `Data/Derive/Eq.hs` to `Data/Derive/Eq2.hs`
* Rename some of the bits in `Eq2.hs` from `Eq`
* `ghci` -- load derive
* `:main` --generate    -- this adds Eq2.hs to the .cabal/All.hs files etc
* `:reload`             -- reload with Eq2.hs

Now fix up `Eq2.hs` appropriately.
