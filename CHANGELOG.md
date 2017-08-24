## 0.3.0.5.9005

* Return `NULL` in registry getting regardless of error.

## 0.3.0.5.9004

* Fix usage of `base` parameter in `$find` method with idempotent resources.

## 0.3.0.5.9003

* Fix NA detection in `%|||%` operator to work with POSIXct objects.

## 0.3.0.5.9001-9002

* Added the `helper` inject to the preprocessor.
* Added a typecheck to the preprocessor `source` inject.

## 0.3.0.5.9000

* Started development version and fixed issue with modification
  tracking on virtual resources.

## 0.3.0.5

 * Fix an issue where sourcing a virtual resource with a preprocessor
   that fails on a `source()` gives an non-descriptive error.
