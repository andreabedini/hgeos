# v0.1.7.3

* Adds createCollection
* Adds createEmptyPolygon

# v0.1.7.2

* Fixes #5: Passing holes to createPolygon results in double free

# v0.1.7.1

* Removes problematic helpers.h

# v0.1.7.0

* Implements getOrdinate and setOrdinate
* Implements createPolygon

# v0.1.6.0

* Internal refactorings
* Add createCoordSeq et al

# v0.1.5.1

* Improves Windows build support by including binary distribution of GEOS
* Embeds GEOS 3.5.0-1 on Windows
* Change "Portability" to "portable"

# v0.1.5.0

* Adds runGEOSEither

# v0.1.4.0

* Fixes some documentation issues
* Adds MaybeT-based API and samples
* Renames GEOSContextHandle_t to GEOSContextHandle

# v0.1.3.0

* Rearranges test sources
* Adds proper handling to high-level API
* Renames high-level API functions

# v0.1.2.0

* Tweaks to documentation
* Implements accessors for coordinates from coordinate sequences
* Expands coverage of API
* Extends examples
* Adds "version" function to report GEOS API version
* Adds links to full code samples
* Renames "withContext" to "withGEOS"

# v0.1.1.0

* Adds link to documentation
* Improves package metadata
* Fixes leak in "intersection"
* Consolidates tracking of geometries
* Adds support for envelope, exterior ring, coordinate sequences

# v0.1.0.1

* Makes exports from Imports module explicit
* Adds missing src/lib/helpers.h to source distribution

# v0.1.0.0

* Initial version with imports and high-level API
