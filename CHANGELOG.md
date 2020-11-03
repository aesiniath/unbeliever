* **core-text** _v0.3.0_  
       Change Render typeclass so that `colourize` uses new built-in ANSI
       colour styles. `intoDocA` renamed `highlight`.

----

* _v0.10.0_  
       Split this into various subpackages, initially **core-text**,
       **core-data**, and **core-program**.

* _v0.9.0_  
       Various function renames. Add -`Rope` suffix to `width`, `insert`,
       etc and the `append` method of Textual. Makes it more uniform both
       within the API for Rope and consistent with Map and Set.

* _v0.8.0_  
       Move `write*` functions to Core.Program.Logging. Essentially a
       cosmetic change, but allows us to document the unified output/
       logging mechanism in one place.

* _v0.7.3_  
       Initial public release
