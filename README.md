# hs-yesod-model-generator

This library hels to generate parts of a Yesod CRUD application. Startingpoint is a defined haskell data structure that contains all infos for the model: its database fields, create/update form fields, the validations for the forms and the rendering of the forms themselves.

The generated forms are [monadic forms](https://www.yesodweb.com/book/forms)

The haskell code is generated with the help of the [ginger template engine](http://hackage.haskell.org/package/ginger)

## Generated parts

* form to create/update/delete a persistent a model including validations
* form for generic actions including validations (like sending a test meil to the admin)
* models database definition quasiquotes (in config/models)
* postgresql triggers to populate history tables
* translations for I18N that can be used in haskell code or to send it to react-like frontends
