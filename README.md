# hs-yesod-model-generator

This library helps to generate parts of a Yesod CRUD application. Starting point is a defined haskell data structure that contains all infos of the model: its database fields, create/update form fields, the validations for the forms, the rendering of the forms themselves, form CSS infos and field labels in german and english.

The generated forms are [monadic forms](https://www.yesodweb.com/book/forms)

The haskell code is generated with the help of the [ginger template engine](http://hackage.haskell.org/package/ginger)

## Generated parts

* form to create/update/delete a persistent a model including validations
* form for generic actions including validations (like sending a test meil to the admin)
* models database definition quasiquotes (in config/models)
* postgresql triggers to populate history tables
* translations for I18N that can be used in haskell code or to send it to react-like frontends

# Examples

* [model definitions sample](https://github.com/agreif/hs-editorial-cms/blob/master/hs-generator/src/Main.hs)
* [generated User model example](https://github.com/agreif/hs-editorial-cms/blob/master/src/Handler/User.hs)
