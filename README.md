# graphql-ui
Autogenerate a user interface (UI) for queries and mutations from a GraphQL schema using introspection

Reimplementation of https://github.com/4Catalyzer/graphql-explorer

# Goals
- No backend -- the only backend is the GraphQL endpoint
- Loadable via CDN `<script>` tag 
- Configurable using Elm flags https://guide.elm-lang.org/interop/flags.html

# TODO

## Startup
- [x] Specify a GraphQL endpoint in a JSON config
- [x] Read the JSON config into Elm
- [x] Store introspection query as a const
- [x] Send introspection query in query body
- [x] Use the decoder from https://github.com/dillonkearns/elm-graphql/blob/master/generator/src/Graphql/Parser.elm#L13-L15

## Initial Models
- [x] Model Types
- [x] Model Queries
- [x] Model Mutations

## Basic Functionality
- [x] Hit GraphQL endpoint with a query using code from https://package.elm-lang.org/packages/ghivert/elm-graphql/latest/
- [x] List Queries
- [x] List Mutations
- [x] Forms from Query/Mutation Arguments
- [x] Store form values in a Dict QueryName (Dict FieldName String)
- [x] Form fields to GraphQL Query/Mutation
- [x] Add most of the fields to the request
- [ ] Store query results https://package.elm-lang.org/packages/andre-dietrich/elm-generic/latest/Generic#Value
- [ ] Tables/FieldViewer from Query Results
- [ ] Use the typeRef of the argument for displaying the form field
- [ ] Use the typeRef of the argument for storing the form field
- [ ] Use the nullability when displaying the form field
- [ ] Use the nullability when inputting the form field
- [ ] Use the nullability when making the GraphQL query
- [ ] TODO: UnionRef
- [ ] Additional types
- [ ] Use RemoteData package https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/

## Linking UI Elements
- [ ] List (Tab, Form)
- [ ] Maybe Form -> Query -> View
- [ ] View -> Form
- [ ] Fields -> DisplayView
- [ ] DisplayView -> FormView population
- [ ] FormView from Mutation Parameters
- [ ] Run mutation on FormView submit

## Advanced
- [ ] Verify config against GraphQL endpoint

## Configs
- GraphQL Queries to Views (with Buttons)
- Buttons to Form Field prepopulation + named Mutation (or Query)
    - Form Submit executes Mutation

## CSS
- Bulma/Bootstrap Class Names for HTML elements as well as custom class names for each element (so they can be overridden)

## Debugging
```
data.query.replace(/\n/g,' ')
```