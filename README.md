# graphql-ui
Autogenerate a user interface (UI) for queries and mutations from a GraphQL schema using introspection

Reimplementation of https://github.com/4Catalyzer/graphql-explorer

# TODO

## Startup
- [ ] Specify a GraphQL endpoint in a JSON config
- [ ] Read the JSON config into Elm
- [ ] Use introspection to get information on types, queries, and mutations

## Initial Models
- [ ] Model Types
- [ ] Model Queries
- [ ] Model Mutations

## Basic Functionality
- [ ] Hit GraphQL endpoint with a query
- [ ] Store query results in a JsonTree https://github.com/klazuka/elm-json-tree-view/blob/master/src/JsonTree.elm
- [ ] Display output

## Views
- [ ] Tables
- [ ] Forms
- [ ] Others?

## Linking UI Elements
- [ ] Fields -> DisplayView
- [ ] DisplayView -> FormView population
- [ ] FormView from Mutation Parameters
- [ ] Run mutation on FormView submit

## Advanced
- [ ] Verify config against GraphQL endpoint
