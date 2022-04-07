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
- [x] Store query results https://package.elm-lang.org/packages/andre-dietrich/elm-generic/latest/Generic#Value
- [x] Tables/FieldViewer from Query Results
- [x] argToFormField - lookup enum values (show fields for any kind of TypeReference actually)
- [x] inputScalarOrEnum should be renamed to inputFromTypeRef
- [x] Move case Type.InputObjectRef objectClassCaseName to inputFromTypeRef
- [x] Currently set value needs to be displayed in the form input
- [ ] typeRefToArgumentType - handle all types
- [ ] inputFromTypeRef - Handle all types
- [x] argToFormField - send additional type information to UpdateFormInput
- [x] Handle enum types instead of just string (i.e. GraphQl.string) 
- [ ] TODO: UnionRef
- [ ] Displaying UnionRef tables is probably wrong -- need to account for all fields first when constructing the header of the tables and then displaying the rows should be based on a list of fields, 
- [ ] TODO: Handle Lists in QueryArgument
- [x] Mutations
- [x] Change Modal to a whole page
- [ ] Prettify Camel Case, Snake Case, and all Caps to more human-readable output via a function 
- [x] Make the URL editable
- [ ] Use the typeRef of the argument for displaying the form field
- [ ] Use the typeRef of the argument for storing the form field
- [x] Handle Mutations
- [x] Use the nullability when displaying the form field
- [x] Use the nullability when inputting the form field
- [x] Use the nullability when making the GraphQL query
- [ ] Use RemoteData package https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/
- [x] Get rid of config.json and instead pass it through flags so that it's runtime-configurable
- [ ] TODO: EverySet type?
- [x] Use GraphQL Descriptions in Form Inputs and maybe Table tooltips
- [ ] Don't allow "Run Query" button to be clickable until all required arguments are met

## Edit View
- [ ] Config flag to set edit_mode = true
- [ ] Display the JSON of the config in edit mode
- [ ] On edit mode, certain config fields are editable directly

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
- Endpoint
- Headers
- Queries and parameters to execute on startup
- GraphQL Queries to Views (with Buttons)
- Buttons to Form Field prepopulation + named Mutation (or Query)
    - Form Submit executes Mutation

## CSS
- Bulma/Bootstrap Class Names for HTML elements as well as custom class names for each element (so they can be overridden)

## Debugging
```
data.query.replace(/\n/g,' ')
```