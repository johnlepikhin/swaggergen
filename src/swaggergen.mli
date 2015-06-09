
type 'a t

type path = string * [ `Path ] t option

type contact = [ `Contact ] t

type license = [ `License ] t

type info = [ `Info ] t

type parameter = [ `Parameter ] t

type parameters = [ `Parameter ] t list

type schema = [ `Schema ] t

type response = [ `Response ] t

type externalDocs = [ `ExternalDocs ] t

type security = [ `Security ] t

type headers = [ `Headers ] t

type responseExamples = [ `ResponseExamples ] t

type operation = [ `Operation ] t

type definition = [ `Definition ] t

type tag = [ `Tag ] t

type securityScheme = [ `SecurityScheme ] t

type swagger = [ `Swagger ] t


val contact : string -> string -> string -> contact

val license : ?url:string -> string -> license

val info :
  ?description:string ->
  ?termsOfService:string ->
  ?contact:contact ->
  ?license:license -> string -> string -> info

module Schema :
  sig
		type s_string = {
			string_format : string option;
		}

		val string_defaults : s_string

		type s_integer = {
			integer_format : string option;
		}

		val integer_defaults : s_integer

    type property

    type st

    val s_string : ?content:s_string -> ?title:string -> ?description:string -> unit -> st

    val s_integer : ?content:s_integer -> ?title:string -> ?description:string -> unit -> st

    val s_property : ?required:bool -> string -> st -> property

    val s_object : property list -> ?title:string -> ?description:string -> unit -> st

	 val s_array : st -> ?title:string -> ?description:string -> unit -> st

    val to_schema : st -> schema
  end

module Parameter :
	sig
    type general
    type body
    type pt

    val s_body : schema -> body

    val s_general : string -> general

    val query : general -> name:string -> ?description:string -> ?required:bool -> unit -> pt

    val header : general -> name:string -> ?description:string -> ?required:bool -> unit -> pt

    val path : general -> name:string -> ?description:string -> ?required:bool -> unit -> pt

    val formData : general -> name:string -> ?description:string -> ?required:bool -> unit -> pt

    val body : body -> name:string -> ?description:string -> ?required:bool -> unit -> pt

	 val to_t : pt -> parameter
	end

val addresponse :
  ?code:[< `Code of int | `Default > `Default ] ->
  ?schema:schema ->
  ?headers:headers ->
  ?examples:responseExamples ->
  string ->
  (string * (response option)) list -> (string * (response option)) list

val operation :
  ?tags:string list ->
  ?summary:string ->
  ?description:string ->
  ?externalDocs:externalDocs ->
  ?operationId:string ->
  ?consumes:string list ->
  ?produces:string list ->
  ?parameters:parameters ->
  ?schemes:string list ->
  ?deprecated:bool ->
  ?security:security ->
  (string * (response option)) list ->
  operation

val addpath :
  string ->
  ?reference:string ->
  ?get:operation ->
  ?put:operation ->
  ?post:operation ->
  ?delete:operation ->
  ?options:operation ->
  ?head:operation ->
  ?patch:operation ->
  ?parameters:parameters ->
  path list -> path list

val swagger :
  ?host:string ->
  ?basePath:string ->
  ?schemes:string list ->
  ?consumes:string list ->
  ?produces:string list ->
  ?definitions:definition list ->
  ?parameters:parameters ->
  ?responses:(string * ([ `Response | `Definition ] t option)) list ->
  ?securityDefinitions:(string * securityScheme option) list ->
  ?security:security list ->
  ?tags:tag list ->
  ?externalDocs:externalDocs ->
  info ->
  path list ->
  swagger

val to_json : swagger -> Yojson.json
