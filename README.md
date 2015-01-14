# json-schema-codegen

Model and Serialization source code generator from a defined Json-Schema.


Scala code generator supports the following of Json-schema spec:

 * Object types with properties

 * required and optional properties

 * Object types with additionProperties

 * Arrays of any type

 * Unique or non-unique arrays

 * Simple types

 * Type definitions

 * Enum of string and integer values

 * Formats - uri, date-time, email, hostname, ipv4, ipv6

Not supported:

 * anyOf, not, allOf, oneOf

 * multiple types

 * enum of objects or variable types

 * validation constructs that do not affect the structures