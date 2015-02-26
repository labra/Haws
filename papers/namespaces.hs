module Namespaces where
import RDF

xsd = "http://www.w3.org/2001/XMLSchema#"
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
owl = "http://www.w3.org/2002/07/owl#"
foaf = "http://xmlns.com/foaf/0.1/"

rdf_type = URI (rdf ++ "type")
xsd_string = URI (xsd ++ "string")
xsd_integer = URI (xsd ++ "integer")

