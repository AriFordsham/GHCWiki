This document is a living, incremental style guide targeted at contributors to the `base` library's documentation. Its first and main purpose is to standardise our current practices when it comes to documentation writing.

This guide is not a language guide on the English language. The writer should refer to the New Oxford Style Manual, the Chicago Manual of Style, or any other professional reference for any concerns pertaining to writing in the English language.

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL
NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and
"OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

## British vs. American spelling

Being an international community of contributors, many of whom coming from English-speaking countries, there are bound to be different spelling standards living with each-other.  

* If editing an existing document, consistency must be favoured. If the document is inconsistent, an arbitrary spelling can be picked in order to harmonise the document.
* When creating a new document, the kind of spelling should be the one the author(s) are most familiar with, as to avoid potential irregularities.

## Greek letters for types, Latin letters for terms

The "Greek letters for types, Latin letters for terms" (GLT,LLT) convention, whilst in use in some circles, is not as widespread as one may think. In particular, it can be harder for the reader to deal with another alphabet if Latin is not their first one.

GLT,LLT can be used in a document, under those conditions:

* The document must contain right after its introductory or summary paragraph a list of the Greek letters used below and their Latin names.
* The use of GLT,LLT must be consistent throughout the document.

Current usage has :

| Greek letter | Latin name | Usage / Position | Example       |
| ------------ | ---------- | ---------------- | ------------- |
| α            | Alpha      | Concrete type    | id :: α -> α  |
| β            | Beta       | Concrete type    | swap :: (α, β) -> (β, α)
| τ            | Tau        | Functor τ        | foldr  :: Foldable τ => (α -> β -> β) -> β -> τ α -> β

Due to its usage and history, λ (Lambda) should be avoided in order to prevent confusion.

For ease of input, Greek letters can be written as HTML codes if the section that holds them will not be rendered outside of a browser. Introductory / summary paragraphs are expected to be rendered in non-HTML environments such as GHCi, and thus should contain Unicode characters instead of their HTML code counterparts.

## Module documentation

Module documentation should contain an `Overview` section that describes in greater length the purpose, APIs and underlying concepts exposed by the module.
This section is typically not rendered through GHCi's `:doc` due to the particularities of a REPL's environment. That is why modules should always have a smaller, more compact introductory / summary paragraph that will be the first point of contact with the developer when they look modules up in the REPL. 


## Sources

### Papers

References to published papers should contain a direct URL to the document, or if unavailable, to the paper's page on the publisher website. It is discouraged to reference papers that are behind paywalls. The Digital Object Identifier (DOI) may be given as well in order to enable the reader's ability to find the paper online should the provided URL suffer from bitrot.