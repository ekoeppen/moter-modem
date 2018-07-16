type cbor_major_t =
  UNSIGNED_INT
  | NEGATIVE_INT
  | BYTE_STRING
  | UTF8_STRING
  | ARRAY
  | MAP
  | TAG
  | SIMPLE_OR_FLOAT
  | ERROR

let major_of_int = function
  | 0 -> UNSIGNED_INT
  | 1 -> NEGATIVE_INT
  | 2 -> BYTE_STRING
  | 3 -> UTF8_STRING
  | 4 -> ARRAY
  | 5 -> MAP
  | 6 -> TAG
  | 7 -> SIMPLE_OR_FLOAT
  | _ -> ERROR

let string_of_major = function
  | UNSIGNED_INT -> "unsigned int"
  | NEGATIVE_INT -> "negative int"
  | BYTE_STRING -> "byte string"
  | UTF8_STRING -> "utf8 string"
  | ARRAY -> "array"
  | MAP -> "map"
  | TAG -> "tag"
  | SIMPLE_OR_FLOAT -> "simple_or_float"
  | ERROR -> "error"

(* **** Low level decoding ************************************************* *)

let additional_info first =
  first land 0b00011111

let major_of_first first =
  major_of_int ((first land 0b11100000) lsr 5)

let decode_first s =
  let first = String.get s 0 |> Char.code in
  (major_of_first first, additional_info first)

let major_of_string s =
  String.get s 0 |> Char.code |> major_of_first

let decode_int additional s =
  match additional with
  | n when n < 24 -> (n, Core.String.drop_prefix s 1)
  | 24 -> (String.get s 1 |> Char.code, Core.String.drop_prefix s 2)
  | 25 ->
      ((String.get s 1 |> Char.code) lsl 8 +
       (String.get s 2 |> Char.code),
    Core.String.drop_prefix s 3)
  | 26 ->
      ((String.get s 1 |> Char.code) lsl 24 +
       (String.get s 2 |> Char.code) lsl 16 +
       (String.get s 3 |> Char.code) lsl 8 +
       (String.get s 4 |> Char.code), Core.String.drop_prefix s 5)
  | 27 -> raise (Failure "Not implemented")
  | _ -> raise (Failure "Incorrect additional information")

let decode_negative_int additional s =
  let (value, s) = decode_int additional s in
  (-1 - value, s)

let decode_byte_string additional s =
  let (length, s) = decode_int additional s in
  let str = Core.String.prefix s length in
  (str, Core.String.drop_prefix s length)

(* **** Decoding of CBOR elements ****************************************** *)

let int_of_string s =
  let (major, additional) = decode_first s in
  match major with
  | UNSIGNED_INT -> Some (decode_int additional s)
  | NEGATIVE_INT -> Some (decode_negative_int additional s)
  | _ -> None

let byte_string_of_string s =
  let (major, additional) = decode_first s in
  if major = BYTE_STRING
  then Some (decode_byte_string additional s)
  else None

let tag_of_string s =
  let (major, additional) = decode_first s in
  if major = TAG
  then Some (decode_int additional s)
  else None

let array_of_string s =
  let (major, additional) = decode_first s in
  if major = ARRAY
  then Some (decode_int additional s)
  else None

let map_of_string s =
  let (major, additional) = decode_first s in
  if major = MAP
  then Some (decode_int additional s)
  else None

let float_of_string s =
  let open Core.Option.Let_syntax in
  let (major, additional) = decode_first s in
  match major with
  | SIMPLE_OR_FLOAT -> None
  | TAG ->
      let tag, s = decode_int additional s in
      if tag == 4 then begin
        let%bind n, s = array_of_string s in
        if n == 2 then begin
          let%bind exponent, s = int_of_string s in
          let%bind mantissa, s = int_of_string s in
          Some (float_of_int mantissa *. 10.0 ** (float_of_int exponent), s)
        end
        else
          None
      end
      else
        None
  | _ -> None
