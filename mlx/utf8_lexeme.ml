type validation_result =
  | Valid
  | Invalid_character of Uchar.t
  | Invalid_beginning of Uchar.t

let normalize s = Ok s

let validate_identifier ?with_dot:_ _name = Valid

let is_lowercase s =
  String.length s > 0 &&
  let c = Char.code s.[0] in
  (c >= 97 && c <= 122) || c = 95

let is_capitalized s =
  String.length s > 0 &&
  let c = Char.code s.[0] in
  c >= 65 && c <= 90
