let substr_index (str: string) (delimiter: string) (index: int) =
    let split = Str.split (Str.regexp delimiter) str in
    let length = Array.length (Array.of_list split) in
    if index >= length || index < 0 then ""
    else List.nth (Str.split (Str.regexp delimiter) str) index
