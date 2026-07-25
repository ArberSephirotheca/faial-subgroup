let prefix : string = "$read_"

let name (array : Variable.t) : string = prefix ^ Variable.name array
