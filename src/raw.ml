type range = {start:int; stop:int}
type substring = { content:string; range:range }

class virtual t = object(self:'self)
    method virtual string: substring -> 'self
    method virtual space: int -> 'self
    method virtual indent: Geometry.Indentation.t -> 'self
    method virtual break: 'self
end


let full s = { start = 0; stop=String.length s }
let all s = { content=s; range=full s }


class buffer b =
  object(self)
    inherit t
    val b = b
    method string {content;range} =
      Buffer.add_substring b content range.start (range.stop - range.start);
      self
    method space n = for _ = 1 to n do
      Buffer.add_char b ' '
    done;
      self
    method indent x = self#space x.Geometry.Indentation.column
    method break = Buffer.add_char b '\n'; self
  end


class chan ch =
  object(self)
    val ch= ch
    inherit t
    method string {content;range} =
      output_substring ch content range.start (range.stop-range.start); self
    method space n = for _ = 1 to n do
      output_string ch " "
    done; self
    method break = output_string ch "\n"; self
    method indent n = self#space n.column
  end
