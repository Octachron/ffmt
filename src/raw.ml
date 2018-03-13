type range = {start:int; stop:int}
type substring = { content:string; range:range }

class virtual['reduced] t = object(_self:'self)
    method virtual string: substring -> 'self
    method len {range;_} = range.stop - range.start
    method virtual space: int -> 'self
    method virtual indent: Geometry.Indentation.t -> 'self
    method virtual break: 'self
    method virtual flush: 'reduced
end


let full s = { start = 0; stop=String.length s }
let all s = { content=s; range=full s }


class buffer b =
  object(self)
    inherit [string]t
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
    method flush = Buffer.contents b
  end


class chan ch =
  object(self)
    val ch= ch
    inherit [unit]t
    method string {content;range} =
      output_substring ch content range.start (range.stop-range.start); self
    method space n = for _ = 1 to n do
      output_string ch " "
    done; self
    method break = output_string ch "\n"; self
    method indent n = self#space n.column
    method flush = Pervasives.flush ch
  end

type symbolic =
  | String of substring
  | Space of int
  | Break
  | Indent of Geometry.Indentation.t

class free =
  object
    inherit [symbolic list]t
    val history = []
    method string s = {< history = String s :: history >}
    method indent n = {< history = Indent n :: history >}
    method break = {< history = Break :: history >}
    method space n = {< history = Space n :: history >}
    method flush = history
  end
