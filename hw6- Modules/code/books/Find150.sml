structure Find150 :> sig val is150Book : Book.page list -> bool end =
struct

  (* is150Book : Book.page list -> bool
   * REQUIRES: true
   * ENSURES:
   *   is150Book b = true iff b contains at least one instance of
   *   the word "lambda" and no instances of the word "pointer"
   *)

   structure Honk = MkNemesis (MkStudent (val count = 1 val favoriteWord = "pointer"))
   structure H' = MkCompose (StudentA = Honk StudentB = Reset)
   structure hR = MkReader(H')
   structure Polly = MkReader(MkStudent (val count = 1 val favoriteWord = "lambda"))

  fun is150Book b = 
    let
      val bb = Book.bind b
    in
      hR.read bb andalso Polly.read bb
    end
end
