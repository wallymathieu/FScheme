module FScheme.World.Core

open System.Drawing
open System.Numerics
open System.Threading
open System.Collections.Generic
open FScheme
type WorldState(refresh:System.Action)=
  let mutable history = new LinkedList<Expression>()
  let mutable current = new LinkedListNode<Expression>(Symbol("Dummy world"))
  
  let mouseX = ref 0
  let mouseY = ref 0
  let events () = List([Number(bigint mouseX.Value); Number(bigint mouseY.Value)])
  
  let eval' name = eval id environment (List([Symbol(name); List([Symbol("quote"); List([current.Value; events ()])])]))
  
  let w = 32
  let h = 32
  let s = 8 // pixel size
  let bmp = new Bitmap(w * s, h * s)
  let running = ref false

  do
    current <- history.AddFirst(eval' "init")
    running := true


  member this.Paint () = lock bmp (fun () ->
      use gc = Graphics.FromImage(bmp)
      gc.Clear(Color.Black) |> ignore
      match eval' "draw" with
      | List(pixels) ->
          let fill = function
              | List([List([Number(x); Number(y)]); List([Number(r); Number(g); Number(b)])]) ->
                  gc.FillRectangle(new SolidBrush(Color.FromArgb(0xFF, int r, int g, int b)), int x * s, int y * s, s - 1, s - 1)
              | _ -> ()
          List.iter fill pixels
      | _ -> failwith "Malformed graphical output.")

  member this.Time slice =
      running := false
      if slice <> null then
          current <- slice
          this.Paint (); refresh.Invoke()

  member this.ThreadStart()=
      while true do
          if running.Value then
              current <- history.AddAfter(current, eval' "tick")
              this.Paint (); refresh.Invoke()
          Thread.Sleep(33)

  // f.Paint.Add(fun a -> GetBitmap(fun bmp -> a.Graphics.DrawImage(bmp, 0, 0)))
  member this.GetBitmap (a:System.Action<Bitmap>) =lock bmp (fun () -> a.Invoke(bmp))

(*
    let debug = function
        | Keys.S -> running := false
        | Keys.G -> running := true
        | Keys.Left -> time current.Previous
        | Keys.Right -> time current.Next
        | Keys.W -> print current.Value |> printfn "World: %s"
        | _ -> ()
    f.MouseMove.Add(fun a -> mouseX := a.X / s; mouseY := a.Y / s)
    f.KeyDown.Add(fun a -> debug a.KeyCode)
    f.Closing.Add(fun _ -> t.Abort())
*)
