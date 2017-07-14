namespace Cellz

type UnitType =
    | Empty
    | Unit of string * int
    | CompositeUnit of UnitType list
    static member Create(s,n) =
        if n = 0 then Empty else Unit(s,n)
    override this.ToString() =
        let exponent = function
            | Empty -> 0
            | Unit(_,n) -> n
            | CompositeUnit(_) -> invalidOp ""
        let rec toString = function
            | Empty -> ""
            | Unit(s,n) when n=0 -> ""
            | Unit(s,n) when n=1 -> s
            | Unit(s,n)          -> s + " ^ " + n.ToString()
            | CompositeUnit(us) ->
                let ps, ns =
                    us |> List.partition (fun u -> exponent u >= 0)
                let join xs =
                    let s = xs |> List.map toString |> List.toArray
                    System.String.Join(" ",s)
                match ps,ns with
                | ps, [] -> join ps
                | ps, ns ->
                    let ns = ns |> List.map UnitType.Reciprocal
                    join ps + " / " + join ns
        match this with
        | Unit(_,n) when n < 0 -> " / " + (this |> UnitType.Reciprocal |> toString)
        | _ -> toString this
    static member ( * ) (v:ValueType,u:UnitType) = UnitValue(v,u)
    static member ( * ) (lhs:UnitType,rhs:UnitType) =
        let text = function
            | Empty -> ""
            | Unit(s,n) -> s
            | CompositeUnit(us) -> us.ToString()
        let normalize us u =
            let t = text u
            match us |> List.tryFind (fun x -> text x = t), u with
            | Some(Unit(s,n) as v), Unit(_,n') ->
                us |> List.map (fun x -> if x = v then UnitType.Create(s,n+n') else x)
            | Some(_), _ -> raise (System.NotImplementedException())
            | None, _ -> us@[u]
        let normalize' us us' =
            us' |> List.fold (fun (acc) x -> normalize acc x) us
        match lhs,rhs with
        | Unit(u1,p1), Unit(u2,p2) when u1 = u2 ->
            UnitType.Create(u1,p1+p2)
        | Empty, _ -> rhs
        | _, Empty -> lhs
        | Unit(u1,p1), Unit(u2,p2) ->
            CompositeUnit([lhs;rhs])
        | CompositeUnit(us), Unit(_,_) ->
            CompositeUnit(normalize us rhs)
        | Unit(_,_), CompositeUnit(us) ->
            CompositeUnit(normalize' [lhs]  us)
        | CompositeUnit(us), CompositeUnit(us') ->
            CompositeUnit(normalize' us us')
        | _,_ -> raise (System.NotImplementedException())
    static member Reciprocal x =
        let rec reciprocal = function
            | Empty -> Empty
            | Unit(s,n) -> Unit(s,-n)
            | CompositeUnit(us) -> CompositeUnit(us |> List.map reciprocal)
        reciprocal x
    static member ( / ) (lhs:UnitType,rhs:UnitType) =
        lhs * (UnitType.Reciprocal rhs)
    static member ( + ) (lhs:UnitType,rhs:UnitType) =
        if lhs = rhs then lhs
        else invalidOp "Unit mismatch"
and ValueType = decimal
and UnitValue (v:ValueType,u:UnitType) =
    new(v:ValueType) = UnitValue(v,Empty)
    new(v:ValueType,s:string) = UnitValue(v,Unit(s,1))
    member this.Value = v
    member this.Unit = u
    override this.ToString() =
        match u with
        | Empty -> v.ToString()
        | _ -> sprintf "%O %O" v u
    static member (~-) (v:UnitValue) =
        UnitValue(-v.Value,v.Unit)
    static member (+) (lhs:UnitValue,rhs:UnitValue) =
        UnitValue(lhs.Value+rhs.Value, lhs.Unit+rhs.Unit)
    static member (-) (lhs:UnitValue,rhs:UnitValue) =
        UnitValue(lhs.Value-rhs.Value, lhs.Unit+rhs.Unit)
    static member (*) (lhs:UnitValue,rhs:UnitValue) =
        UnitValue(lhs.Value*rhs.Value,lhs.Unit*rhs.Unit)
    static member (*) (lhs:UnitValue,rhs:ValueType) =
        UnitValue(lhs.Value*rhs,lhs.Unit)
    static member (*) (v:UnitValue,u:UnitType) =
        UnitValue(v.Value,v.Unit*u)
    static member (/) (lhs:UnitValue,rhs:UnitValue) =
        UnitValue(lhs.Value/rhs.Value,lhs.Unit/rhs.Unit)
    static member (/) (lhs:UnitValue,rhs:ValueType) =
        UnitValue(lhs.Value/rhs,lhs.Unit)
    static member (/) (v:UnitValue,u:UnitType) =
        UnitValue(v.Value,v.Unit/u)
    static member Pow (lhs:UnitValue,rhs:UnitValue) =
        let isInt x = 0.0M = x - (x |> int |> decimal)
        let areAllInts =
            List.forall (function (Unit(_,p)) -> isInt (decimal p*rhs.Value) | _ -> false)
        let toInts =
            List.map (function (Unit(s,p)) -> Unit(s, int (decimal p * rhs.Value)) | _ -> invalidOp "" )
        match lhs.Unit, rhs.Unit with
        | Empty, Empty ->
            let x = (float lhs.Value) ** (float rhs.Value)
            UnitValue(decimal x)
        | _, Empty when isInt rhs.Value ->
            pown lhs (int rhs.Value)
        | Unit(s,p1), Empty when isInt (decimal p1*rhs.Value) ->
            let x = (float lhs.Value) ** (float rhs.Value)
            UnitValue(x |> decimal, Unit(s,int (decimal p1*rhs.Value)))
        | CompositeUnit us, Empty when areAllInts us ->
            let x = (float lhs.Value) ** (float rhs.Value)
            UnitValue(x |> decimal, CompositeUnit(toInts us))
        | _ -> invalidOp "Unit mismatch"
    static member One = UnitValue(1.0M,Empty)
    override this.Equals(that) =
        let that = that :?> UnitValue
        this.Unit = that.Unit && this.Value = that.Value
    override this.GetHashCode() = hash this
    interface System.IComparable with
        member this.CompareTo(that) =
            let that = that :?> UnitValue
            if this.Unit = that.Unit then
                if this.Value < that.Value then -1
                elif this.Value > that.Value then 1
                else 0
            else invalidOp "Unit mismatch"