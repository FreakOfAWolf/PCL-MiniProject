module University

open System.Linq.Expressions

let scale = [1..10]

type Department(name:string) =
  member x.Name = name;
  
let IT = new Department(name="IT")
let Mechanical = new Department(name="Mechanical")
let SocialStudies = new Department(name="Social Studies")
let Marketing = new Department(name="Marketing")
let HealthCare = new Department(name="Health Care")
let NoDep = new Department(name="Dont waste your time!")


type PTraits(name:string, scale) =
  member x.Name = name;
  member x.scale = scale;

let Programming = new PTraits(name="Programming", scale = 9)
let Mathematics = new PTraits(name="Mathematics", scale = 9)
let Social = new PTraits(name="Sociology", scale = 9)
let Research = new PTraits(name="Research", scale = 6)
let Biology = new PTraits(name="Biology", scale = 7)

type NonGroupedStudent(name:string, age:int, EPoint:int, PTraits:List<PTraits>) =
  member x.Name = name;
  member x.Age = age;
  member x.EPoint = EPoint;
  member x.PTraits = PTraits;

let Steven = new NonGroupedStudent(name="Steven", age=22, EPoint=105, PTraits=[Programming; Research])
let Iva = new NonGroupedStudent(name="Iva", age= 20, EPoint=100, PTraits= [Mathematics;Social])
let Kusky = new NonGroupedStudent(name="Kusky", age=21, EPoint=110, PTraits= [Research; Biology])
let Willy = new NonGroupedStudent(name="Willy", age=24, EPoint=120, PTraits=[Programming; Biology])
let John = new NonGroupedStudent(name="John", age=22, EPoint=110, PTraits=[Research; Social])
 //   if student.PTraits.Name="Biology" && student.PTraits.scale>=9 then HealthCare
    //elif student.PTraits.Name="Research" && student.PTraits.scale>=8 then Marketing
 //   else NoDep
let findDepartment (traits:PTraits):Department =
        let result:Department=
          match traits.Name  with
                | "Biology" when traits.scale >= 9 ->HealthCare 
                | "Programming" when traits.scale >= 7 ->IT
                | "Mathematics" when traits.scale >= 7 ->Mechanical
                | "Sociology" when traits.scale >= 7 ->SocialStudies
                | "Research" when traits.scale >= 7->Marketing
                | _ -> NoDep
        result;;
                //let result:Department =student.PTraits |> Seq.map(fun x ->(findDepartment(x)))

let grouping(student : NonGroupedStudent):string= 
    let mutable result1:string = ""
    for x in student.PTraits do 
            result1 <- x.Name+": "+ result1+" - " + findDepartment(x).Name
    result1;;

let test1 = grouping(Willy)
let test2= grouping(John)
let test3= grouping(Iva)
let test4= grouping(Steven)
let test5= grouping(Kusky)


printfn "%s" test1
printfn "%s" test2
printfn "%s" test3
printfn "%s" test4
printfn "%s" test5

let ePoints =5,10

type RealStudent(Name:string, Age:int, ePoints, Department:string)=
    member x.Name = Name;
    member x.Age= Age;
    member x.EPoint = ePoints;
    member x.Department= Department;

let register(student:NonGroupedStudent):RealStudent =
    let result =
       if student.EPoint>=110 then 
          let department =grouping(student);
          RealStudent(Name=student.Name,Age=student.Age,ePoints=student.EPoint,Department=department)
       else
         RealStudent(Name="",Age=0,ePoints=0,Department="")
    result;;


let f = register(Steven)

printf "%s" f.Name
