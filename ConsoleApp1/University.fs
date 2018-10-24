module University

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

let Programming = new PTraits(name="Programming", scale = 10)
let Mathematics = new PTraits(name="Mathematics", scale = 7)
let Social = new PTraits(name="Social", scale = 8)
let Research = new PTraits(name="Research", scale = 9)
let Biology = new PTraits(name="Biology", scale = 4)

type NonGroupedStudent(name:string, age:int, EPoint:int, PTraits:PTraits) =
  member x.Name = name;
  member x.Age = age;
  member x.EPoint = EPoint;
  member x.PTraits = PTraits;

let Steven = new NonGroupedStudent(name="Steven", age=22, EPoint=105, PTraits=Programming)
let Iva = new NonGroupedStudent(name="Iva", age= 20, EPoint=100, PTraits=Mathematics)
let Kusky = new NonGroupedStudent(name="Kusky", age=21, EPoint=110, PTraits=Social)
let Willy = new NonGroupedStudent(name="Willy", age=24, EPoint=120, PTraits=Research)
let John = new NonGroupedStudent(name="John", age=22, EPoint=110, PTraits=Biology)

let grouping (student : NonGroupedStudent) =
  let result = 
    if student.PTraits.Name="Biology" && student.PTraits.scale>=9 then "Res"
    elif student.PTraits.Name="Research" && student.PTraits.scale>=8 then "Hdhasd"
    else ":dufibs"
  result;;


let printStudent = grouping(Iva)

type student(student:NonGroupedStudent) = 
  member x.Name = student.Name
  member x.Age = student.Age
  member x.EPoint = student.EPoint
  member x.PTraits = student.PTraits
  override x.GetHashCode() = hash(student)
  override x.Equals(a) = 
    match a with 
    | :? student as p -> (student.PTraits.Name, student.PTraits.scale) = (p.PTraits.Name, p.PTraits.scale)
    | _ -> false

student(Steven) = student(Iva)