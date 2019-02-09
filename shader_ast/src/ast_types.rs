
pub enum Expr{
    Id(Id),
}

pub struct Id{
    pub name:String
}

pub struct FnArg{
    pub id:String,
    pub ty:String
}

pub struct Function{
    pub args:Vec<FnArg>
}