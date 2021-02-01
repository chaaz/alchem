//! Some JSON-based native functions for alchem testing.

use alchem::collapsed::CollapsedType;
use alchem::value::{add_native, CollapsedInfo, Globals, MorphStatus, NativeInfo, NoCustom, Type, Value};
use alchem::vm::Runner;
use alchem::{native_fn, native_tfn};
use macro_rules_attribute::macro_rules_attribute;
use serde_json::{Number, Value as Json};

type Val = Value<NoCustom>;
type Info = NativeInfo<NoCustom>;
type CoInfo = CollapsedInfo<NoCustom>;
type Run = Runner<NoCustom>;
type Tp = Type<NoCustom>;
type Gl = Globals<NoCustom>;
type Status = MorphStatus<NoCustom>;

pub fn add_all_natives(globals: &mut Gl) { add_native(globals, "to_json", 1, ntv_to_json, ntvt_to_json); }

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_to_json(args: Vec<Tp>, _globals: &Gl) -> Status {
  let mut info = Info::new();
  assert_eq!(args.len(), 1);
  info.add_type(CollapsedType::from_common(&args[0]));
  MorphStatus::NativeCompleted(info, Type::Json)
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_to_json(vals: Vec<Val>, info: CoInfo, _runner: &mut Run) -> Val {
  let val = vals.into_iter().next().unwrap();
  let col_type = info.into_types().into_iter().next().unwrap();
  Value::Json(convert_to_json(val, col_type))
}

fn convert_to_json(val: Val, t: CollapsedType<NoCustom>) -> Json {
  match val {
    Value::Float(v) => Json::Number(Number::from_f64(v).unwrap()),
    Value::Int(v) => Json::Number(Number::from_f64(v as f64).unwrap()),
    Value::Bool(v) => Json::Bool(v),
    Value::String(v) => Json::String(v.to_string()),
    Value::Array(v) => match t {
      CollapsedType::Object(o) => {
        Json::Object(v.into_iter().zip(o.into_key_types()).map(|(v, (k, t))| (k, convert_to_json(v, t))).collect())
      }
      CollapsedType::Array(a) => {
        Json::Array(v.into_iter().zip(a.into_types()).map(|(v, t)| convert_to_json(v, t)).collect())
      }
      other => panic!("Array value can't have type {:?}", other)
    },
    Value::Json(v) => v,
    other => panic!("Can't convert to JSON: {:?}", other)
  }
}
