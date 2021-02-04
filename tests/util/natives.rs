//! Some native functions for alchem testing.

use alchem::collapsed::{CollapsedInfo, CollapsedType};
use alchem::value::{add_native, add_std, Globals, MorphStatus, NativeInfo, NoCustom, Type, Value};
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

pub fn add_all_natives(globals: &mut Gl) {
  add_std(globals);
  add_native(globals, "show", 1, ntv_show, ntvt_show);
  add_native(globals, "fourty_two", 0, ntv_number, ntvt_number);
  add_native(globals, "recall", 1, ntv_recall, ntvt_recall);
  add_native(globals, "recall_1", 2, ntv_recall_1, ntvt_recall_1);
  add_native(globals, "reloop", 1, ntv_reloop, ntvt_reloop);
  add_native(globals, "to_json", 1, ntv_to_json, ntvt_to_json);
}

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_show(args: Vec<Tp>, _globals: &Gl) -> Status {
  assert!(args[0].is_string());
  MorphStatus::NativeCompleted(Info::new(), Type::String(None))
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_show(vals: Vec<Val>, _info: CoInfo, _runner: &mut Run) -> Val { vals.into_iter().next().unwrap() }

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_number(_args: Vec<Tp>, _globals: &Gl) -> Status {
  MorphStatus::NativeCompleted(Info::new(), Type::Number)
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_number(_argv: Vec<Val>, _info: CoInfo, _runner: &mut Run) -> Val { Value::Int(42) }

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_recall(args: Vec<Tp>, globals: &Gl) -> Status {
  assert_eq!(args.len(), 1);
  let func = args[0].as_function().upgrade().unwrap();
  assert_eq!(func.arity(), 0);
  let (inst_ind, ftype) = func.clone().find_or_build(Vec::new(), globals).await;

  if let Some(ftype) = ftype {
    let mut info = Info::new();
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::depends(&func, inst_ind))
  }
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_recall(vals: Vec<Val>, info: CoInfo, runner: &mut Run) -> Val {
  let mut vals = vals;
  let f = vals.remove(0);
  let inst_ind = info.call_indexes()[0];
  runner.run_value(f, inst_ind, Vec::new()).await
}

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_recall_1(args: Vec<Tp>, globals: &Gl) -> Status {
  let mut args = args.into_iter();
  let func = args.next().unwrap().as_function().upgrade().unwrap();
  assert_eq!(func.arity(), 1);
  let (inst_ind, ftype) = func.clone().find_or_build(args.collect(), globals).await;

  if let Some(ftype) = ftype {
    let mut info = Info::new();
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::depends(&func, inst_ind))
  }
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_recall_1(vals: Vec<Val>, info: CoInfo, runner: &mut Run) -> Val {
  let mut vals = vals.into_iter();
  let f = vals.next().unwrap();
  let a = vals.next().unwrap();
  let inst_ind = info.call_indexes()[0];
  runner.run_value(f, inst_ind, vec![a]).await
}

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_reloop(args: Vec<Tp>, globals: &Gl) -> Status {
  assert_eq!(args.len(), 1);
  let f = args[0].as_function().upgrade().unwrap();
  assert_eq!(f.arity(), 0);
  assert!(!f.is_single_use());
  let (inst_ind, ftype) = f.clone().find_or_build(Vec::new(), globals).await;

  if let Some(ftype) = ftype {
    let mut info = Info::new();
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::depends(&f, inst_ind))
  }
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_reloop(vals: Vec<Val>, info: CoInfo, runner: &mut Run) -> Val {
  let mut vals = vals;
  let mut f = vals.remove(0);
  let inst_ind = info.call_indexes()[0];
  let _ = runner.run_value(f.shift(), inst_ind, Vec::new()).await;
  runner.run_value(f, inst_ind, Vec::new()).await
}

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
