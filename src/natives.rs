//! Some natives to add to the standard Alchem compile/runtime.

use crate::collapsed::{Captured, CollapsedType, RunMeta};
use crate::common::{FunctionIndex, TypeCaptured};
use crate::value::{add_native, match_native, CustomType, Function, Globals, MorphStatus, NativeInfo, Type, Value};
use crate::vm::{compile, Runner};
use alchem_macros::{native_fn, native_tfn};
use serde_json::{Number, Value as Json};
use std::sync::Arc;

pub fn add_std<C: CustomType + 'static>(globals: &mut Globals<C>) {
  add_native(globals, "show", 1, ntv_show, ntvt_show);
  match_native(globals, "eval", 1, ntv_eval, ntvt_eval, ntvm_eval);
  add_native(globals, "print", 1, ntv_print, ntvt_print);
  add_native(globals, "to_json", 1, ntv_to_json, ntvt_to_json);
}

#[native_tfn]
async fn ntvt_show<C: CustomType + 'static>(
  args: Vec<Type<C>>, _tc: TypeCaptured<C>, _globals: &Globals<C>
) -> MorphStatus<C> {
  assert!(args[0].is_string() || args[0].is_number() || args[0].is_bool() || args[0].is_json());
  MorphStatus::NativeCompleted(NativeInfo::new(), Type::String(None))
}

#[native_fn]
async fn ntv_show<C: CustomType + 'static>(
  vals: Vec<Value<C>>, _info: Captured<C>, _meta: RunMeta<C>, _runner: &mut Runner<C>
) -> Value<C> {
  let mut vals = vals.into_iter();
  let val = vals.next().unwrap();

  let val = match val {
    Value::Float(v) => v.to_string(),
    Value::Int(v) => v.to_string(),
    Value::Bool(v) => v.to_string(),
    Value::String(v) => v.to_string(),
    Value::Json(v) => v.to_string(),
    other => panic!("Cannot show: {:?}", other)
  };

  Value::String(val.into())
}

fn ntvm_eval<C: CustomType + 'static>(a1: &[Type<C>], a2: &[Type<C>]) -> bool {
  assert_eq!(a1.len(), 1);
  assert_eq!(a2.len(), 1);

  let c1 = a1[0].as_string().as_deref().expect("Eval arg must be a literal string.");
  let c2 = a2[0].as_string().as_deref().expect("Eval arg must be a literal string.");
  c1 == c2
}

#[native_tfn]
async fn ntvt_eval<C: CustomType + 'static>(
  args: Vec<Type<C>>, _tc: TypeCaptured<C>, globals: &Globals<C>
) -> MorphStatus<C> {
  let code = args[0].as_string().as_deref().expect("Eval argument must be a literal string.");
  let (scope, stype) = compile(code, &globals).await;
  let chunk = scope.into_chunk();
  let function = Function::script(chunk, stype.clone());

  let mut info = NativeInfo::new();
  info.add_function(Arc::new(function));
  MorphStatus::NativeCompleted(info, stype)
}

#[native_fn]
async fn ntv_eval<C: CustomType + 'static>(
  _vals: Vec<Value<C>>, info: Captured<C>, meta: RunMeta<C>, runner: &mut Runner<C>
) -> Value<C> {
  let f = info.into_functions().into_iter().next().unwrap();
  runner.run(f.into_value(), FunctionIndex::empty(0), meta, Vec::new()).await
}

#[native_tfn]
async fn ntvt_print<C: CustomType + 'static>(
  _args: Vec<Type<C>>, _tc: TypeCaptured<C>, _globals: &Globals<C>
) -> MorphStatus<C> {
  let info = NativeInfo::new();
  MorphStatus::NativeCompleted(info, Type::Number)
}

#[native_fn]
async fn ntv_print<C: CustomType + 'static>(
  vals: Vec<Value<C>>, _info: Captured<C>, _meta: RunMeta<C>, _runner: &mut Runner<C>
) -> Value<C> {
  println!("*** PRINT: {:?}", vals[0]);
  Value::Int(1)
}

#[native_tfn]
async fn ntvt_to_json<C: CustomType + 'static>(
  args: Vec<Type<C>>, _tc: TypeCaptured<C>, _globals: &Globals<C>
) -> MorphStatus<C> {
  let mut info = NativeInfo::new();
  assert_eq!(args.len(), 1);
  info.add_type(CollapsedType::from_common(&args[0]));
  MorphStatus::NativeCompleted(info, Type::Json)
}

#[native_fn]
async fn ntv_to_json<C: CustomType + 'static>(
  vals: Vec<Value<C>>, info: Captured<C>, _meta: RunMeta<C>, _runner: &mut Runner<C>
) -> Value<C> {
  let val = vals.into_iter().next().unwrap();
  let col_type = info.into_types().into_iter().next().unwrap();
  Value::Json(convert_to_json(val, col_type))
}

pub fn convert_to_json<C: CustomType + 'static>(val: Value<C>, t: CollapsedType<C>) -> Json {
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
