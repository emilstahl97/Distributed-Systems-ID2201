-module(empty).
-compile(export_all).

call_new() ->
  intf = interfaces:new(),
  intf.

