import Lake
open Lake DSL

package «kasiopea-2023-lean» {
  -- add package configuration options here
}

lean_lib «Kasiopea2023Lean» {
  -- add library configuration options here
}

@[default_target]
lean_exe «kasiopea-2023-lean» {
  root := `Main
}
