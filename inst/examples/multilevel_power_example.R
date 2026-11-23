# End-to-end offline specshiny example:
# Natural language -> validated IR -> refinement -> AST-verified R script -> bundle

library(specshiny)

prompt <- paste(
  "2-level power analysis for the cross-level interaction between x1 and z1 of 0.30",
  "with 50 schools, 20 students per school, cluster size sd 5, ICC 0.2,",
  "level-2 z1 effect 0.25, random slope for x1, t-distributed errors df 5,",
  "20 replications"
)

# 1. Translate natural language into a validated spec_ir
tr <- nl_to_spec(prompt)
print(tr$spec)

# 2. Track undo/redo history and apply a conversational refinement
hist <- spec_history(tr$spec)
ref <- refine_spec(tr$spec, "increase clusters to 60 and sweep ICC across 0.1, 0.2")
history_push(hist, ref$spec, label = "Increase clusters & add ICC sweep")
print(ref$diff)

# 3. Inspect synthesized minimal UI controls
desc <- spec_to_controls(ref$spec)
print(desc)

# 4. Deterministically compile and statically verify AST safety
code <- compile_r_script(ref$spec)
print(verify_compiled_r_ast(code))

# 5. Export and verify a reproducible study bundle in tempdir()
bundle_dir <- file.path(tempdir(), "specshiny_example_bundle")
export_spec_bundle(ref$spec, bundle_dir, overwrite = TRUE)
print(verify_spec_bundle(bundle_dir))
unlink(bundle_dir, recursive = TRUE)
