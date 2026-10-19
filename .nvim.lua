-- .nvim.lua — project-local Neovim config for Boltzmannomics.
--
-- Loaded automatically when you open Neovim from this directory, provided
-- your main config enables project-local configs:
--   vim.o.exrc = true
-- (add it to SECTION 1 of your init.lua).
--
-- Conda note: if Neovim is started with the project's conda environment
-- activated (e.g. `conda activate boltz`), fortls is taken from
-- $CONDA_PREFIX/bin/fortls so the server version stays pinned with the
-- project's toolchain. Otherwise it falls back to `fortls` on PATH
-- (pipx/Mason copy). To keep everything inside conda:
--   conda install -c conda-forge fortls fprettify

-- 1. Resolve the fortls binary: conda env first, PATH second.
local fortls_bin = 'fortls'
local conda_prefix = vim.env.CONDA_PREFIX
if conda_prefix and conda_prefix ~= '' then
  local conda_fortls = conda_prefix .. '/bin/fortls'
  if vim.fn.executable(conda_fortls) == 1 then
    fortls_bin = conda_fortls
  end
end
if fortls_bin == 'fortls' and vim.fn.executable('fortls') ~= 1 then
  vim.notify(
    'Boltzmannomics .nvim.lua: no `fortls` found (neither $CONDA_PREFIX/bin/fortls nor PATH).'
      .. ' Install it, e.g. `conda install -c conda-forge fortls`.',
    vim.log.levels.WARN
  )
  return
end

-- 2. fortls reads its settings from CLI flags / config file, NOT from LSP
-- `settings` (it ignores workspace/didChangeConfiguration), so everything
-- project-specific goes into `cmd`. The first flags repeat the global
-- fortls setup from init.lua since `cmd` replaces the default wholesale.
local cmd = {
  fortls_bin,
  '--notify_init',
  '--hover_signature',
  '--hover_language=fortran',
  '--use_signature_help',
  -- Only scan project sources (skips images/, terraform/, postprocess/, ...).
  '--source_dirs',
  'src',
  'app',
  'test',
  -- This project keeps preprocessor branches (#ifdef BOLTZ_USE_*) in
  -- lowercase .f90 files, which fortls otherwise parses without evaluating
  -- (it only preprocesses UPPERCASE suffixes by default), reading both
  -- branches at once. Treat .f90 as preprocessed so navigation/diagnostics
  -- follow the branch actually compiled by the default gfortran build
  -- (no defines, i.e. `do concurrent`, matching gfortran-environment-vars.sh).
  '--pp_suffixes',
  '.f90',
  '.F90',
  '.F',
  -- To mirror an MPI or coarray build instead, add one of:
  --   '--pp_defs={"BOLTZ_USE_MPI": ""}',
  --   '--pp_defs={"BOLTZ_USE_COARRAY": ""}',
}

vim.lsp.config('fortls', {
  cmd = cmd,
  filetypes = { 'fortran' },
  root_markers = { 'fpm.toml', '.fortls', '.fortlsrc', '.fortls.json', '.git' },
})
vim.lsp.enable('fortls')

-- 3. Match the project's canonical formatter settings (see format.sh):
-- fprettify with indent 4, line-length 300, whitespace 4.
local ok, conform = pcall(require, 'conform')
if ok then
  conform.formatters.fprettify = {
    prepend_args = {
      '--silent',
      '--indent',
      '4',
      '--line-length',
      '300',
      '--whitespace',
      '4',
    },
  }
end
