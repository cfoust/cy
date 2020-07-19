# the roadmap

## basic functionality
* [ ] Recording
  - [ ] Improve exception handling so that we don't throw an exception when the process exits
  - [ ] Proxy all signals down to the actual shell (SIGINT doesn't work rn)
  - [ ] Make sure that everything works as expected; it seems like things that use the alternate screen buffer don't work
  - [ ] Save `.borg` files to `$HOME/.cy` or `$CYDIR` in the format `YYYY.MM.DD.HH.MM.SS.MSMSMS.borg`
