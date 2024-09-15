# doc: Now

Return a [Time struct](/api.md#time) representing the current local time.

# doc: Format

(time/format ts format)

Format a [Time struct](/api.md#time) using the provided format string. This uses Go's [Time.Format](https://pkg.go.dev/time#Time.Format). `cy` provides all of Go's built-in time format layouts as [constants](https://github.com/cfoust/cy/blob/main/pkg/cy/boot/time.janet), such as `time/format/rfc-822`.

For example:

```janet
(time/format (time/now) time/format/rfc-822)
```
