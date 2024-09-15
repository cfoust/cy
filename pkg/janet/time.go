package janet

import (
	"time"
)

type janetTimestamp struct {
	Hours        int
	Minutes      int
	Month        int
	MonthDay     int
	Seconds      int
	Milliseconds int
	WeekDay      int
	Year         int
	YearDay      int
	UTC          bool
	DST          bool
}

func janetFromTime(t time.Time) janetTimestamp {
	return janetTimestamp{
		DST:          false,
		Hours:        t.Hour(),
		Minutes:      t.Minute(),
		Month:        int(t.Month()),
		MonthDay:     t.Day(),
		Seconds:      t.Second(),
		Milliseconds: t.Nanosecond() / 1000000,
		WeekDay:      int(t.Weekday()),
		Year:         t.Year(),
		YearDay:      t.YearDay(),
		UTC:          t.Location() == time.UTC,
	}
}

func janetToTime(j janetTimestamp) time.Time {
	location := time.Local
	if j.UTC {
		location = time.UTC
	}

	return time.Date(
		j.Year,
		time.Month(j.Month),
		j.MonthDay,
		j.Hours,
		j.Minutes,
		j.Seconds,
		j.Milliseconds*1000000,
		location,
	)
}
