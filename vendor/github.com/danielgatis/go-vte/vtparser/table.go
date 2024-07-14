package vtparser

// THIS FILE WAS AUTO-GENERATED. DO NOT MODIFY.

const (
	anywhereState           State = 0
	csiEntryState           State = 1
	csiIgnoreState          State = 2
	csiIntermediateState    State = 3
	csiParamState           State = 4
	dcsEntryState           State = 5
	dcsIgnoreState          State = 6
	dcsIntermediateState    State = 7
	dcsParamState           State = 8
	dcsPassthroughState     State = 9
	escapeState             State = 10
	escapeIntermediateState State = 11
	groundState             State = 12
	oscStringState          State = 13
	sosPmApcStringState     State = 14
	utf8State               State = 15

	noneAction        Action = 0
	clearAction       Action = 1
	collectAction     Action = 2
	csiDispatchAction Action = 3
	escDispatchAction Action = 4
	executeAction     Action = 5
	hookAction        Action = 6
	ignoreAction      Action = 7
	oscEndAction      Action = 8
	oscPutAction      Action = 9
	oscStartAction    Action = 10
	paramAction       Action = 11
	printAction       Action = 12
	putAction         Action = 13
	unhookAction      Action = 14
	beginUtf8Action   Action = 15
)

var (
	stateNames = []string{
		"anywhereState",
		"csiEntryState",
		"csiIgnoreState",
		"csiIntermediateState",
		"csiParamState",
		"dcsEntryState",
		"dcsIgnoreState",
		"dcsIntermediateState",
		"dcsParamState",
		"dcsPassthroughState",
		"escapeState",
		"escapeIntermediateState",
		"groundState",
		"oscStringState",
		"sosPmApcStringState",
		"utf8State",
	}

	stateTable = [][]byte{
		{ //State anywhereState = 0
			0,                                  // 00
			0,                                  // 01
			0,                                  // 02
			0,                                  // 03
			0,                                  // 04
			0,                                  // 05
			0,                                  // 06
			0,                                  // 07
			0,                                  // 08
			0,                                  // 09
			0,                                  // 0A
			0,                                  // 0B
			0,                                  // 0C
			0,                                  // 0D
			0,                                  // 0E
			0,                                  // 0F
			0,                                  // 10
			0,                                  // 11
			0,                                  // 12
			0,                                  // 13
			0,                                  // 14
			0,                                  // 15
			0,                                  // 16
			0,                                  // 17
			groundState | (executeAction << 4), // 18
			0,                                  // 19
			groundState | (executeAction << 4), // 1A
			escapeState | (noneAction << 4),    // 1B
			0,                                  // 1C
			0,                                  // 1D
			0,                                  // 1E
			0,                                  // 1F
			0,                                  // 20
			0,                                  // 21
			0,                                  // 22
			0,                                  // 23
			0,                                  // 24
			0,                                  // 25
			0,                                  // 26
			0,                                  // 27
			0,                                  // 28
			0,                                  // 29
			0,                                  // 2A
			0,                                  // 2B
			0,                                  // 2C
			0,                                  // 2D
			0,                                  // 2E
			0,                                  // 2F
			0,                                  // 30
			0,                                  // 31
			0,                                  // 32
			0,                                  // 33
			0,                                  // 34
			0,                                  // 35
			0,                                  // 36
			0,                                  // 37
			0,                                  // 38
			0,                                  // 39
			0,                                  // 3A
			0,                                  // 3B
			0,                                  // 3C
			0,                                  // 3D
			0,                                  // 3E
			0,                                  // 3F
			0,                                  // 40
			0,                                  // 41
			0,                                  // 42
			0,                                  // 43
			0,                                  // 44
			0,                                  // 45
			0,                                  // 46
			0,                                  // 47
			0,                                  // 48
			0,                                  // 49
			0,                                  // 4A
			0,                                  // 4B
			0,                                  // 4C
			0,                                  // 4D
			0,                                  // 4E
			0,                                  // 4F
			0,                                  // 50
			0,                                  // 51
			0,                                  // 52
			0,                                  // 53
			0,                                  // 54
			0,                                  // 55
			0,                                  // 56
			0,                                  // 57
			0,                                  // 58
			0,                                  // 59
			0,                                  // 5A
			0,                                  // 5B
			0,                                  // 5C
			0,                                  // 5D
			0,                                  // 5E
			0,                                  // 5F
			0,                                  // 60
			0,                                  // 61
			0,                                  // 62
			0,                                  // 63
			0,                                  // 64
			0,                                  // 65
			0,                                  // 66
			0,                                  // 67
			0,                                  // 68
			0,                                  // 69
			0,                                  // 6A
			0,                                  // 6B
			0,                                  // 6C
			0,                                  // 6D
			0,                                  // 6E
			0,                                  // 6F
			0,                                  // 70
			0,                                  // 71
			0,                                  // 72
			0,                                  // 73
			0,                                  // 74
			0,                                  // 75
			0,                                  // 76
			0,                                  // 77
			0,                                  // 78
			0,                                  // 79
			0,                                  // 7A
			0,                                  // 7B
			0,                                  // 7C
			0,                                  // 7D
			0,                                  // 7E
			0,                                  // 7F
			0,                                  // 80
			0,                                  // 81
			0,                                  // 82
			0,                                  // 83
			0,                                  // 84
			0,                                  // 85
			0,                                  // 86
			0,                                  // 87
			0,                                  // 88
			0,                                  // 89
			0,                                  // 8A
			0,                                  // 8B
			0,                                  // 8C
			0,                                  // 8D
			0,                                  // 8E
			0,                                  // 8F
			0,                                  // 90
			0,                                  // 91
			0,                                  // 92
			0,                                  // 93
			0,                                  // 94
			0,                                  // 95
			0,                                  // 96
			0,                                  // 97
			0,                                  // 98
			0,                                  // 99
			0,                                  // 9A
			0,                                  // 9B
			0,                                  // 9C
			0,                                  // 9D
			0,                                  // 9E
			0,                                  // 9F
			0,                                  // A0
			0,                                  // A1
			0,                                  // A2
			0,                                  // A3
			0,                                  // A4
			0,                                  // A5
			0,                                  // A6
			0,                                  // A7
			0,                                  // A8
			0,                                  // A9
			0,                                  // AA
			0,                                  // AB
			0,                                  // AC
			0,                                  // AD
			0,                                  // AE
			0,                                  // AF
			0,                                  // B0
			0,                                  // B1
			0,                                  // B2
			0,                                  // B3
			0,                                  // B4
			0,                                  // B5
			0,                                  // B6
			0,                                  // B7
			0,                                  // B8
			0,                                  // B9
			0,                                  // BA
			0,                                  // BB
			0,                                  // BC
			0,                                  // BD
			0,                                  // BE
			0,                                  // BF
			0,                                  // C0
			0,                                  // C1
			0,                                  // C2
			0,                                  // C3
			0,                                  // C4
			0,                                  // C5
			0,                                  // C6
			0,                                  // C7
			0,                                  // C8
			0,                                  // C9
			0,                                  // CA
			0,                                  // CB
			0,                                  // CC
			0,                                  // CD
			0,                                  // CE
			0,                                  // CF
			0,                                  // D0
			0,                                  // D1
			0,                                  // D2
			0,                                  // D3
			0,                                  // D4
			0,                                  // D5
			0,                                  // D6
			0,                                  // D7
			0,                                  // D8
			0,                                  // D9
			0,                                  // DA
			0,                                  // DB
			0,                                  // DC
			0,                                  // DD
			0,                                  // DE
			0,                                  // DF
			0,                                  // E0
			0,                                  // E1
			0,                                  // E2
			0,                                  // E3
			0,                                  // E4
			0,                                  // E5
			0,                                  // E6
			0,                                  // E7
			0,                                  // E8
			0,                                  // E9
			0,                                  // EA
			0,                                  // EB
			0,                                  // EC
			0,                                  // ED
			0,                                  // EE
			0,                                  // EF
			0,                                  // F0
			0,                                  // F1
			0,                                  // F2
			0,                                  // F3
			0,                                  // F4
			0,                                  // F5
			0,                                  // F6
			0,                                  // F7
			0,                                  // F8
			0,                                  // F9
			0,                                  // FA
			0,                                  // FB
			0,                                  // FC
			0,                                  // FD
			0,                                  // FE
			0,                                  // FF
		},
		{ //State csiEntryState = 1
			anywhereState | (executeAction << 4),        // 00
			anywhereState | (executeAction << 4),        // 01
			anywhereState | (executeAction << 4),        // 02
			anywhereState | (executeAction << 4),        // 03
			anywhereState | (executeAction << 4),        // 04
			anywhereState | (executeAction << 4),        // 05
			anywhereState | (executeAction << 4),        // 06
			anywhereState | (executeAction << 4),        // 07
			anywhereState | (executeAction << 4),        // 08
			anywhereState | (executeAction << 4),        // 09
			anywhereState | (executeAction << 4),        // 0A
			anywhereState | (executeAction << 4),        // 0B
			anywhereState | (executeAction << 4),        // 0C
			anywhereState | (executeAction << 4),        // 0D
			anywhereState | (executeAction << 4),        // 0E
			anywhereState | (executeAction << 4),        // 0F
			anywhereState | (executeAction << 4),        // 10
			anywhereState | (executeAction << 4),        // 11
			anywhereState | (executeAction << 4),        // 12
			anywhereState | (executeAction << 4),        // 13
			anywhereState | (executeAction << 4),        // 14
			anywhereState | (executeAction << 4),        // 15
			anywhereState | (executeAction << 4),        // 16
			anywhereState | (executeAction << 4),        // 17
			0,                                           // 18
			anywhereState | (executeAction << 4),        // 19
			0,                                           // 1A
			0,                                           // 1B
			anywhereState | (executeAction << 4),        // 1C
			anywhereState | (executeAction << 4),        // 1D
			anywhereState | (executeAction << 4),        // 1E
			anywhereState | (executeAction << 4),        // 1F
			csiIntermediateState | (collectAction << 4), // 20
			csiIntermediateState | (collectAction << 4), // 21
			csiIntermediateState | (collectAction << 4), // 22
			csiIntermediateState | (collectAction << 4), // 23
			csiIntermediateState | (collectAction << 4), // 24
			csiIntermediateState | (collectAction << 4), // 25
			csiIntermediateState | (collectAction << 4), // 26
			csiIntermediateState | (collectAction << 4), // 27
			csiIntermediateState | (collectAction << 4), // 28
			csiIntermediateState | (collectAction << 4), // 29
			csiIntermediateState | (collectAction << 4), // 2A
			csiIntermediateState | (collectAction << 4), // 2B
			csiIntermediateState | (collectAction << 4), // 2C
			csiIntermediateState | (collectAction << 4), // 2D
			csiIntermediateState | (collectAction << 4), // 2E
			csiIntermediateState | (collectAction << 4), // 2F
			csiParamState | (paramAction << 4),          // 30
			csiParamState | (paramAction << 4),          // 31
			csiParamState | (paramAction << 4),          // 32
			csiParamState | (paramAction << 4),          // 33
			csiParamState | (paramAction << 4),          // 34
			csiParamState | (paramAction << 4),          // 35
			csiParamState | (paramAction << 4),          // 36
			csiParamState | (paramAction << 4),          // 37
			csiParamState | (paramAction << 4),          // 38
			csiParamState | (paramAction << 4),          // 39
			csiIgnoreState | (noneAction << 4),          // 3A
			csiParamState | (paramAction << 4),          // 3B
			csiParamState | (collectAction << 4),        // 3C
			csiParamState | (collectAction << 4),        // 3D
			csiParamState | (collectAction << 4),        // 3E
			csiParamState | (collectAction << 4),        // 3F
			groundState | (csiDispatchAction << 4),      // 40
			groundState | (csiDispatchAction << 4),      // 41
			groundState | (csiDispatchAction << 4),      // 42
			groundState | (csiDispatchAction << 4),      // 43
			groundState | (csiDispatchAction << 4),      // 44
			groundState | (csiDispatchAction << 4),      // 45
			groundState | (csiDispatchAction << 4),      // 46
			groundState | (csiDispatchAction << 4),      // 47
			groundState | (csiDispatchAction << 4),      // 48
			groundState | (csiDispatchAction << 4),      // 49
			groundState | (csiDispatchAction << 4),      // 4A
			groundState | (csiDispatchAction << 4),      // 4B
			groundState | (csiDispatchAction << 4),      // 4C
			groundState | (csiDispatchAction << 4),      // 4D
			groundState | (csiDispatchAction << 4),      // 4E
			groundState | (csiDispatchAction << 4),      // 4F
			groundState | (csiDispatchAction << 4),      // 50
			groundState | (csiDispatchAction << 4),      // 51
			groundState | (csiDispatchAction << 4),      // 52
			groundState | (csiDispatchAction << 4),      // 53
			groundState | (csiDispatchAction << 4),      // 54
			groundState | (csiDispatchAction << 4),      // 55
			groundState | (csiDispatchAction << 4),      // 56
			groundState | (csiDispatchAction << 4),      // 57
			groundState | (csiDispatchAction << 4),      // 58
			groundState | (csiDispatchAction << 4),      // 59
			groundState | (csiDispatchAction << 4),      // 5A
			groundState | (csiDispatchAction << 4),      // 5B
			groundState | (csiDispatchAction << 4),      // 5C
			groundState | (csiDispatchAction << 4),      // 5D
			groundState | (csiDispatchAction << 4),      // 5E
			groundState | (csiDispatchAction << 4),      // 5F
			groundState | (csiDispatchAction << 4),      // 60
			groundState | (csiDispatchAction << 4),      // 61
			groundState | (csiDispatchAction << 4),      // 62
			groundState | (csiDispatchAction << 4),      // 63
			groundState | (csiDispatchAction << 4),      // 64
			groundState | (csiDispatchAction << 4),      // 65
			groundState | (csiDispatchAction << 4),      // 66
			groundState | (csiDispatchAction << 4),      // 67
			groundState | (csiDispatchAction << 4),      // 68
			groundState | (csiDispatchAction << 4),      // 69
			groundState | (csiDispatchAction << 4),      // 6A
			groundState | (csiDispatchAction << 4),      // 6B
			groundState | (csiDispatchAction << 4),      // 6C
			groundState | (csiDispatchAction << 4),      // 6D
			groundState | (csiDispatchAction << 4),      // 6E
			groundState | (csiDispatchAction << 4),      // 6F
			groundState | (csiDispatchAction << 4),      // 70
			groundState | (csiDispatchAction << 4),      // 71
			groundState | (csiDispatchAction << 4),      // 72
			groundState | (csiDispatchAction << 4),      // 73
			groundState | (csiDispatchAction << 4),      // 74
			groundState | (csiDispatchAction << 4),      // 75
			groundState | (csiDispatchAction << 4),      // 76
			groundState | (csiDispatchAction << 4),      // 77
			groundState | (csiDispatchAction << 4),      // 78
			groundState | (csiDispatchAction << 4),      // 79
			groundState | (csiDispatchAction << 4),      // 7A
			groundState | (csiDispatchAction << 4),      // 7B
			groundState | (csiDispatchAction << 4),      // 7C
			groundState | (csiDispatchAction << 4),      // 7D
			groundState | (csiDispatchAction << 4),      // 7E
			anywhereState | (ignoreAction << 4),         // 7F
			0,                                           // 80
			0,                                           // 81
			0,                                           // 82
			0,                                           // 83
			0,                                           // 84
			0,                                           // 85
			0,                                           // 86
			0,                                           // 87
			0,                                           // 88
			0,                                           // 89
			0,                                           // 8A
			0,                                           // 8B
			0,                                           // 8C
			0,                                           // 8D
			0,                                           // 8E
			0,                                           // 8F
			0,                                           // 90
			0,                                           // 91
			0,                                           // 92
			0,                                           // 93
			0,                                           // 94
			0,                                           // 95
			0,                                           // 96
			0,                                           // 97
			0,                                           // 98
			0,                                           // 99
			0,                                           // 9A
			0,                                           // 9B
			0,                                           // 9C
			0,                                           // 9D
			0,                                           // 9E
			0,                                           // 9F
			0,                                           // A0
			0,                                           // A1
			0,                                           // A2
			0,                                           // A3
			0,                                           // A4
			0,                                           // A5
			0,                                           // A6
			0,                                           // A7
			0,                                           // A8
			0,                                           // A9
			0,                                           // AA
			0,                                           // AB
			0,                                           // AC
			0,                                           // AD
			0,                                           // AE
			0,                                           // AF
			0,                                           // B0
			0,                                           // B1
			0,                                           // B2
			0,                                           // B3
			0,                                           // B4
			0,                                           // B5
			0,                                           // B6
			0,                                           // B7
			0,                                           // B8
			0,                                           // B9
			0,                                           // BA
			0,                                           // BB
			0,                                           // BC
			0,                                           // BD
			0,                                           // BE
			0,                                           // BF
			0,                                           // C0
			0,                                           // C1
			0,                                           // C2
			0,                                           // C3
			0,                                           // C4
			0,                                           // C5
			0,                                           // C6
			0,                                           // C7
			0,                                           // C8
			0,                                           // C9
			0,                                           // CA
			0,                                           // CB
			0,                                           // CC
			0,                                           // CD
			0,                                           // CE
			0,                                           // CF
			0,                                           // D0
			0,                                           // D1
			0,                                           // D2
			0,                                           // D3
			0,                                           // D4
			0,                                           // D5
			0,                                           // D6
			0,                                           // D7
			0,                                           // D8
			0,                                           // D9
			0,                                           // DA
			0,                                           // DB
			0,                                           // DC
			0,                                           // DD
			0,                                           // DE
			0,                                           // DF
			0,                                           // E0
			0,                                           // E1
			0,                                           // E2
			0,                                           // E3
			0,                                           // E4
			0,                                           // E5
			0,                                           // E6
			0,                                           // E7
			0,                                           // E8
			0,                                           // E9
			0,                                           // EA
			0,                                           // EB
			0,                                           // EC
			0,                                           // ED
			0,                                           // EE
			0,                                           // EF
			0,                                           // F0
			0,                                           // F1
			0,                                           // F2
			0,                                           // F3
			0,                                           // F4
			0,                                           // F5
			0,                                           // F6
			0,                                           // F7
			0,                                           // F8
			0,                                           // F9
			0,                                           // FA
			0,                                           // FB
			0,                                           // FC
			0,                                           // FD
			0,                                           // FE
			0,                                           // FF
		},
		{ //State csiIgnoreState = 2
			anywhereState | (executeAction << 4), // 00
			anywhereState | (executeAction << 4), // 01
			anywhereState | (executeAction << 4), // 02
			anywhereState | (executeAction << 4), // 03
			anywhereState | (executeAction << 4), // 04
			anywhereState | (executeAction << 4), // 05
			anywhereState | (executeAction << 4), // 06
			anywhereState | (executeAction << 4), // 07
			anywhereState | (executeAction << 4), // 08
			anywhereState | (executeAction << 4), // 09
			anywhereState | (executeAction << 4), // 0A
			anywhereState | (executeAction << 4), // 0B
			anywhereState | (executeAction << 4), // 0C
			anywhereState | (executeAction << 4), // 0D
			anywhereState | (executeAction << 4), // 0E
			anywhereState | (executeAction << 4), // 0F
			anywhereState | (executeAction << 4), // 10
			anywhereState | (executeAction << 4), // 11
			anywhereState | (executeAction << 4), // 12
			anywhereState | (executeAction << 4), // 13
			anywhereState | (executeAction << 4), // 14
			anywhereState | (executeAction << 4), // 15
			anywhereState | (executeAction << 4), // 16
			anywhereState | (executeAction << 4), // 17
			0,                                    // 18
			anywhereState | (executeAction << 4), // 19
			0,                                    // 1A
			0,                                    // 1B
			anywhereState | (executeAction << 4), // 1C
			anywhereState | (executeAction << 4), // 1D
			anywhereState | (executeAction << 4), // 1E
			anywhereState | (executeAction << 4), // 1F
			anywhereState | (ignoreAction << 4),  // 20
			anywhereState | (ignoreAction << 4),  // 21
			anywhereState | (ignoreAction << 4),  // 22
			anywhereState | (ignoreAction << 4),  // 23
			anywhereState | (ignoreAction << 4),  // 24
			anywhereState | (ignoreAction << 4),  // 25
			anywhereState | (ignoreAction << 4),  // 26
			anywhereState | (ignoreAction << 4),  // 27
			anywhereState | (ignoreAction << 4),  // 28
			anywhereState | (ignoreAction << 4),  // 29
			anywhereState | (ignoreAction << 4),  // 2A
			anywhereState | (ignoreAction << 4),  // 2B
			anywhereState | (ignoreAction << 4),  // 2C
			anywhereState | (ignoreAction << 4),  // 2D
			anywhereState | (ignoreAction << 4),  // 2E
			anywhereState | (ignoreAction << 4),  // 2F
			anywhereState | (ignoreAction << 4),  // 30
			anywhereState | (ignoreAction << 4),  // 31
			anywhereState | (ignoreAction << 4),  // 32
			anywhereState | (ignoreAction << 4),  // 33
			anywhereState | (ignoreAction << 4),  // 34
			anywhereState | (ignoreAction << 4),  // 35
			anywhereState | (ignoreAction << 4),  // 36
			anywhereState | (ignoreAction << 4),  // 37
			anywhereState | (ignoreAction << 4),  // 38
			anywhereState | (ignoreAction << 4),  // 39
			anywhereState | (ignoreAction << 4),  // 3A
			anywhereState | (ignoreAction << 4),  // 3B
			anywhereState | (ignoreAction << 4),  // 3C
			anywhereState | (ignoreAction << 4),  // 3D
			anywhereState | (ignoreAction << 4),  // 3E
			anywhereState | (ignoreAction << 4),  // 3F
			groundState | (noneAction << 4),      // 40
			groundState | (noneAction << 4),      // 41
			groundState | (noneAction << 4),      // 42
			groundState | (noneAction << 4),      // 43
			groundState | (noneAction << 4),      // 44
			groundState | (noneAction << 4),      // 45
			groundState | (noneAction << 4),      // 46
			groundState | (noneAction << 4),      // 47
			groundState | (noneAction << 4),      // 48
			groundState | (noneAction << 4),      // 49
			groundState | (noneAction << 4),      // 4A
			groundState | (noneAction << 4),      // 4B
			groundState | (noneAction << 4),      // 4C
			groundState | (noneAction << 4),      // 4D
			groundState | (noneAction << 4),      // 4E
			groundState | (noneAction << 4),      // 4F
			groundState | (noneAction << 4),      // 50
			groundState | (noneAction << 4),      // 51
			groundState | (noneAction << 4),      // 52
			groundState | (noneAction << 4),      // 53
			groundState | (noneAction << 4),      // 54
			groundState | (noneAction << 4),      // 55
			groundState | (noneAction << 4),      // 56
			groundState | (noneAction << 4),      // 57
			groundState | (noneAction << 4),      // 58
			groundState | (noneAction << 4),      // 59
			groundState | (noneAction << 4),      // 5A
			groundState | (noneAction << 4),      // 5B
			groundState | (noneAction << 4),      // 5C
			groundState | (noneAction << 4),      // 5D
			groundState | (noneAction << 4),      // 5E
			groundState | (noneAction << 4),      // 5F
			groundState | (noneAction << 4),      // 60
			groundState | (noneAction << 4),      // 61
			groundState | (noneAction << 4),      // 62
			groundState | (noneAction << 4),      // 63
			groundState | (noneAction << 4),      // 64
			groundState | (noneAction << 4),      // 65
			groundState | (noneAction << 4),      // 66
			groundState | (noneAction << 4),      // 67
			groundState | (noneAction << 4),      // 68
			groundState | (noneAction << 4),      // 69
			groundState | (noneAction << 4),      // 6A
			groundState | (noneAction << 4),      // 6B
			groundState | (noneAction << 4),      // 6C
			groundState | (noneAction << 4),      // 6D
			groundState | (noneAction << 4),      // 6E
			groundState | (noneAction << 4),      // 6F
			groundState | (noneAction << 4),      // 70
			groundState | (noneAction << 4),      // 71
			groundState | (noneAction << 4),      // 72
			groundState | (noneAction << 4),      // 73
			groundState | (noneAction << 4),      // 74
			groundState | (noneAction << 4),      // 75
			groundState | (noneAction << 4),      // 76
			groundState | (noneAction << 4),      // 77
			groundState | (noneAction << 4),      // 78
			groundState | (noneAction << 4),      // 79
			groundState | (noneAction << 4),      // 7A
			groundState | (noneAction << 4),      // 7B
			groundState | (noneAction << 4),      // 7C
			groundState | (noneAction << 4),      // 7D
			groundState | (noneAction << 4),      // 7E
			anywhereState | (ignoreAction << 4),  // 7F
			0,                                    // 80
			0,                                    // 81
			0,                                    // 82
			0,                                    // 83
			0,                                    // 84
			0,                                    // 85
			0,                                    // 86
			0,                                    // 87
			0,                                    // 88
			0,                                    // 89
			0,                                    // 8A
			0,                                    // 8B
			0,                                    // 8C
			0,                                    // 8D
			0,                                    // 8E
			0,                                    // 8F
			0,                                    // 90
			0,                                    // 91
			0,                                    // 92
			0,                                    // 93
			0,                                    // 94
			0,                                    // 95
			0,                                    // 96
			0,                                    // 97
			0,                                    // 98
			0,                                    // 99
			0,                                    // 9A
			0,                                    // 9B
			0,                                    // 9C
			0,                                    // 9D
			0,                                    // 9E
			0,                                    // 9F
			0,                                    // A0
			0,                                    // A1
			0,                                    // A2
			0,                                    // A3
			0,                                    // A4
			0,                                    // A5
			0,                                    // A6
			0,                                    // A7
			0,                                    // A8
			0,                                    // A9
			0,                                    // AA
			0,                                    // AB
			0,                                    // AC
			0,                                    // AD
			0,                                    // AE
			0,                                    // AF
			0,                                    // B0
			0,                                    // B1
			0,                                    // B2
			0,                                    // B3
			0,                                    // B4
			0,                                    // B5
			0,                                    // B6
			0,                                    // B7
			0,                                    // B8
			0,                                    // B9
			0,                                    // BA
			0,                                    // BB
			0,                                    // BC
			0,                                    // BD
			0,                                    // BE
			0,                                    // BF
			0,                                    // C0
			0,                                    // C1
			0,                                    // C2
			0,                                    // C3
			0,                                    // C4
			0,                                    // C5
			0,                                    // C6
			0,                                    // C7
			0,                                    // C8
			0,                                    // C9
			0,                                    // CA
			0,                                    // CB
			0,                                    // CC
			0,                                    // CD
			0,                                    // CE
			0,                                    // CF
			0,                                    // D0
			0,                                    // D1
			0,                                    // D2
			0,                                    // D3
			0,                                    // D4
			0,                                    // D5
			0,                                    // D6
			0,                                    // D7
			0,                                    // D8
			0,                                    // D9
			0,                                    // DA
			0,                                    // DB
			0,                                    // DC
			0,                                    // DD
			0,                                    // DE
			0,                                    // DF
			0,                                    // E0
			0,                                    // E1
			0,                                    // E2
			0,                                    // E3
			0,                                    // E4
			0,                                    // E5
			0,                                    // E6
			0,                                    // E7
			0,                                    // E8
			0,                                    // E9
			0,                                    // EA
			0,                                    // EB
			0,                                    // EC
			0,                                    // ED
			0,                                    // EE
			0,                                    // EF
			0,                                    // F0
			0,                                    // F1
			0,                                    // F2
			0,                                    // F3
			0,                                    // F4
			0,                                    // F5
			0,                                    // F6
			0,                                    // F7
			0,                                    // F8
			0,                                    // F9
			0,                                    // FA
			0,                                    // FB
			0,                                    // FC
			0,                                    // FD
			0,                                    // FE
			0,                                    // FF
		},
		{ //State csiIntermediateState = 3
			anywhereState | (executeAction << 4),   // 00
			anywhereState | (executeAction << 4),   // 01
			anywhereState | (executeAction << 4),   // 02
			anywhereState | (executeAction << 4),   // 03
			anywhereState | (executeAction << 4),   // 04
			anywhereState | (executeAction << 4),   // 05
			anywhereState | (executeAction << 4),   // 06
			anywhereState | (executeAction << 4),   // 07
			anywhereState | (executeAction << 4),   // 08
			anywhereState | (executeAction << 4),   // 09
			anywhereState | (executeAction << 4),   // 0A
			anywhereState | (executeAction << 4),   // 0B
			anywhereState | (executeAction << 4),   // 0C
			anywhereState | (executeAction << 4),   // 0D
			anywhereState | (executeAction << 4),   // 0E
			anywhereState | (executeAction << 4),   // 0F
			anywhereState | (executeAction << 4),   // 10
			anywhereState | (executeAction << 4),   // 11
			anywhereState | (executeAction << 4),   // 12
			anywhereState | (executeAction << 4),   // 13
			anywhereState | (executeAction << 4),   // 14
			anywhereState | (executeAction << 4),   // 15
			anywhereState | (executeAction << 4),   // 16
			anywhereState | (executeAction << 4),   // 17
			0,                                      // 18
			anywhereState | (executeAction << 4),   // 19
			0,                                      // 1A
			0,                                      // 1B
			anywhereState | (executeAction << 4),   // 1C
			anywhereState | (executeAction << 4),   // 1D
			anywhereState | (executeAction << 4),   // 1E
			anywhereState | (executeAction << 4),   // 1F
			anywhereState | (collectAction << 4),   // 20
			anywhereState | (collectAction << 4),   // 21
			anywhereState | (collectAction << 4),   // 22
			anywhereState | (collectAction << 4),   // 23
			anywhereState | (collectAction << 4),   // 24
			anywhereState | (collectAction << 4),   // 25
			anywhereState | (collectAction << 4),   // 26
			anywhereState | (collectAction << 4),   // 27
			anywhereState | (collectAction << 4),   // 28
			anywhereState | (collectAction << 4),   // 29
			anywhereState | (collectAction << 4),   // 2A
			anywhereState | (collectAction << 4),   // 2B
			anywhereState | (collectAction << 4),   // 2C
			anywhereState | (collectAction << 4),   // 2D
			anywhereState | (collectAction << 4),   // 2E
			anywhereState | (collectAction << 4),   // 2F
			csiIgnoreState | (noneAction << 4),     // 30
			csiIgnoreState | (noneAction << 4),     // 31
			csiIgnoreState | (noneAction << 4),     // 32
			csiIgnoreState | (noneAction << 4),     // 33
			csiIgnoreState | (noneAction << 4),     // 34
			csiIgnoreState | (noneAction << 4),     // 35
			csiIgnoreState | (noneAction << 4),     // 36
			csiIgnoreState | (noneAction << 4),     // 37
			csiIgnoreState | (noneAction << 4),     // 38
			csiIgnoreState | (noneAction << 4),     // 39
			csiIgnoreState | (noneAction << 4),     // 3A
			csiIgnoreState | (noneAction << 4),     // 3B
			csiIgnoreState | (noneAction << 4),     // 3C
			csiIgnoreState | (noneAction << 4),     // 3D
			csiIgnoreState | (noneAction << 4),     // 3E
			csiIgnoreState | (noneAction << 4),     // 3F
			groundState | (csiDispatchAction << 4), // 40
			groundState | (csiDispatchAction << 4), // 41
			groundState | (csiDispatchAction << 4), // 42
			groundState | (csiDispatchAction << 4), // 43
			groundState | (csiDispatchAction << 4), // 44
			groundState | (csiDispatchAction << 4), // 45
			groundState | (csiDispatchAction << 4), // 46
			groundState | (csiDispatchAction << 4), // 47
			groundState | (csiDispatchAction << 4), // 48
			groundState | (csiDispatchAction << 4), // 49
			groundState | (csiDispatchAction << 4), // 4A
			groundState | (csiDispatchAction << 4), // 4B
			groundState | (csiDispatchAction << 4), // 4C
			groundState | (csiDispatchAction << 4), // 4D
			groundState | (csiDispatchAction << 4), // 4E
			groundState | (csiDispatchAction << 4), // 4F
			groundState | (csiDispatchAction << 4), // 50
			groundState | (csiDispatchAction << 4), // 51
			groundState | (csiDispatchAction << 4), // 52
			groundState | (csiDispatchAction << 4), // 53
			groundState | (csiDispatchAction << 4), // 54
			groundState | (csiDispatchAction << 4), // 55
			groundState | (csiDispatchAction << 4), // 56
			groundState | (csiDispatchAction << 4), // 57
			groundState | (csiDispatchAction << 4), // 58
			groundState | (csiDispatchAction << 4), // 59
			groundState | (csiDispatchAction << 4), // 5A
			groundState | (csiDispatchAction << 4), // 5B
			groundState | (csiDispatchAction << 4), // 5C
			groundState | (csiDispatchAction << 4), // 5D
			groundState | (csiDispatchAction << 4), // 5E
			groundState | (csiDispatchAction << 4), // 5F
			groundState | (csiDispatchAction << 4), // 60
			groundState | (csiDispatchAction << 4), // 61
			groundState | (csiDispatchAction << 4), // 62
			groundState | (csiDispatchAction << 4), // 63
			groundState | (csiDispatchAction << 4), // 64
			groundState | (csiDispatchAction << 4), // 65
			groundState | (csiDispatchAction << 4), // 66
			groundState | (csiDispatchAction << 4), // 67
			groundState | (csiDispatchAction << 4), // 68
			groundState | (csiDispatchAction << 4), // 69
			groundState | (csiDispatchAction << 4), // 6A
			groundState | (csiDispatchAction << 4), // 6B
			groundState | (csiDispatchAction << 4), // 6C
			groundState | (csiDispatchAction << 4), // 6D
			groundState | (csiDispatchAction << 4), // 6E
			groundState | (csiDispatchAction << 4), // 6F
			groundState | (csiDispatchAction << 4), // 70
			groundState | (csiDispatchAction << 4), // 71
			groundState | (csiDispatchAction << 4), // 72
			groundState | (csiDispatchAction << 4), // 73
			groundState | (csiDispatchAction << 4), // 74
			groundState | (csiDispatchAction << 4), // 75
			groundState | (csiDispatchAction << 4), // 76
			groundState | (csiDispatchAction << 4), // 77
			groundState | (csiDispatchAction << 4), // 78
			groundState | (csiDispatchAction << 4), // 79
			groundState | (csiDispatchAction << 4), // 7A
			groundState | (csiDispatchAction << 4), // 7B
			groundState | (csiDispatchAction << 4), // 7C
			groundState | (csiDispatchAction << 4), // 7D
			groundState | (csiDispatchAction << 4), // 7E
			anywhereState | (ignoreAction << 4),    // 7F
			0,                                      // 80
			0,                                      // 81
			0,                                      // 82
			0,                                      // 83
			0,                                      // 84
			0,                                      // 85
			0,                                      // 86
			0,                                      // 87
			0,                                      // 88
			0,                                      // 89
			0,                                      // 8A
			0,                                      // 8B
			0,                                      // 8C
			0,                                      // 8D
			0,                                      // 8E
			0,                                      // 8F
			0,                                      // 90
			0,                                      // 91
			0,                                      // 92
			0,                                      // 93
			0,                                      // 94
			0,                                      // 95
			0,                                      // 96
			0,                                      // 97
			0,                                      // 98
			0,                                      // 99
			0,                                      // 9A
			0,                                      // 9B
			0,                                      // 9C
			0,                                      // 9D
			0,                                      // 9E
			0,                                      // 9F
			0,                                      // A0
			0,                                      // A1
			0,                                      // A2
			0,                                      // A3
			0,                                      // A4
			0,                                      // A5
			0,                                      // A6
			0,                                      // A7
			0,                                      // A8
			0,                                      // A9
			0,                                      // AA
			0,                                      // AB
			0,                                      // AC
			0,                                      // AD
			0,                                      // AE
			0,                                      // AF
			0,                                      // B0
			0,                                      // B1
			0,                                      // B2
			0,                                      // B3
			0,                                      // B4
			0,                                      // B5
			0,                                      // B6
			0,                                      // B7
			0,                                      // B8
			0,                                      // B9
			0,                                      // BA
			0,                                      // BB
			0,                                      // BC
			0,                                      // BD
			0,                                      // BE
			0,                                      // BF
			0,                                      // C0
			0,                                      // C1
			0,                                      // C2
			0,                                      // C3
			0,                                      // C4
			0,                                      // C5
			0,                                      // C6
			0,                                      // C7
			0,                                      // C8
			0,                                      // C9
			0,                                      // CA
			0,                                      // CB
			0,                                      // CC
			0,                                      // CD
			0,                                      // CE
			0,                                      // CF
			0,                                      // D0
			0,                                      // D1
			0,                                      // D2
			0,                                      // D3
			0,                                      // D4
			0,                                      // D5
			0,                                      // D6
			0,                                      // D7
			0,                                      // D8
			0,                                      // D9
			0,                                      // DA
			0,                                      // DB
			0,                                      // DC
			0,                                      // DD
			0,                                      // DE
			0,                                      // DF
			0,                                      // E0
			0,                                      // E1
			0,                                      // E2
			0,                                      // E3
			0,                                      // E4
			0,                                      // E5
			0,                                      // E6
			0,                                      // E7
			0,                                      // E8
			0,                                      // E9
			0,                                      // EA
			0,                                      // EB
			0,                                      // EC
			0,                                      // ED
			0,                                      // EE
			0,                                      // EF
			0,                                      // F0
			0,                                      // F1
			0,                                      // F2
			0,                                      // F3
			0,                                      // F4
			0,                                      // F5
			0,                                      // F6
			0,                                      // F7
			0,                                      // F8
			0,                                      // F9
			0,                                      // FA
			0,                                      // FB
			0,                                      // FC
			0,                                      // FD
			0,                                      // FE
			0,                                      // FF
		},
		{ //State csiParamState = 4
			anywhereState | (executeAction << 4),        // 00
			anywhereState | (executeAction << 4),        // 01
			anywhereState | (executeAction << 4),        // 02
			anywhereState | (executeAction << 4),        // 03
			anywhereState | (executeAction << 4),        // 04
			anywhereState | (executeAction << 4),        // 05
			anywhereState | (executeAction << 4),        // 06
			anywhereState | (executeAction << 4),        // 07
			anywhereState | (executeAction << 4),        // 08
			anywhereState | (executeAction << 4),        // 09
			anywhereState | (executeAction << 4),        // 0A
			anywhereState | (executeAction << 4),        // 0B
			anywhereState | (executeAction << 4),        // 0C
			anywhereState | (executeAction << 4),        // 0D
			anywhereState | (executeAction << 4),        // 0E
			anywhereState | (executeAction << 4),        // 0F
			anywhereState | (executeAction << 4),        // 10
			anywhereState | (executeAction << 4),        // 11
			anywhereState | (executeAction << 4),        // 12
			anywhereState | (executeAction << 4),        // 13
			anywhereState | (executeAction << 4),        // 14
			anywhereState | (executeAction << 4),        // 15
			anywhereState | (executeAction << 4),        // 16
			anywhereState | (executeAction << 4),        // 17
			0,                                           // 18
			anywhereState | (executeAction << 4),        // 19
			0,                                           // 1A
			0,                                           // 1B
			anywhereState | (executeAction << 4),        // 1C
			anywhereState | (executeAction << 4),        // 1D
			anywhereState | (executeAction << 4),        // 1E
			anywhereState | (executeAction << 4),        // 1F
			csiIntermediateState | (collectAction << 4), // 20
			csiIntermediateState | (collectAction << 4), // 21
			csiIntermediateState | (collectAction << 4), // 22
			csiIntermediateState | (collectAction << 4), // 23
			csiIntermediateState | (collectAction << 4), // 24
			csiIntermediateState | (collectAction << 4), // 25
			csiIntermediateState | (collectAction << 4), // 26
			csiIntermediateState | (collectAction << 4), // 27
			csiIntermediateState | (collectAction << 4), // 28
			csiIntermediateState | (collectAction << 4), // 29
			csiIntermediateState | (collectAction << 4), // 2A
			csiIntermediateState | (collectAction << 4), // 2B
			csiIntermediateState | (collectAction << 4), // 2C
			csiIntermediateState | (collectAction << 4), // 2D
			csiIntermediateState | (collectAction << 4), // 2E
			csiIntermediateState | (collectAction << 4), // 2F
			anywhereState | (paramAction << 4),          // 30
			anywhereState | (paramAction << 4),          // 31
			anywhereState | (paramAction << 4),          // 32
			anywhereState | (paramAction << 4),          // 33
			anywhereState | (paramAction << 4),          // 34
			anywhereState | (paramAction << 4),          // 35
			anywhereState | (paramAction << 4),          // 36
			anywhereState | (paramAction << 4),          // 37
			anywhereState | (paramAction << 4),          // 38
			anywhereState | (paramAction << 4),          // 39
			csiIgnoreState | (noneAction << 4),          // 3A
			anywhereState | (paramAction << 4),          // 3B
			csiIgnoreState | (noneAction << 4),          // 3C
			csiIgnoreState | (noneAction << 4),          // 3D
			csiIgnoreState | (noneAction << 4),          // 3E
			csiIgnoreState | (noneAction << 4),          // 3F
			groundState | (csiDispatchAction << 4),      // 40
			groundState | (csiDispatchAction << 4),      // 41
			groundState | (csiDispatchAction << 4),      // 42
			groundState | (csiDispatchAction << 4),      // 43
			groundState | (csiDispatchAction << 4),      // 44
			groundState | (csiDispatchAction << 4),      // 45
			groundState | (csiDispatchAction << 4),      // 46
			groundState | (csiDispatchAction << 4),      // 47
			groundState | (csiDispatchAction << 4),      // 48
			groundState | (csiDispatchAction << 4),      // 49
			groundState | (csiDispatchAction << 4),      // 4A
			groundState | (csiDispatchAction << 4),      // 4B
			groundState | (csiDispatchAction << 4),      // 4C
			groundState | (csiDispatchAction << 4),      // 4D
			groundState | (csiDispatchAction << 4),      // 4E
			groundState | (csiDispatchAction << 4),      // 4F
			groundState | (csiDispatchAction << 4),      // 50
			groundState | (csiDispatchAction << 4),      // 51
			groundState | (csiDispatchAction << 4),      // 52
			groundState | (csiDispatchAction << 4),      // 53
			groundState | (csiDispatchAction << 4),      // 54
			groundState | (csiDispatchAction << 4),      // 55
			groundState | (csiDispatchAction << 4),      // 56
			groundState | (csiDispatchAction << 4),      // 57
			groundState | (csiDispatchAction << 4),      // 58
			groundState | (csiDispatchAction << 4),      // 59
			groundState | (csiDispatchAction << 4),      // 5A
			groundState | (csiDispatchAction << 4),      // 5B
			groundState | (csiDispatchAction << 4),      // 5C
			groundState | (csiDispatchAction << 4),      // 5D
			groundState | (csiDispatchAction << 4),      // 5E
			groundState | (csiDispatchAction << 4),      // 5F
			groundState | (csiDispatchAction << 4),      // 60
			groundState | (csiDispatchAction << 4),      // 61
			groundState | (csiDispatchAction << 4),      // 62
			groundState | (csiDispatchAction << 4),      // 63
			groundState | (csiDispatchAction << 4),      // 64
			groundState | (csiDispatchAction << 4),      // 65
			groundState | (csiDispatchAction << 4),      // 66
			groundState | (csiDispatchAction << 4),      // 67
			groundState | (csiDispatchAction << 4),      // 68
			groundState | (csiDispatchAction << 4),      // 69
			groundState | (csiDispatchAction << 4),      // 6A
			groundState | (csiDispatchAction << 4),      // 6B
			groundState | (csiDispatchAction << 4),      // 6C
			groundState | (csiDispatchAction << 4),      // 6D
			groundState | (csiDispatchAction << 4),      // 6E
			groundState | (csiDispatchAction << 4),      // 6F
			groundState | (csiDispatchAction << 4),      // 70
			groundState | (csiDispatchAction << 4),      // 71
			groundState | (csiDispatchAction << 4),      // 72
			groundState | (csiDispatchAction << 4),      // 73
			groundState | (csiDispatchAction << 4),      // 74
			groundState | (csiDispatchAction << 4),      // 75
			groundState | (csiDispatchAction << 4),      // 76
			groundState | (csiDispatchAction << 4),      // 77
			groundState | (csiDispatchAction << 4),      // 78
			groundState | (csiDispatchAction << 4),      // 79
			groundState | (csiDispatchAction << 4),      // 7A
			groundState | (csiDispatchAction << 4),      // 7B
			groundState | (csiDispatchAction << 4),      // 7C
			groundState | (csiDispatchAction << 4),      // 7D
			groundState | (csiDispatchAction << 4),      // 7E
			anywhereState | (ignoreAction << 4),         // 7F
			0,                                           // 80
			0,                                           // 81
			0,                                           // 82
			0,                                           // 83
			0,                                           // 84
			0,                                           // 85
			0,                                           // 86
			0,                                           // 87
			0,                                           // 88
			0,                                           // 89
			0,                                           // 8A
			0,                                           // 8B
			0,                                           // 8C
			0,                                           // 8D
			0,                                           // 8E
			0,                                           // 8F
			0,                                           // 90
			0,                                           // 91
			0,                                           // 92
			0,                                           // 93
			0,                                           // 94
			0,                                           // 95
			0,                                           // 96
			0,                                           // 97
			0,                                           // 98
			0,                                           // 99
			0,                                           // 9A
			0,                                           // 9B
			0,                                           // 9C
			0,                                           // 9D
			0,                                           // 9E
			0,                                           // 9F
			0,                                           // A0
			0,                                           // A1
			0,                                           // A2
			0,                                           // A3
			0,                                           // A4
			0,                                           // A5
			0,                                           // A6
			0,                                           // A7
			0,                                           // A8
			0,                                           // A9
			0,                                           // AA
			0,                                           // AB
			0,                                           // AC
			0,                                           // AD
			0,                                           // AE
			0,                                           // AF
			0,                                           // B0
			0,                                           // B1
			0,                                           // B2
			0,                                           // B3
			0,                                           // B4
			0,                                           // B5
			0,                                           // B6
			0,                                           // B7
			0,                                           // B8
			0,                                           // B9
			0,                                           // BA
			0,                                           // BB
			0,                                           // BC
			0,                                           // BD
			0,                                           // BE
			0,                                           // BF
			0,                                           // C0
			0,                                           // C1
			0,                                           // C2
			0,                                           // C3
			0,                                           // C4
			0,                                           // C5
			0,                                           // C6
			0,                                           // C7
			0,                                           // C8
			0,                                           // C9
			0,                                           // CA
			0,                                           // CB
			0,                                           // CC
			0,                                           // CD
			0,                                           // CE
			0,                                           // CF
			0,                                           // D0
			0,                                           // D1
			0,                                           // D2
			0,                                           // D3
			0,                                           // D4
			0,                                           // D5
			0,                                           // D6
			0,                                           // D7
			0,                                           // D8
			0,                                           // D9
			0,                                           // DA
			0,                                           // DB
			0,                                           // DC
			0,                                           // DD
			0,                                           // DE
			0,                                           // DF
			0,                                           // E0
			0,                                           // E1
			0,                                           // E2
			0,                                           // E3
			0,                                           // E4
			0,                                           // E5
			0,                                           // E6
			0,                                           // E7
			0,                                           // E8
			0,                                           // E9
			0,                                           // EA
			0,                                           // EB
			0,                                           // EC
			0,                                           // ED
			0,                                           // EE
			0,                                           // EF
			0,                                           // F0
			0,                                           // F1
			0,                                           // F2
			0,                                           // F3
			0,                                           // F4
			0,                                           // F5
			0,                                           // F6
			0,                                           // F7
			0,                                           // F8
			0,                                           // F9
			0,                                           // FA
			0,                                           // FB
			0,                                           // FC
			0,                                           // FD
			0,                                           // FE
			0,                                           // FF
		},
		{ //State dcsEntryState = 5
			anywhereState | (ignoreAction << 4),         // 00
			anywhereState | (ignoreAction << 4),         // 01
			anywhereState | (ignoreAction << 4),         // 02
			anywhereState | (ignoreAction << 4),         // 03
			anywhereState | (ignoreAction << 4),         // 04
			anywhereState | (ignoreAction << 4),         // 05
			anywhereState | (ignoreAction << 4),         // 06
			anywhereState | (ignoreAction << 4),         // 07
			anywhereState | (ignoreAction << 4),         // 08
			anywhereState | (ignoreAction << 4),         // 09
			anywhereState | (ignoreAction << 4),         // 0A
			anywhereState | (ignoreAction << 4),         // 0B
			anywhereState | (ignoreAction << 4),         // 0C
			anywhereState | (ignoreAction << 4),         // 0D
			anywhereState | (ignoreAction << 4),         // 0E
			anywhereState | (ignoreAction << 4),         // 0F
			anywhereState | (ignoreAction << 4),         // 10
			anywhereState | (ignoreAction << 4),         // 11
			anywhereState | (ignoreAction << 4),         // 12
			anywhereState | (ignoreAction << 4),         // 13
			anywhereState | (ignoreAction << 4),         // 14
			anywhereState | (ignoreAction << 4),         // 15
			anywhereState | (ignoreAction << 4),         // 16
			anywhereState | (ignoreAction << 4),         // 17
			0,                                           // 18
			anywhereState | (ignoreAction << 4),         // 19
			0,                                           // 1A
			0,                                           // 1B
			anywhereState | (ignoreAction << 4),         // 1C
			anywhereState | (ignoreAction << 4),         // 1D
			anywhereState | (ignoreAction << 4),         // 1E
			anywhereState | (ignoreAction << 4),         // 1F
			dcsIntermediateState | (collectAction << 4), // 20
			dcsIntermediateState | (collectAction << 4), // 21
			dcsIntermediateState | (collectAction << 4), // 22
			dcsIntermediateState | (collectAction << 4), // 23
			dcsIntermediateState | (collectAction << 4), // 24
			dcsIntermediateState | (collectAction << 4), // 25
			dcsIntermediateState | (collectAction << 4), // 26
			dcsIntermediateState | (collectAction << 4), // 27
			dcsIntermediateState | (collectAction << 4), // 28
			dcsIntermediateState | (collectAction << 4), // 29
			dcsIntermediateState | (collectAction << 4), // 2A
			dcsIntermediateState | (collectAction << 4), // 2B
			dcsIntermediateState | (collectAction << 4), // 2C
			dcsIntermediateState | (collectAction << 4), // 2D
			dcsIntermediateState | (collectAction << 4), // 2E
			dcsIntermediateState | (collectAction << 4), // 2F
			dcsParamState | (paramAction << 4),          // 30
			dcsParamState | (paramAction << 4),          // 31
			dcsParamState | (paramAction << 4),          // 32
			dcsParamState | (paramAction << 4),          // 33
			dcsParamState | (paramAction << 4),          // 34
			dcsParamState | (paramAction << 4),          // 35
			dcsParamState | (paramAction << 4),          // 36
			dcsParamState | (paramAction << 4),          // 37
			dcsParamState | (paramAction << 4),          // 38
			dcsParamState | (paramAction << 4),          // 39
			dcsIgnoreState | (noneAction << 4),          // 3A
			dcsParamState | (paramAction << 4),          // 3B
			dcsParamState | (collectAction << 4),        // 3C
			dcsParamState | (collectAction << 4),        // 3D
			dcsParamState | (collectAction << 4),        // 3E
			dcsParamState | (collectAction << 4),        // 3F
			dcsPassthroughState | (noneAction << 4),     // 40
			dcsPassthroughState | (noneAction << 4),     // 41
			dcsPassthroughState | (noneAction << 4),     // 42
			dcsPassthroughState | (noneAction << 4),     // 43
			dcsPassthroughState | (noneAction << 4),     // 44
			dcsPassthroughState | (noneAction << 4),     // 45
			dcsPassthroughState | (noneAction << 4),     // 46
			dcsPassthroughState | (noneAction << 4),     // 47
			dcsPassthroughState | (noneAction << 4),     // 48
			dcsPassthroughState | (noneAction << 4),     // 49
			dcsPassthroughState | (noneAction << 4),     // 4A
			dcsPassthroughState | (noneAction << 4),     // 4B
			dcsPassthroughState | (noneAction << 4),     // 4C
			dcsPassthroughState | (noneAction << 4),     // 4D
			dcsPassthroughState | (noneAction << 4),     // 4E
			dcsPassthroughState | (noneAction << 4),     // 4F
			dcsPassthroughState | (noneAction << 4),     // 50
			dcsPassthroughState | (noneAction << 4),     // 51
			dcsPassthroughState | (noneAction << 4),     // 52
			dcsPassthroughState | (noneAction << 4),     // 53
			dcsPassthroughState | (noneAction << 4),     // 54
			dcsPassthroughState | (noneAction << 4),     // 55
			dcsPassthroughState | (noneAction << 4),     // 56
			dcsPassthroughState | (noneAction << 4),     // 57
			dcsPassthroughState | (noneAction << 4),     // 58
			dcsPassthroughState | (noneAction << 4),     // 59
			dcsPassthroughState | (noneAction << 4),     // 5A
			dcsPassthroughState | (noneAction << 4),     // 5B
			dcsPassthroughState | (noneAction << 4),     // 5C
			dcsPassthroughState | (noneAction << 4),     // 5D
			dcsPassthroughState | (noneAction << 4),     // 5E
			dcsPassthroughState | (noneAction << 4),     // 5F
			dcsPassthroughState | (noneAction << 4),     // 60
			dcsPassthroughState | (noneAction << 4),     // 61
			dcsPassthroughState | (noneAction << 4),     // 62
			dcsPassthroughState | (noneAction << 4),     // 63
			dcsPassthroughState | (noneAction << 4),     // 64
			dcsPassthroughState | (noneAction << 4),     // 65
			dcsPassthroughState | (noneAction << 4),     // 66
			dcsPassthroughState | (noneAction << 4),     // 67
			dcsPassthroughState | (noneAction << 4),     // 68
			dcsPassthroughState | (noneAction << 4),     // 69
			dcsPassthroughState | (noneAction << 4),     // 6A
			dcsPassthroughState | (noneAction << 4),     // 6B
			dcsPassthroughState | (noneAction << 4),     // 6C
			dcsPassthroughState | (noneAction << 4),     // 6D
			dcsPassthroughState | (noneAction << 4),     // 6E
			dcsPassthroughState | (noneAction << 4),     // 6F
			dcsPassthroughState | (noneAction << 4),     // 70
			dcsPassthroughState | (noneAction << 4),     // 71
			dcsPassthroughState | (noneAction << 4),     // 72
			dcsPassthroughState | (noneAction << 4),     // 73
			dcsPassthroughState | (noneAction << 4),     // 74
			dcsPassthroughState | (noneAction << 4),     // 75
			dcsPassthroughState | (noneAction << 4),     // 76
			dcsPassthroughState | (noneAction << 4),     // 77
			dcsPassthroughState | (noneAction << 4),     // 78
			dcsPassthroughState | (noneAction << 4),     // 79
			dcsPassthroughState | (noneAction << 4),     // 7A
			dcsPassthroughState | (noneAction << 4),     // 7B
			dcsPassthroughState | (noneAction << 4),     // 7C
			dcsPassthroughState | (noneAction << 4),     // 7D
			dcsPassthroughState | (noneAction << 4),     // 7E
			anywhereState | (ignoreAction << 4),         // 7F
			0,                                           // 80
			0,                                           // 81
			0,                                           // 82
			0,                                           // 83
			0,                                           // 84
			0,                                           // 85
			0,                                           // 86
			0,                                           // 87
			0,                                           // 88
			0,                                           // 89
			0,                                           // 8A
			0,                                           // 8B
			0,                                           // 8C
			0,                                           // 8D
			0,                                           // 8E
			0,                                           // 8F
			0,                                           // 90
			0,                                           // 91
			0,                                           // 92
			0,                                           // 93
			0,                                           // 94
			0,                                           // 95
			0,                                           // 96
			0,                                           // 97
			0,                                           // 98
			0,                                           // 99
			0,                                           // 9A
			0,                                           // 9B
			0,                                           // 9C
			0,                                           // 9D
			0,                                           // 9E
			0,                                           // 9F
			0,                                           // A0
			0,                                           // A1
			0,                                           // A2
			0,                                           // A3
			0,                                           // A4
			0,                                           // A5
			0,                                           // A6
			0,                                           // A7
			0,                                           // A8
			0,                                           // A9
			0,                                           // AA
			0,                                           // AB
			0,                                           // AC
			0,                                           // AD
			0,                                           // AE
			0,                                           // AF
			0,                                           // B0
			0,                                           // B1
			0,                                           // B2
			0,                                           // B3
			0,                                           // B4
			0,                                           // B5
			0,                                           // B6
			0,                                           // B7
			0,                                           // B8
			0,                                           // B9
			0,                                           // BA
			0,                                           // BB
			0,                                           // BC
			0,                                           // BD
			0,                                           // BE
			0,                                           // BF
			0,                                           // C0
			0,                                           // C1
			0,                                           // C2
			0,                                           // C3
			0,                                           // C4
			0,                                           // C5
			0,                                           // C6
			0,                                           // C7
			0,                                           // C8
			0,                                           // C9
			0,                                           // CA
			0,                                           // CB
			0,                                           // CC
			0,                                           // CD
			0,                                           // CE
			0,                                           // CF
			0,                                           // D0
			0,                                           // D1
			0,                                           // D2
			0,                                           // D3
			0,                                           // D4
			0,                                           // D5
			0,                                           // D6
			0,                                           // D7
			0,                                           // D8
			0,                                           // D9
			0,                                           // DA
			0,                                           // DB
			0,                                           // DC
			0,                                           // DD
			0,                                           // DE
			0,                                           // DF
			0,                                           // E0
			0,                                           // E1
			0,                                           // E2
			0,                                           // E3
			0,                                           // E4
			0,                                           // E5
			0,                                           // E6
			0,                                           // E7
			0,                                           // E8
			0,                                           // E9
			0,                                           // EA
			0,                                           // EB
			0,                                           // EC
			0,                                           // ED
			0,                                           // EE
			0,                                           // EF
			0,                                           // F0
			0,                                           // F1
			0,                                           // F2
			0,                                           // F3
			0,                                           // F4
			0,                                           // F5
			0,                                           // F6
			0,                                           // F7
			0,                                           // F8
			0,                                           // F9
			0,                                           // FA
			0,                                           // FB
			0,                                           // FC
			0,                                           // FD
			0,                                           // FE
			0,                                           // FF
		},
		{ //State dcsIgnoreState = 6
			anywhereState | (ignoreAction << 4), // 00
			anywhereState | (ignoreAction << 4), // 01
			anywhereState | (ignoreAction << 4), // 02
			anywhereState | (ignoreAction << 4), // 03
			anywhereState | (ignoreAction << 4), // 04
			anywhereState | (ignoreAction << 4), // 05
			anywhereState | (ignoreAction << 4), // 06
			anywhereState | (ignoreAction << 4), // 07
			anywhereState | (ignoreAction << 4), // 08
			anywhereState | (ignoreAction << 4), // 09
			anywhereState | (ignoreAction << 4), // 0A
			anywhereState | (ignoreAction << 4), // 0B
			anywhereState | (ignoreAction << 4), // 0C
			anywhereState | (ignoreAction << 4), // 0D
			anywhereState | (ignoreAction << 4), // 0E
			anywhereState | (ignoreAction << 4), // 0F
			anywhereState | (ignoreAction << 4), // 10
			anywhereState | (ignoreAction << 4), // 11
			anywhereState | (ignoreAction << 4), // 12
			anywhereState | (ignoreAction << 4), // 13
			anywhereState | (ignoreAction << 4), // 14
			anywhereState | (ignoreAction << 4), // 15
			anywhereState | (ignoreAction << 4), // 16
			anywhereState | (ignoreAction << 4), // 17
			0,                                   // 18
			anywhereState | (ignoreAction << 4), // 19
			0,                                   // 1A
			0,                                   // 1B
			anywhereState | (ignoreAction << 4), // 1C
			anywhereState | (ignoreAction << 4), // 1D
			anywhereState | (ignoreAction << 4), // 1E
			anywhereState | (ignoreAction << 4), // 1F
			anywhereState | (ignoreAction << 4), // 20
			anywhereState | (ignoreAction << 4), // 21
			anywhereState | (ignoreAction << 4), // 22
			anywhereState | (ignoreAction << 4), // 23
			anywhereState | (ignoreAction << 4), // 24
			anywhereState | (ignoreAction << 4), // 25
			anywhereState | (ignoreAction << 4), // 26
			anywhereState | (ignoreAction << 4), // 27
			anywhereState | (ignoreAction << 4), // 28
			anywhereState | (ignoreAction << 4), // 29
			anywhereState | (ignoreAction << 4), // 2A
			anywhereState | (ignoreAction << 4), // 2B
			anywhereState | (ignoreAction << 4), // 2C
			anywhereState | (ignoreAction << 4), // 2D
			anywhereState | (ignoreAction << 4), // 2E
			anywhereState | (ignoreAction << 4), // 2F
			anywhereState | (ignoreAction << 4), // 30
			anywhereState | (ignoreAction << 4), // 31
			anywhereState | (ignoreAction << 4), // 32
			anywhereState | (ignoreAction << 4), // 33
			anywhereState | (ignoreAction << 4), // 34
			anywhereState | (ignoreAction << 4), // 35
			anywhereState | (ignoreAction << 4), // 36
			anywhereState | (ignoreAction << 4), // 37
			anywhereState | (ignoreAction << 4), // 38
			anywhereState | (ignoreAction << 4), // 39
			anywhereState | (ignoreAction << 4), // 3A
			anywhereState | (ignoreAction << 4), // 3B
			anywhereState | (ignoreAction << 4), // 3C
			anywhereState | (ignoreAction << 4), // 3D
			anywhereState | (ignoreAction << 4), // 3E
			anywhereState | (ignoreAction << 4), // 3F
			anywhereState | (ignoreAction << 4), // 40
			anywhereState | (ignoreAction << 4), // 41
			anywhereState | (ignoreAction << 4), // 42
			anywhereState | (ignoreAction << 4), // 43
			anywhereState | (ignoreAction << 4), // 44
			anywhereState | (ignoreAction << 4), // 45
			anywhereState | (ignoreAction << 4), // 46
			anywhereState | (ignoreAction << 4), // 47
			anywhereState | (ignoreAction << 4), // 48
			anywhereState | (ignoreAction << 4), // 49
			anywhereState | (ignoreAction << 4), // 4A
			anywhereState | (ignoreAction << 4), // 4B
			anywhereState | (ignoreAction << 4), // 4C
			anywhereState | (ignoreAction << 4), // 4D
			anywhereState | (ignoreAction << 4), // 4E
			anywhereState | (ignoreAction << 4), // 4F
			anywhereState | (ignoreAction << 4), // 50
			anywhereState | (ignoreAction << 4), // 51
			anywhereState | (ignoreAction << 4), // 52
			anywhereState | (ignoreAction << 4), // 53
			anywhereState | (ignoreAction << 4), // 54
			anywhereState | (ignoreAction << 4), // 55
			anywhereState | (ignoreAction << 4), // 56
			anywhereState | (ignoreAction << 4), // 57
			anywhereState | (ignoreAction << 4), // 58
			anywhereState | (ignoreAction << 4), // 59
			anywhereState | (ignoreAction << 4), // 5A
			anywhereState | (ignoreAction << 4), // 5B
			anywhereState | (ignoreAction << 4), // 5C
			anywhereState | (ignoreAction << 4), // 5D
			anywhereState | (ignoreAction << 4), // 5E
			anywhereState | (ignoreAction << 4), // 5F
			anywhereState | (ignoreAction << 4), // 60
			anywhereState | (ignoreAction << 4), // 61
			anywhereState | (ignoreAction << 4), // 62
			anywhereState | (ignoreAction << 4), // 63
			anywhereState | (ignoreAction << 4), // 64
			anywhereState | (ignoreAction << 4), // 65
			anywhereState | (ignoreAction << 4), // 66
			anywhereState | (ignoreAction << 4), // 67
			anywhereState | (ignoreAction << 4), // 68
			anywhereState | (ignoreAction << 4), // 69
			anywhereState | (ignoreAction << 4), // 6A
			anywhereState | (ignoreAction << 4), // 6B
			anywhereState | (ignoreAction << 4), // 6C
			anywhereState | (ignoreAction << 4), // 6D
			anywhereState | (ignoreAction << 4), // 6E
			anywhereState | (ignoreAction << 4), // 6F
			anywhereState | (ignoreAction << 4), // 70
			anywhereState | (ignoreAction << 4), // 71
			anywhereState | (ignoreAction << 4), // 72
			anywhereState | (ignoreAction << 4), // 73
			anywhereState | (ignoreAction << 4), // 74
			anywhereState | (ignoreAction << 4), // 75
			anywhereState | (ignoreAction << 4), // 76
			anywhereState | (ignoreAction << 4), // 77
			anywhereState | (ignoreAction << 4), // 78
			anywhereState | (ignoreAction << 4), // 79
			anywhereState | (ignoreAction << 4), // 7A
			anywhereState | (ignoreAction << 4), // 7B
			anywhereState | (ignoreAction << 4), // 7C
			anywhereState | (ignoreAction << 4), // 7D
			anywhereState | (ignoreAction << 4), // 7E
			anywhereState | (ignoreAction << 4), // 7F
			0,                                   // 80
			0,                                   // 81
			0,                                   // 82
			0,                                   // 83
			0,                                   // 84
			0,                                   // 85
			0,                                   // 86
			0,                                   // 87
			0,                                   // 88
			0,                                   // 89
			0,                                   // 8A
			0,                                   // 8B
			0,                                   // 8C
			0,                                   // 8D
			0,                                   // 8E
			0,                                   // 8F
			0,                                   // 90
			0,                                   // 91
			0,                                   // 92
			0,                                   // 93
			0,                                   // 94
			0,                                   // 95
			0,                                   // 96
			0,                                   // 97
			0,                                   // 98
			0,                                   // 99
			0,                                   // 9A
			0,                                   // 9B
			groundState | (noneAction << 4),     // 9C
			0,                                   // 9D
			0,                                   // 9E
			0,                                   // 9F
			0,                                   // A0
			0,                                   // A1
			0,                                   // A2
			0,                                   // A3
			0,                                   // A4
			0,                                   // A5
			0,                                   // A6
			0,                                   // A7
			0,                                   // A8
			0,                                   // A9
			0,                                   // AA
			0,                                   // AB
			0,                                   // AC
			0,                                   // AD
			0,                                   // AE
			0,                                   // AF
			0,                                   // B0
			0,                                   // B1
			0,                                   // B2
			0,                                   // B3
			0,                                   // B4
			0,                                   // B5
			0,                                   // B6
			0,                                   // B7
			0,                                   // B8
			0,                                   // B9
			0,                                   // BA
			0,                                   // BB
			0,                                   // BC
			0,                                   // BD
			0,                                   // BE
			0,                                   // BF
			0,                                   // C0
			0,                                   // C1
			0,                                   // C2
			0,                                   // C3
			0,                                   // C4
			0,                                   // C5
			0,                                   // C6
			0,                                   // C7
			0,                                   // C8
			0,                                   // C9
			0,                                   // CA
			0,                                   // CB
			0,                                   // CC
			0,                                   // CD
			0,                                   // CE
			0,                                   // CF
			0,                                   // D0
			0,                                   // D1
			0,                                   // D2
			0,                                   // D3
			0,                                   // D4
			0,                                   // D5
			0,                                   // D6
			0,                                   // D7
			0,                                   // D8
			0,                                   // D9
			0,                                   // DA
			0,                                   // DB
			0,                                   // DC
			0,                                   // DD
			0,                                   // DE
			0,                                   // DF
			0,                                   // E0
			0,                                   // E1
			0,                                   // E2
			0,                                   // E3
			0,                                   // E4
			0,                                   // E5
			0,                                   // E6
			0,                                   // E7
			0,                                   // E8
			0,                                   // E9
			0,                                   // EA
			0,                                   // EB
			0,                                   // EC
			0,                                   // ED
			0,                                   // EE
			0,                                   // EF
			0,                                   // F0
			0,                                   // F1
			0,                                   // F2
			0,                                   // F3
			0,                                   // F4
			0,                                   // F5
			0,                                   // F6
			0,                                   // F7
			0,                                   // F8
			0,                                   // F9
			0,                                   // FA
			0,                                   // FB
			0,                                   // FC
			0,                                   // FD
			0,                                   // FE
			0,                                   // FF
		},
		{ //State dcsIntermediateState = 7
			anywhereState | (ignoreAction << 4),     // 00
			anywhereState | (ignoreAction << 4),     // 01
			anywhereState | (ignoreAction << 4),     // 02
			anywhereState | (ignoreAction << 4),     // 03
			anywhereState | (ignoreAction << 4),     // 04
			anywhereState | (ignoreAction << 4),     // 05
			anywhereState | (ignoreAction << 4),     // 06
			anywhereState | (ignoreAction << 4),     // 07
			anywhereState | (ignoreAction << 4),     // 08
			anywhereState | (ignoreAction << 4),     // 09
			anywhereState | (ignoreAction << 4),     // 0A
			anywhereState | (ignoreAction << 4),     // 0B
			anywhereState | (ignoreAction << 4),     // 0C
			anywhereState | (ignoreAction << 4),     // 0D
			anywhereState | (ignoreAction << 4),     // 0E
			anywhereState | (ignoreAction << 4),     // 0F
			anywhereState | (ignoreAction << 4),     // 10
			anywhereState | (ignoreAction << 4),     // 11
			anywhereState | (ignoreAction << 4),     // 12
			anywhereState | (ignoreAction << 4),     // 13
			anywhereState | (ignoreAction << 4),     // 14
			anywhereState | (ignoreAction << 4),     // 15
			anywhereState | (ignoreAction << 4),     // 16
			anywhereState | (ignoreAction << 4),     // 17
			0,                                       // 18
			anywhereState | (ignoreAction << 4),     // 19
			0,                                       // 1A
			0,                                       // 1B
			anywhereState | (ignoreAction << 4),     // 1C
			anywhereState | (ignoreAction << 4),     // 1D
			anywhereState | (ignoreAction << 4),     // 1E
			anywhereState | (ignoreAction << 4),     // 1F
			anywhereState | (collectAction << 4),    // 20
			anywhereState | (collectAction << 4),    // 21
			anywhereState | (collectAction << 4),    // 22
			anywhereState | (collectAction << 4),    // 23
			anywhereState | (collectAction << 4),    // 24
			anywhereState | (collectAction << 4),    // 25
			anywhereState | (collectAction << 4),    // 26
			anywhereState | (collectAction << 4),    // 27
			anywhereState | (collectAction << 4),    // 28
			anywhereState | (collectAction << 4),    // 29
			anywhereState | (collectAction << 4),    // 2A
			anywhereState | (collectAction << 4),    // 2B
			anywhereState | (collectAction << 4),    // 2C
			anywhereState | (collectAction << 4),    // 2D
			anywhereState | (collectAction << 4),    // 2E
			anywhereState | (collectAction << 4),    // 2F
			dcsIgnoreState | (noneAction << 4),      // 30
			dcsIgnoreState | (noneAction << 4),      // 31
			dcsIgnoreState | (noneAction << 4),      // 32
			dcsIgnoreState | (noneAction << 4),      // 33
			dcsIgnoreState | (noneAction << 4),      // 34
			dcsIgnoreState | (noneAction << 4),      // 35
			dcsIgnoreState | (noneAction << 4),      // 36
			dcsIgnoreState | (noneAction << 4),      // 37
			dcsIgnoreState | (noneAction << 4),      // 38
			dcsIgnoreState | (noneAction << 4),      // 39
			dcsIgnoreState | (noneAction << 4),      // 3A
			dcsIgnoreState | (noneAction << 4),      // 3B
			dcsIgnoreState | (noneAction << 4),      // 3C
			dcsIgnoreState | (noneAction << 4),      // 3D
			dcsIgnoreState | (noneAction << 4),      // 3E
			dcsIgnoreState | (noneAction << 4),      // 3F
			dcsPassthroughState | (noneAction << 4), // 40
			dcsPassthroughState | (noneAction << 4), // 41
			dcsPassthroughState | (noneAction << 4), // 42
			dcsPassthroughState | (noneAction << 4), // 43
			dcsPassthroughState | (noneAction << 4), // 44
			dcsPassthroughState | (noneAction << 4), // 45
			dcsPassthroughState | (noneAction << 4), // 46
			dcsPassthroughState | (noneAction << 4), // 47
			dcsPassthroughState | (noneAction << 4), // 48
			dcsPassthroughState | (noneAction << 4), // 49
			dcsPassthroughState | (noneAction << 4), // 4A
			dcsPassthroughState | (noneAction << 4), // 4B
			dcsPassthroughState | (noneAction << 4), // 4C
			dcsPassthroughState | (noneAction << 4), // 4D
			dcsPassthroughState | (noneAction << 4), // 4E
			dcsPassthroughState | (noneAction << 4), // 4F
			dcsPassthroughState | (noneAction << 4), // 50
			dcsPassthroughState | (noneAction << 4), // 51
			dcsPassthroughState | (noneAction << 4), // 52
			dcsPassthroughState | (noneAction << 4), // 53
			dcsPassthroughState | (noneAction << 4), // 54
			dcsPassthroughState | (noneAction << 4), // 55
			dcsPassthroughState | (noneAction << 4), // 56
			dcsPassthroughState | (noneAction << 4), // 57
			dcsPassthroughState | (noneAction << 4), // 58
			dcsPassthroughState | (noneAction << 4), // 59
			dcsPassthroughState | (noneAction << 4), // 5A
			dcsPassthroughState | (noneAction << 4), // 5B
			dcsPassthroughState | (noneAction << 4), // 5C
			dcsPassthroughState | (noneAction << 4), // 5D
			dcsPassthroughState | (noneAction << 4), // 5E
			dcsPassthroughState | (noneAction << 4), // 5F
			dcsPassthroughState | (noneAction << 4), // 60
			dcsPassthroughState | (noneAction << 4), // 61
			dcsPassthroughState | (noneAction << 4), // 62
			dcsPassthroughState | (noneAction << 4), // 63
			dcsPassthroughState | (noneAction << 4), // 64
			dcsPassthroughState | (noneAction << 4), // 65
			dcsPassthroughState | (noneAction << 4), // 66
			dcsPassthroughState | (noneAction << 4), // 67
			dcsPassthroughState | (noneAction << 4), // 68
			dcsPassthroughState | (noneAction << 4), // 69
			dcsPassthroughState | (noneAction << 4), // 6A
			dcsPassthroughState | (noneAction << 4), // 6B
			dcsPassthroughState | (noneAction << 4), // 6C
			dcsPassthroughState | (noneAction << 4), // 6D
			dcsPassthroughState | (noneAction << 4), // 6E
			dcsPassthroughState | (noneAction << 4), // 6F
			dcsPassthroughState | (noneAction << 4), // 70
			dcsPassthroughState | (noneAction << 4), // 71
			dcsPassthroughState | (noneAction << 4), // 72
			dcsPassthroughState | (noneAction << 4), // 73
			dcsPassthroughState | (noneAction << 4), // 74
			dcsPassthroughState | (noneAction << 4), // 75
			dcsPassthroughState | (noneAction << 4), // 76
			dcsPassthroughState | (noneAction << 4), // 77
			dcsPassthroughState | (noneAction << 4), // 78
			dcsPassthroughState | (noneAction << 4), // 79
			dcsPassthroughState | (noneAction << 4), // 7A
			dcsPassthroughState | (noneAction << 4), // 7B
			dcsPassthroughState | (noneAction << 4), // 7C
			dcsPassthroughState | (noneAction << 4), // 7D
			dcsPassthroughState | (noneAction << 4), // 7E
			anywhereState | (ignoreAction << 4),     // 7F
			0,                                       // 80
			0,                                       // 81
			0,                                       // 82
			0,                                       // 83
			0,                                       // 84
			0,                                       // 85
			0,                                       // 86
			0,                                       // 87
			0,                                       // 88
			0,                                       // 89
			0,                                       // 8A
			0,                                       // 8B
			0,                                       // 8C
			0,                                       // 8D
			0,                                       // 8E
			0,                                       // 8F
			0,                                       // 90
			0,                                       // 91
			0,                                       // 92
			0,                                       // 93
			0,                                       // 94
			0,                                       // 95
			0,                                       // 96
			0,                                       // 97
			0,                                       // 98
			0,                                       // 99
			0,                                       // 9A
			0,                                       // 9B
			0,                                       // 9C
			0,                                       // 9D
			0,                                       // 9E
			0,                                       // 9F
			0,                                       // A0
			0,                                       // A1
			0,                                       // A2
			0,                                       // A3
			0,                                       // A4
			0,                                       // A5
			0,                                       // A6
			0,                                       // A7
			0,                                       // A8
			0,                                       // A9
			0,                                       // AA
			0,                                       // AB
			0,                                       // AC
			0,                                       // AD
			0,                                       // AE
			0,                                       // AF
			0,                                       // B0
			0,                                       // B1
			0,                                       // B2
			0,                                       // B3
			0,                                       // B4
			0,                                       // B5
			0,                                       // B6
			0,                                       // B7
			0,                                       // B8
			0,                                       // B9
			0,                                       // BA
			0,                                       // BB
			0,                                       // BC
			0,                                       // BD
			0,                                       // BE
			0,                                       // BF
			0,                                       // C0
			0,                                       // C1
			0,                                       // C2
			0,                                       // C3
			0,                                       // C4
			0,                                       // C5
			0,                                       // C6
			0,                                       // C7
			0,                                       // C8
			0,                                       // C9
			0,                                       // CA
			0,                                       // CB
			0,                                       // CC
			0,                                       // CD
			0,                                       // CE
			0,                                       // CF
			0,                                       // D0
			0,                                       // D1
			0,                                       // D2
			0,                                       // D3
			0,                                       // D4
			0,                                       // D5
			0,                                       // D6
			0,                                       // D7
			0,                                       // D8
			0,                                       // D9
			0,                                       // DA
			0,                                       // DB
			0,                                       // DC
			0,                                       // DD
			0,                                       // DE
			0,                                       // DF
			0,                                       // E0
			0,                                       // E1
			0,                                       // E2
			0,                                       // E3
			0,                                       // E4
			0,                                       // E5
			0,                                       // E6
			0,                                       // E7
			0,                                       // E8
			0,                                       // E9
			0,                                       // EA
			0,                                       // EB
			0,                                       // EC
			0,                                       // ED
			0,                                       // EE
			0,                                       // EF
			0,                                       // F0
			0,                                       // F1
			0,                                       // F2
			0,                                       // F3
			0,                                       // F4
			0,                                       // F5
			0,                                       // F6
			0,                                       // F7
			0,                                       // F8
			0,                                       // F9
			0,                                       // FA
			0,                                       // FB
			0,                                       // FC
			0,                                       // FD
			0,                                       // FE
			0,                                       // FF
		},
		{ //State dcsParamState = 8
			anywhereState | (ignoreAction << 4),         // 00
			anywhereState | (ignoreAction << 4),         // 01
			anywhereState | (ignoreAction << 4),         // 02
			anywhereState | (ignoreAction << 4),         // 03
			anywhereState | (ignoreAction << 4),         // 04
			anywhereState | (ignoreAction << 4),         // 05
			anywhereState | (ignoreAction << 4),         // 06
			anywhereState | (ignoreAction << 4),         // 07
			anywhereState | (ignoreAction << 4),         // 08
			anywhereState | (ignoreAction << 4),         // 09
			anywhereState | (ignoreAction << 4),         // 0A
			anywhereState | (ignoreAction << 4),         // 0B
			anywhereState | (ignoreAction << 4),         // 0C
			anywhereState | (ignoreAction << 4),         // 0D
			anywhereState | (ignoreAction << 4),         // 0E
			anywhereState | (ignoreAction << 4),         // 0F
			anywhereState | (ignoreAction << 4),         // 10
			anywhereState | (ignoreAction << 4),         // 11
			anywhereState | (ignoreAction << 4),         // 12
			anywhereState | (ignoreAction << 4),         // 13
			anywhereState | (ignoreAction << 4),         // 14
			anywhereState | (ignoreAction << 4),         // 15
			anywhereState | (ignoreAction << 4),         // 16
			anywhereState | (ignoreAction << 4),         // 17
			0,                                           // 18
			anywhereState | (ignoreAction << 4),         // 19
			0,                                           // 1A
			0,                                           // 1B
			anywhereState | (ignoreAction << 4),         // 1C
			anywhereState | (ignoreAction << 4),         // 1D
			anywhereState | (ignoreAction << 4),         // 1E
			anywhereState | (ignoreAction << 4),         // 1F
			dcsIntermediateState | (collectAction << 4), // 20
			dcsIntermediateState | (collectAction << 4), // 21
			dcsIntermediateState | (collectAction << 4), // 22
			dcsIntermediateState | (collectAction << 4), // 23
			dcsIntermediateState | (collectAction << 4), // 24
			dcsIntermediateState | (collectAction << 4), // 25
			dcsIntermediateState | (collectAction << 4), // 26
			dcsIntermediateState | (collectAction << 4), // 27
			dcsIntermediateState | (collectAction << 4), // 28
			dcsIntermediateState | (collectAction << 4), // 29
			dcsIntermediateState | (collectAction << 4), // 2A
			dcsIntermediateState | (collectAction << 4), // 2B
			dcsIntermediateState | (collectAction << 4), // 2C
			dcsIntermediateState | (collectAction << 4), // 2D
			dcsIntermediateState | (collectAction << 4), // 2E
			dcsIntermediateState | (collectAction << 4), // 2F
			anywhereState | (paramAction << 4),          // 30
			anywhereState | (paramAction << 4),          // 31
			anywhereState | (paramAction << 4),          // 32
			anywhereState | (paramAction << 4),          // 33
			anywhereState | (paramAction << 4),          // 34
			anywhereState | (paramAction << 4),          // 35
			anywhereState | (paramAction << 4),          // 36
			anywhereState | (paramAction << 4),          // 37
			anywhereState | (paramAction << 4),          // 38
			anywhereState | (paramAction << 4),          // 39
			dcsIgnoreState | (noneAction << 4),          // 3A
			anywhereState | (paramAction << 4),          // 3B
			dcsIgnoreState | (noneAction << 4),          // 3C
			dcsIgnoreState | (noneAction << 4),          // 3D
			dcsIgnoreState | (noneAction << 4),          // 3E
			dcsIgnoreState | (noneAction << 4),          // 3F
			dcsPassthroughState | (noneAction << 4),     // 40
			dcsPassthroughState | (noneAction << 4),     // 41
			dcsPassthroughState | (noneAction << 4),     // 42
			dcsPassthroughState | (noneAction << 4),     // 43
			dcsPassthroughState | (noneAction << 4),     // 44
			dcsPassthroughState | (noneAction << 4),     // 45
			dcsPassthroughState | (noneAction << 4),     // 46
			dcsPassthroughState | (noneAction << 4),     // 47
			dcsPassthroughState | (noneAction << 4),     // 48
			dcsPassthroughState | (noneAction << 4),     // 49
			dcsPassthroughState | (noneAction << 4),     // 4A
			dcsPassthroughState | (noneAction << 4),     // 4B
			dcsPassthroughState | (noneAction << 4),     // 4C
			dcsPassthroughState | (noneAction << 4),     // 4D
			dcsPassthroughState | (noneAction << 4),     // 4E
			dcsPassthroughState | (noneAction << 4),     // 4F
			dcsPassthroughState | (noneAction << 4),     // 50
			dcsPassthroughState | (noneAction << 4),     // 51
			dcsPassthroughState | (noneAction << 4),     // 52
			dcsPassthroughState | (noneAction << 4),     // 53
			dcsPassthroughState | (noneAction << 4),     // 54
			dcsPassthroughState | (noneAction << 4),     // 55
			dcsPassthroughState | (noneAction << 4),     // 56
			dcsPassthroughState | (noneAction << 4),     // 57
			dcsPassthroughState | (noneAction << 4),     // 58
			dcsPassthroughState | (noneAction << 4),     // 59
			dcsPassthroughState | (noneAction << 4),     // 5A
			dcsPassthroughState | (noneAction << 4),     // 5B
			dcsPassthroughState | (noneAction << 4),     // 5C
			dcsPassthroughState | (noneAction << 4),     // 5D
			dcsPassthroughState | (noneAction << 4),     // 5E
			dcsPassthroughState | (noneAction << 4),     // 5F
			dcsPassthroughState | (noneAction << 4),     // 60
			dcsPassthroughState | (noneAction << 4),     // 61
			dcsPassthroughState | (noneAction << 4),     // 62
			dcsPassthroughState | (noneAction << 4),     // 63
			dcsPassthroughState | (noneAction << 4),     // 64
			dcsPassthroughState | (noneAction << 4),     // 65
			dcsPassthroughState | (noneAction << 4),     // 66
			dcsPassthroughState | (noneAction << 4),     // 67
			dcsPassthroughState | (noneAction << 4),     // 68
			dcsPassthroughState | (noneAction << 4),     // 69
			dcsPassthroughState | (noneAction << 4),     // 6A
			dcsPassthroughState | (noneAction << 4),     // 6B
			dcsPassthroughState | (noneAction << 4),     // 6C
			dcsPassthroughState | (noneAction << 4),     // 6D
			dcsPassthroughState | (noneAction << 4),     // 6E
			dcsPassthroughState | (noneAction << 4),     // 6F
			dcsPassthroughState | (noneAction << 4),     // 70
			dcsPassthroughState | (noneAction << 4),     // 71
			dcsPassthroughState | (noneAction << 4),     // 72
			dcsPassthroughState | (noneAction << 4),     // 73
			dcsPassthroughState | (noneAction << 4),     // 74
			dcsPassthroughState | (noneAction << 4),     // 75
			dcsPassthroughState | (noneAction << 4),     // 76
			dcsPassthroughState | (noneAction << 4),     // 77
			dcsPassthroughState | (noneAction << 4),     // 78
			dcsPassthroughState | (noneAction << 4),     // 79
			dcsPassthroughState | (noneAction << 4),     // 7A
			dcsPassthroughState | (noneAction << 4),     // 7B
			dcsPassthroughState | (noneAction << 4),     // 7C
			dcsPassthroughState | (noneAction << 4),     // 7D
			dcsPassthroughState | (noneAction << 4),     // 7E
			anywhereState | (ignoreAction << 4),         // 7F
			0,                                           // 80
			0,                                           // 81
			0,                                           // 82
			0,                                           // 83
			0,                                           // 84
			0,                                           // 85
			0,                                           // 86
			0,                                           // 87
			0,                                           // 88
			0,                                           // 89
			0,                                           // 8A
			0,                                           // 8B
			0,                                           // 8C
			0,                                           // 8D
			0,                                           // 8E
			0,                                           // 8F
			0,                                           // 90
			0,                                           // 91
			0,                                           // 92
			0,                                           // 93
			0,                                           // 94
			0,                                           // 95
			0,                                           // 96
			0,                                           // 97
			0,                                           // 98
			0,                                           // 99
			0,                                           // 9A
			0,                                           // 9B
			0,                                           // 9C
			0,                                           // 9D
			0,                                           // 9E
			0,                                           // 9F
			0,                                           // A0
			0,                                           // A1
			0,                                           // A2
			0,                                           // A3
			0,                                           // A4
			0,                                           // A5
			0,                                           // A6
			0,                                           // A7
			0,                                           // A8
			0,                                           // A9
			0,                                           // AA
			0,                                           // AB
			0,                                           // AC
			0,                                           // AD
			0,                                           // AE
			0,                                           // AF
			0,                                           // B0
			0,                                           // B1
			0,                                           // B2
			0,                                           // B3
			0,                                           // B4
			0,                                           // B5
			0,                                           // B6
			0,                                           // B7
			0,                                           // B8
			0,                                           // B9
			0,                                           // BA
			0,                                           // BB
			0,                                           // BC
			0,                                           // BD
			0,                                           // BE
			0,                                           // BF
			0,                                           // C0
			0,                                           // C1
			0,                                           // C2
			0,                                           // C3
			0,                                           // C4
			0,                                           // C5
			0,                                           // C6
			0,                                           // C7
			0,                                           // C8
			0,                                           // C9
			0,                                           // CA
			0,                                           // CB
			0,                                           // CC
			0,                                           // CD
			0,                                           // CE
			0,                                           // CF
			0,                                           // D0
			0,                                           // D1
			0,                                           // D2
			0,                                           // D3
			0,                                           // D4
			0,                                           // D5
			0,                                           // D6
			0,                                           // D7
			0,                                           // D8
			0,                                           // D9
			0,                                           // DA
			0,                                           // DB
			0,                                           // DC
			0,                                           // DD
			0,                                           // DE
			0,                                           // DF
			0,                                           // E0
			0,                                           // E1
			0,                                           // E2
			0,                                           // E3
			0,                                           // E4
			0,                                           // E5
			0,                                           // E6
			0,                                           // E7
			0,                                           // E8
			0,                                           // E9
			0,                                           // EA
			0,                                           // EB
			0,                                           // EC
			0,                                           // ED
			0,                                           // EE
			0,                                           // EF
			0,                                           // F0
			0,                                           // F1
			0,                                           // F2
			0,                                           // F3
			0,                                           // F4
			0,                                           // F5
			0,                                           // F6
			0,                                           // F7
			0,                                           // F8
			0,                                           // F9
			0,                                           // FA
			0,                                           // FB
			0,                                           // FC
			0,                                           // FD
			0,                                           // FE
			0,                                           // FF
		},
		{ //State dcsPassthroughState = 9
			anywhereState | (putAction << 4),    // 00
			anywhereState | (putAction << 4),    // 01
			anywhereState | (putAction << 4),    // 02
			anywhereState | (putAction << 4),    // 03
			anywhereState | (putAction << 4),    // 04
			anywhereState | (putAction << 4),    // 05
			anywhereState | (putAction << 4),    // 06
			anywhereState | (putAction << 4),    // 07
			anywhereState | (putAction << 4),    // 08
			anywhereState | (putAction << 4),    // 09
			anywhereState | (putAction << 4),    // 0A
			anywhereState | (putAction << 4),    // 0B
			anywhereState | (putAction << 4),    // 0C
			anywhereState | (putAction << 4),    // 0D
			anywhereState | (putAction << 4),    // 0E
			anywhereState | (putAction << 4),    // 0F
			anywhereState | (putAction << 4),    // 10
			anywhereState | (putAction << 4),    // 11
			anywhereState | (putAction << 4),    // 12
			anywhereState | (putAction << 4),    // 13
			anywhereState | (putAction << 4),    // 14
			anywhereState | (putAction << 4),    // 15
			anywhereState | (putAction << 4),    // 16
			anywhereState | (putAction << 4),    // 17
			0,                                   // 18
			anywhereState | (putAction << 4),    // 19
			0,                                   // 1A
			0,                                   // 1B
			anywhereState | (putAction << 4),    // 1C
			anywhereState | (putAction << 4),    // 1D
			anywhereState | (putAction << 4),    // 1E
			anywhereState | (putAction << 4),    // 1F
			anywhereState | (putAction << 4),    // 20
			anywhereState | (putAction << 4),    // 21
			anywhereState | (putAction << 4),    // 22
			anywhereState | (putAction << 4),    // 23
			anywhereState | (putAction << 4),    // 24
			anywhereState | (putAction << 4),    // 25
			anywhereState | (putAction << 4),    // 26
			anywhereState | (putAction << 4),    // 27
			anywhereState | (putAction << 4),    // 28
			anywhereState | (putAction << 4),    // 29
			anywhereState | (putAction << 4),    // 2A
			anywhereState | (putAction << 4),    // 2B
			anywhereState | (putAction << 4),    // 2C
			anywhereState | (putAction << 4),    // 2D
			anywhereState | (putAction << 4),    // 2E
			anywhereState | (putAction << 4),    // 2F
			anywhereState | (putAction << 4),    // 30
			anywhereState | (putAction << 4),    // 31
			anywhereState | (putAction << 4),    // 32
			anywhereState | (putAction << 4),    // 33
			anywhereState | (putAction << 4),    // 34
			anywhereState | (putAction << 4),    // 35
			anywhereState | (putAction << 4),    // 36
			anywhereState | (putAction << 4),    // 37
			anywhereState | (putAction << 4),    // 38
			anywhereState | (putAction << 4),    // 39
			anywhereState | (putAction << 4),    // 3A
			anywhereState | (putAction << 4),    // 3B
			anywhereState | (putAction << 4),    // 3C
			anywhereState | (putAction << 4),    // 3D
			anywhereState | (putAction << 4),    // 3E
			anywhereState | (putAction << 4),    // 3F
			anywhereState | (putAction << 4),    // 40
			anywhereState | (putAction << 4),    // 41
			anywhereState | (putAction << 4),    // 42
			anywhereState | (putAction << 4),    // 43
			anywhereState | (putAction << 4),    // 44
			anywhereState | (putAction << 4),    // 45
			anywhereState | (putAction << 4),    // 46
			anywhereState | (putAction << 4),    // 47
			anywhereState | (putAction << 4),    // 48
			anywhereState | (putAction << 4),    // 49
			anywhereState | (putAction << 4),    // 4A
			anywhereState | (putAction << 4),    // 4B
			anywhereState | (putAction << 4),    // 4C
			anywhereState | (putAction << 4),    // 4D
			anywhereState | (putAction << 4),    // 4E
			anywhereState | (putAction << 4),    // 4F
			anywhereState | (putAction << 4),    // 50
			anywhereState | (putAction << 4),    // 51
			anywhereState | (putAction << 4),    // 52
			anywhereState | (putAction << 4),    // 53
			anywhereState | (putAction << 4),    // 54
			anywhereState | (putAction << 4),    // 55
			anywhereState | (putAction << 4),    // 56
			anywhereState | (putAction << 4),    // 57
			anywhereState | (putAction << 4),    // 58
			anywhereState | (putAction << 4),    // 59
			anywhereState | (putAction << 4),    // 5A
			anywhereState | (putAction << 4),    // 5B
			anywhereState | (putAction << 4),    // 5C
			anywhereState | (putAction << 4),    // 5D
			anywhereState | (putAction << 4),    // 5E
			anywhereState | (putAction << 4),    // 5F
			anywhereState | (putAction << 4),    // 60
			anywhereState | (putAction << 4),    // 61
			anywhereState | (putAction << 4),    // 62
			anywhereState | (putAction << 4),    // 63
			anywhereState | (putAction << 4),    // 64
			anywhereState | (putAction << 4),    // 65
			anywhereState | (putAction << 4),    // 66
			anywhereState | (putAction << 4),    // 67
			anywhereState | (putAction << 4),    // 68
			anywhereState | (putAction << 4),    // 69
			anywhereState | (putAction << 4),    // 6A
			anywhereState | (putAction << 4),    // 6B
			anywhereState | (putAction << 4),    // 6C
			anywhereState | (putAction << 4),    // 6D
			anywhereState | (putAction << 4),    // 6E
			anywhereState | (putAction << 4),    // 6F
			anywhereState | (putAction << 4),    // 70
			anywhereState | (putAction << 4),    // 71
			anywhereState | (putAction << 4),    // 72
			anywhereState | (putAction << 4),    // 73
			anywhereState | (putAction << 4),    // 74
			anywhereState | (putAction << 4),    // 75
			anywhereState | (putAction << 4),    // 76
			anywhereState | (putAction << 4),    // 77
			anywhereState | (putAction << 4),    // 78
			anywhereState | (putAction << 4),    // 79
			anywhereState | (putAction << 4),    // 7A
			anywhereState | (putAction << 4),    // 7B
			anywhereState | (putAction << 4),    // 7C
			anywhereState | (putAction << 4),    // 7D
			anywhereState | (putAction << 4),    // 7E
			anywhereState | (ignoreAction << 4), // 7F
			0,                                   // 80
			0,                                   // 81
			0,                                   // 82
			0,                                   // 83
			0,                                   // 84
			0,                                   // 85
			0,                                   // 86
			0,                                   // 87
			0,                                   // 88
			0,                                   // 89
			0,                                   // 8A
			0,                                   // 8B
			0,                                   // 8C
			0,                                   // 8D
			0,                                   // 8E
			0,                                   // 8F
			0,                                   // 90
			0,                                   // 91
			0,                                   // 92
			0,                                   // 93
			0,                                   // 94
			0,                                   // 95
			0,                                   // 96
			0,                                   // 97
			0,                                   // 98
			0,                                   // 99
			0,                                   // 9A
			0,                                   // 9B
			groundState | (noneAction << 4),     // 9C
			0,                                   // 9D
			0,                                   // 9E
			0,                                   // 9F
			0,                                   // A0
			0,                                   // A1
			0,                                   // A2
			0,                                   // A3
			0,                                   // A4
			0,                                   // A5
			0,                                   // A6
			0,                                   // A7
			0,                                   // A8
			0,                                   // A9
			0,                                   // AA
			0,                                   // AB
			0,                                   // AC
			0,                                   // AD
			0,                                   // AE
			0,                                   // AF
			0,                                   // B0
			0,                                   // B1
			0,                                   // B2
			0,                                   // B3
			0,                                   // B4
			0,                                   // B5
			0,                                   // B6
			0,                                   // B7
			0,                                   // B8
			0,                                   // B9
			0,                                   // BA
			0,                                   // BB
			0,                                   // BC
			0,                                   // BD
			0,                                   // BE
			0,                                   // BF
			0,                                   // C0
			0,                                   // C1
			0,                                   // C2
			0,                                   // C3
			0,                                   // C4
			0,                                   // C5
			0,                                   // C6
			0,                                   // C7
			0,                                   // C8
			0,                                   // C9
			0,                                   // CA
			0,                                   // CB
			0,                                   // CC
			0,                                   // CD
			0,                                   // CE
			0,                                   // CF
			0,                                   // D0
			0,                                   // D1
			0,                                   // D2
			0,                                   // D3
			0,                                   // D4
			0,                                   // D5
			0,                                   // D6
			0,                                   // D7
			0,                                   // D8
			0,                                   // D9
			0,                                   // DA
			0,                                   // DB
			0,                                   // DC
			0,                                   // DD
			0,                                   // DE
			0,                                   // DF
			0,                                   // E0
			0,                                   // E1
			0,                                   // E2
			0,                                   // E3
			0,                                   // E4
			0,                                   // E5
			0,                                   // E6
			0,                                   // E7
			0,                                   // E8
			0,                                   // E9
			0,                                   // EA
			0,                                   // EB
			0,                                   // EC
			0,                                   // ED
			0,                                   // EE
			0,                                   // EF
			0,                                   // F0
			0,                                   // F1
			0,                                   // F2
			0,                                   // F3
			0,                                   // F4
			0,                                   // F5
			0,                                   // F6
			0,                                   // F7
			0,                                   // F8
			0,                                   // F9
			0,                                   // FA
			0,                                   // FB
			0,                                   // FC
			0,                                   // FD
			0,                                   // FE
			0,                                   // FF
		},
		{ //State escapeState = 10
			anywhereState | (executeAction << 4),           // 00
			anywhereState | (executeAction << 4),           // 01
			anywhereState | (executeAction << 4),           // 02
			anywhereState | (executeAction << 4),           // 03
			anywhereState | (executeAction << 4),           // 04
			anywhereState | (executeAction << 4),           // 05
			anywhereState | (executeAction << 4),           // 06
			anywhereState | (executeAction << 4),           // 07
			anywhereState | (executeAction << 4),           // 08
			anywhereState | (executeAction << 4),           // 09
			anywhereState | (executeAction << 4),           // 0A
			anywhereState | (executeAction << 4),           // 0B
			anywhereState | (executeAction << 4),           // 0C
			anywhereState | (executeAction << 4),           // 0D
			anywhereState | (executeAction << 4),           // 0E
			anywhereState | (executeAction << 4),           // 0F
			anywhereState | (executeAction << 4),           // 10
			anywhereState | (executeAction << 4),           // 11
			anywhereState | (executeAction << 4),           // 12
			anywhereState | (executeAction << 4),           // 13
			anywhereState | (executeAction << 4),           // 14
			anywhereState | (executeAction << 4),           // 15
			anywhereState | (executeAction << 4),           // 16
			anywhereState | (executeAction << 4),           // 17
			0,                                              // 18
			anywhereState | (executeAction << 4),           // 19
			0,                                              // 1A
			0,                                              // 1B
			anywhereState | (executeAction << 4),           // 1C
			anywhereState | (executeAction << 4),           // 1D
			anywhereState | (executeAction << 4),           // 1E
			anywhereState | (executeAction << 4),           // 1F
			escapeIntermediateState | (collectAction << 4), // 20
			escapeIntermediateState | (collectAction << 4), // 21
			escapeIntermediateState | (collectAction << 4), // 22
			escapeIntermediateState | (collectAction << 4), // 23
			escapeIntermediateState | (collectAction << 4), // 24
			escapeIntermediateState | (collectAction << 4), // 25
			escapeIntermediateState | (collectAction << 4), // 26
			escapeIntermediateState | (collectAction << 4), // 27
			escapeIntermediateState | (collectAction << 4), // 28
			escapeIntermediateState | (collectAction << 4), // 29
			escapeIntermediateState | (collectAction << 4), // 2A
			escapeIntermediateState | (collectAction << 4), // 2B
			escapeIntermediateState | (collectAction << 4), // 2C
			escapeIntermediateState | (collectAction << 4), // 2D
			escapeIntermediateState | (collectAction << 4), // 2E
			escapeIntermediateState | (collectAction << 4), // 2F
			groundState | (escDispatchAction << 4),         // 30
			groundState | (escDispatchAction << 4),         // 31
			groundState | (escDispatchAction << 4),         // 32
			groundState | (escDispatchAction << 4),         // 33
			groundState | (escDispatchAction << 4),         // 34
			groundState | (escDispatchAction << 4),         // 35
			groundState | (escDispatchAction << 4),         // 36
			groundState | (escDispatchAction << 4),         // 37
			groundState | (escDispatchAction << 4),         // 38
			groundState | (escDispatchAction << 4),         // 39
			groundState | (escDispatchAction << 4),         // 3A
			groundState | (escDispatchAction << 4),         // 3B
			groundState | (escDispatchAction << 4),         // 3C
			groundState | (escDispatchAction << 4),         // 3D
			groundState | (escDispatchAction << 4),         // 3E
			groundState | (escDispatchAction << 4),         // 3F
			groundState | (escDispatchAction << 4),         // 40
			groundState | (escDispatchAction << 4),         // 41
			groundState | (escDispatchAction << 4),         // 42
			groundState | (escDispatchAction << 4),         // 43
			groundState | (escDispatchAction << 4),         // 44
			groundState | (escDispatchAction << 4),         // 45
			groundState | (escDispatchAction << 4),         // 46
			groundState | (escDispatchAction << 4),         // 47
			groundState | (escDispatchAction << 4),         // 48
			groundState | (escDispatchAction << 4),         // 49
			groundState | (escDispatchAction << 4),         // 4A
			groundState | (escDispatchAction << 4),         // 4B
			groundState | (escDispatchAction << 4),         // 4C
			groundState | (escDispatchAction << 4),         // 4D
			groundState | (escDispatchAction << 4),         // 4E
			groundState | (escDispatchAction << 4),         // 4F
			dcsEntryState | (noneAction << 4),              // 50
			groundState | (escDispatchAction << 4),         // 51
			groundState | (escDispatchAction << 4),         // 52
			groundState | (escDispatchAction << 4),         // 53
			groundState | (escDispatchAction << 4),         // 54
			groundState | (escDispatchAction << 4),         // 55
			groundState | (escDispatchAction << 4),         // 56
			groundState | (escDispatchAction << 4),         // 57
			sosPmApcStringState | (noneAction << 4),        // 58
			groundState | (escDispatchAction << 4),         // 59
			groundState | (escDispatchAction << 4),         // 5A
			csiEntryState | (noneAction << 4),              // 5B
			groundState | (escDispatchAction << 4),         // 5C
			oscStringState | (noneAction << 4),             // 5D
			sosPmApcStringState | (noneAction << 4),        // 5E
			sosPmApcStringState | (noneAction << 4),        // 5F
			groundState | (escDispatchAction << 4),         // 60
			groundState | (escDispatchAction << 4),         // 61
			groundState | (escDispatchAction << 4),         // 62
			groundState | (escDispatchAction << 4),         // 63
			groundState | (escDispatchAction << 4),         // 64
			groundState | (escDispatchAction << 4),         // 65
			groundState | (escDispatchAction << 4),         // 66
			groundState | (escDispatchAction << 4),         // 67
			groundState | (escDispatchAction << 4),         // 68
			groundState | (escDispatchAction << 4),         // 69
			groundState | (escDispatchAction << 4),         // 6A
			groundState | (escDispatchAction << 4),         // 6B
			groundState | (escDispatchAction << 4),         // 6C
			groundState | (escDispatchAction << 4),         // 6D
			groundState | (escDispatchAction << 4),         // 6E
			groundState | (escDispatchAction << 4),         // 6F
			groundState | (escDispatchAction << 4),         // 70
			groundState | (escDispatchAction << 4),         // 71
			groundState | (escDispatchAction << 4),         // 72
			groundState | (escDispatchAction << 4),         // 73
			groundState | (escDispatchAction << 4),         // 74
			groundState | (escDispatchAction << 4),         // 75
			groundState | (escDispatchAction << 4),         // 76
			groundState | (escDispatchAction << 4),         // 77
			groundState | (escDispatchAction << 4),         // 78
			groundState | (escDispatchAction << 4),         // 79
			groundState | (escDispatchAction << 4),         // 7A
			groundState | (escDispatchAction << 4),         // 7B
			groundState | (escDispatchAction << 4),         // 7C
			groundState | (escDispatchAction << 4),         // 7D
			groundState | (escDispatchAction << 4),         // 7E
			anywhereState | (ignoreAction << 4),            // 7F
			0,                                              // 80
			0,                                              // 81
			0,                                              // 82
			0,                                              // 83
			0,                                              // 84
			0,                                              // 85
			0,                                              // 86
			0,                                              // 87
			0,                                              // 88
			0,                                              // 89
			0,                                              // 8A
			0,                                              // 8B
			0,                                              // 8C
			0,                                              // 8D
			0,                                              // 8E
			0,                                              // 8F
			0,                                              // 90
			0,                                              // 91
			0,                                              // 92
			0,                                              // 93
			0,                                              // 94
			0,                                              // 95
			0,                                              // 96
			0,                                              // 97
			0,                                              // 98
			0,                                              // 99
			0,                                              // 9A
			0,                                              // 9B
			0,                                              // 9C
			0,                                              // 9D
			0,                                              // 9E
			0,                                              // 9F
			0,                                              // A0
			0,                                              // A1
			0,                                              // A2
			0,                                              // A3
			0,                                              // A4
			0,                                              // A5
			0,                                              // A6
			0,                                              // A7
			0,                                              // A8
			0,                                              // A9
			0,                                              // AA
			0,                                              // AB
			0,                                              // AC
			0,                                              // AD
			0,                                              // AE
			0,                                              // AF
			0,                                              // B0
			0,                                              // B1
			0,                                              // B2
			0,                                              // B3
			0,                                              // B4
			0,                                              // B5
			0,                                              // B6
			0,                                              // B7
			0,                                              // B8
			0,                                              // B9
			0,                                              // BA
			0,                                              // BB
			0,                                              // BC
			0,                                              // BD
			0,                                              // BE
			0,                                              // BF
			0,                                              // C0
			0,                                              // C1
			0,                                              // C2
			0,                                              // C3
			0,                                              // C4
			0,                                              // C5
			0,                                              // C6
			0,                                              // C7
			0,                                              // C8
			0,                                              // C9
			0,                                              // CA
			0,                                              // CB
			0,                                              // CC
			0,                                              // CD
			0,                                              // CE
			0,                                              // CF
			0,                                              // D0
			0,                                              // D1
			0,                                              // D2
			0,                                              // D3
			0,                                              // D4
			0,                                              // D5
			0,                                              // D6
			0,                                              // D7
			0,                                              // D8
			0,                                              // D9
			0,                                              // DA
			0,                                              // DB
			0,                                              // DC
			0,                                              // DD
			0,                                              // DE
			0,                                              // DF
			0,                                              // E0
			0,                                              // E1
			0,                                              // E2
			0,                                              // E3
			0,                                              // E4
			0,                                              // E5
			0,                                              // E6
			0,                                              // E7
			0,                                              // E8
			0,                                              // E9
			0,                                              // EA
			0,                                              // EB
			0,                                              // EC
			0,                                              // ED
			0,                                              // EE
			0,                                              // EF
			0,                                              // F0
			0,                                              // F1
			0,                                              // F2
			0,                                              // F3
			0,                                              // F4
			0,                                              // F5
			0,                                              // F6
			0,                                              // F7
			0,                                              // F8
			0,                                              // F9
			0,                                              // FA
			0,                                              // FB
			0,                                              // FC
			0,                                              // FD
			0,                                              // FE
			0,                                              // FF
		},
		{ //State escapeIntermediateState = 11
			anywhereState | (executeAction << 4),   // 00
			anywhereState | (executeAction << 4),   // 01
			anywhereState | (executeAction << 4),   // 02
			anywhereState | (executeAction << 4),   // 03
			anywhereState | (executeAction << 4),   // 04
			anywhereState | (executeAction << 4),   // 05
			anywhereState | (executeAction << 4),   // 06
			anywhereState | (executeAction << 4),   // 07
			anywhereState | (executeAction << 4),   // 08
			anywhereState | (executeAction << 4),   // 09
			anywhereState | (executeAction << 4),   // 0A
			anywhereState | (executeAction << 4),   // 0B
			anywhereState | (executeAction << 4),   // 0C
			anywhereState | (executeAction << 4),   // 0D
			anywhereState | (executeAction << 4),   // 0E
			anywhereState | (executeAction << 4),   // 0F
			anywhereState | (executeAction << 4),   // 10
			anywhereState | (executeAction << 4),   // 11
			anywhereState | (executeAction << 4),   // 12
			anywhereState | (executeAction << 4),   // 13
			anywhereState | (executeAction << 4),   // 14
			anywhereState | (executeAction << 4),   // 15
			anywhereState | (executeAction << 4),   // 16
			anywhereState | (executeAction << 4),   // 17
			0,                                      // 18
			anywhereState | (executeAction << 4),   // 19
			0,                                      // 1A
			0,                                      // 1B
			anywhereState | (executeAction << 4),   // 1C
			anywhereState | (executeAction << 4),   // 1D
			anywhereState | (executeAction << 4),   // 1E
			anywhereState | (executeAction << 4),   // 1F
			anywhereState | (collectAction << 4),   // 20
			anywhereState | (collectAction << 4),   // 21
			anywhereState | (collectAction << 4),   // 22
			anywhereState | (collectAction << 4),   // 23
			anywhereState | (collectAction << 4),   // 24
			anywhereState | (collectAction << 4),   // 25
			anywhereState | (collectAction << 4),   // 26
			anywhereState | (collectAction << 4),   // 27
			anywhereState | (collectAction << 4),   // 28
			anywhereState | (collectAction << 4),   // 29
			anywhereState | (collectAction << 4),   // 2A
			anywhereState | (collectAction << 4),   // 2B
			anywhereState | (collectAction << 4),   // 2C
			anywhereState | (collectAction << 4),   // 2D
			anywhereState | (collectAction << 4),   // 2E
			anywhereState | (collectAction << 4),   // 2F
			groundState | (escDispatchAction << 4), // 30
			groundState | (escDispatchAction << 4), // 31
			groundState | (escDispatchAction << 4), // 32
			groundState | (escDispatchAction << 4), // 33
			groundState | (escDispatchAction << 4), // 34
			groundState | (escDispatchAction << 4), // 35
			groundState | (escDispatchAction << 4), // 36
			groundState | (escDispatchAction << 4), // 37
			groundState | (escDispatchAction << 4), // 38
			groundState | (escDispatchAction << 4), // 39
			groundState | (escDispatchAction << 4), // 3A
			groundState | (escDispatchAction << 4), // 3B
			groundState | (escDispatchAction << 4), // 3C
			groundState | (escDispatchAction << 4), // 3D
			groundState | (escDispatchAction << 4), // 3E
			groundState | (escDispatchAction << 4), // 3F
			groundState | (escDispatchAction << 4), // 40
			groundState | (escDispatchAction << 4), // 41
			groundState | (escDispatchAction << 4), // 42
			groundState | (escDispatchAction << 4), // 43
			groundState | (escDispatchAction << 4), // 44
			groundState | (escDispatchAction << 4), // 45
			groundState | (escDispatchAction << 4), // 46
			groundState | (escDispatchAction << 4), // 47
			groundState | (escDispatchAction << 4), // 48
			groundState | (escDispatchAction << 4), // 49
			groundState | (escDispatchAction << 4), // 4A
			groundState | (escDispatchAction << 4), // 4B
			groundState | (escDispatchAction << 4), // 4C
			groundState | (escDispatchAction << 4), // 4D
			groundState | (escDispatchAction << 4), // 4E
			groundState | (escDispatchAction << 4), // 4F
			groundState | (escDispatchAction << 4), // 50
			groundState | (escDispatchAction << 4), // 51
			groundState | (escDispatchAction << 4), // 52
			groundState | (escDispatchAction << 4), // 53
			groundState | (escDispatchAction << 4), // 54
			groundState | (escDispatchAction << 4), // 55
			groundState | (escDispatchAction << 4), // 56
			groundState | (escDispatchAction << 4), // 57
			groundState | (escDispatchAction << 4), // 58
			groundState | (escDispatchAction << 4), // 59
			groundState | (escDispatchAction << 4), // 5A
			groundState | (escDispatchAction << 4), // 5B
			groundState | (escDispatchAction << 4), // 5C
			groundState | (escDispatchAction << 4), // 5D
			groundState | (escDispatchAction << 4), // 5E
			groundState | (escDispatchAction << 4), // 5F
			groundState | (escDispatchAction << 4), // 60
			groundState | (escDispatchAction << 4), // 61
			groundState | (escDispatchAction << 4), // 62
			groundState | (escDispatchAction << 4), // 63
			groundState | (escDispatchAction << 4), // 64
			groundState | (escDispatchAction << 4), // 65
			groundState | (escDispatchAction << 4), // 66
			groundState | (escDispatchAction << 4), // 67
			groundState | (escDispatchAction << 4), // 68
			groundState | (escDispatchAction << 4), // 69
			groundState | (escDispatchAction << 4), // 6A
			groundState | (escDispatchAction << 4), // 6B
			groundState | (escDispatchAction << 4), // 6C
			groundState | (escDispatchAction << 4), // 6D
			groundState | (escDispatchAction << 4), // 6E
			groundState | (escDispatchAction << 4), // 6F
			groundState | (escDispatchAction << 4), // 70
			groundState | (escDispatchAction << 4), // 71
			groundState | (escDispatchAction << 4), // 72
			groundState | (escDispatchAction << 4), // 73
			groundState | (escDispatchAction << 4), // 74
			groundState | (escDispatchAction << 4), // 75
			groundState | (escDispatchAction << 4), // 76
			groundState | (escDispatchAction << 4), // 77
			groundState | (escDispatchAction << 4), // 78
			groundState | (escDispatchAction << 4), // 79
			groundState | (escDispatchAction << 4), // 7A
			groundState | (escDispatchAction << 4), // 7B
			groundState | (escDispatchAction << 4), // 7C
			groundState | (escDispatchAction << 4), // 7D
			groundState | (escDispatchAction << 4), // 7E
			anywhereState | (ignoreAction << 4),    // 7F
			0,                                      // 80
			0,                                      // 81
			0,                                      // 82
			0,                                      // 83
			0,                                      // 84
			0,                                      // 85
			0,                                      // 86
			0,                                      // 87
			0,                                      // 88
			0,                                      // 89
			0,                                      // 8A
			0,                                      // 8B
			0,                                      // 8C
			0,                                      // 8D
			0,                                      // 8E
			0,                                      // 8F
			0,                                      // 90
			0,                                      // 91
			0,                                      // 92
			0,                                      // 93
			0,                                      // 94
			0,                                      // 95
			0,                                      // 96
			0,                                      // 97
			0,                                      // 98
			0,                                      // 99
			0,                                      // 9A
			0,                                      // 9B
			0,                                      // 9C
			0,                                      // 9D
			0,                                      // 9E
			0,                                      // 9F
			0,                                      // A0
			0,                                      // A1
			0,                                      // A2
			0,                                      // A3
			0,                                      // A4
			0,                                      // A5
			0,                                      // A6
			0,                                      // A7
			0,                                      // A8
			0,                                      // A9
			0,                                      // AA
			0,                                      // AB
			0,                                      // AC
			0,                                      // AD
			0,                                      // AE
			0,                                      // AF
			0,                                      // B0
			0,                                      // B1
			0,                                      // B2
			0,                                      // B3
			0,                                      // B4
			0,                                      // B5
			0,                                      // B6
			0,                                      // B7
			0,                                      // B8
			0,                                      // B9
			0,                                      // BA
			0,                                      // BB
			0,                                      // BC
			0,                                      // BD
			0,                                      // BE
			0,                                      // BF
			0,                                      // C0
			0,                                      // C1
			0,                                      // C2
			0,                                      // C3
			0,                                      // C4
			0,                                      // C5
			0,                                      // C6
			0,                                      // C7
			0,                                      // C8
			0,                                      // C9
			0,                                      // CA
			0,                                      // CB
			0,                                      // CC
			0,                                      // CD
			0,                                      // CE
			0,                                      // CF
			0,                                      // D0
			0,                                      // D1
			0,                                      // D2
			0,                                      // D3
			0,                                      // D4
			0,                                      // D5
			0,                                      // D6
			0,                                      // D7
			0,                                      // D8
			0,                                      // D9
			0,                                      // DA
			0,                                      // DB
			0,                                      // DC
			0,                                      // DD
			0,                                      // DE
			0,                                      // DF
			0,                                      // E0
			0,                                      // E1
			0,                                      // E2
			0,                                      // E3
			0,                                      // E4
			0,                                      // E5
			0,                                      // E6
			0,                                      // E7
			0,                                      // E8
			0,                                      // E9
			0,                                      // EA
			0,                                      // EB
			0,                                      // EC
			0,                                      // ED
			0,                                      // EE
			0,                                      // EF
			0,                                      // F0
			0,                                      // F1
			0,                                      // F2
			0,                                      // F3
			0,                                      // F4
			0,                                      // F5
			0,                                      // F6
			0,                                      // F7
			0,                                      // F8
			0,                                      // F9
			0,                                      // FA
			0,                                      // FB
			0,                                      // FC
			0,                                      // FD
			0,                                      // FE
			0,                                      // FF
		},
		{ //State groundState = 12
			anywhereState | (executeAction << 4), // 00
			anywhereState | (executeAction << 4), // 01
			anywhereState | (executeAction << 4), // 02
			anywhereState | (executeAction << 4), // 03
			anywhereState | (executeAction << 4), // 04
			anywhereState | (executeAction << 4), // 05
			anywhereState | (executeAction << 4), // 06
			anywhereState | (executeAction << 4), // 07
			anywhereState | (executeAction << 4), // 08
			anywhereState | (executeAction << 4), // 09
			anywhereState | (executeAction << 4), // 0A
			anywhereState | (executeAction << 4), // 0B
			anywhereState | (executeAction << 4), // 0C
			anywhereState | (executeAction << 4), // 0D
			anywhereState | (executeAction << 4), // 0E
			anywhereState | (executeAction << 4), // 0F
			anywhereState | (executeAction << 4), // 10
			anywhereState | (executeAction << 4), // 11
			anywhereState | (executeAction << 4), // 12
			anywhereState | (executeAction << 4), // 13
			anywhereState | (executeAction << 4), // 14
			anywhereState | (executeAction << 4), // 15
			anywhereState | (executeAction << 4), // 16
			anywhereState | (executeAction << 4), // 17
			0,                                    // 18
			anywhereState | (executeAction << 4), // 19
			0,                                    // 1A
			0,                                    // 1B
			anywhereState | (executeAction << 4), // 1C
			anywhereState | (executeAction << 4), // 1D
			anywhereState | (executeAction << 4), // 1E
			anywhereState | (executeAction << 4), // 1F
			anywhereState | (printAction << 4),   // 20
			anywhereState | (printAction << 4),   // 21
			anywhereState | (printAction << 4),   // 22
			anywhereState | (printAction << 4),   // 23
			anywhereState | (printAction << 4),   // 24
			anywhereState | (printAction << 4),   // 25
			anywhereState | (printAction << 4),   // 26
			anywhereState | (printAction << 4),   // 27
			anywhereState | (printAction << 4),   // 28
			anywhereState | (printAction << 4),   // 29
			anywhereState | (printAction << 4),   // 2A
			anywhereState | (printAction << 4),   // 2B
			anywhereState | (printAction << 4),   // 2C
			anywhereState | (printAction << 4),   // 2D
			anywhereState | (printAction << 4),   // 2E
			anywhereState | (printAction << 4),   // 2F
			anywhereState | (printAction << 4),   // 30
			anywhereState | (printAction << 4),   // 31
			anywhereState | (printAction << 4),   // 32
			anywhereState | (printAction << 4),   // 33
			anywhereState | (printAction << 4),   // 34
			anywhereState | (printAction << 4),   // 35
			anywhereState | (printAction << 4),   // 36
			anywhereState | (printAction << 4),   // 37
			anywhereState | (printAction << 4),   // 38
			anywhereState | (printAction << 4),   // 39
			anywhereState | (printAction << 4),   // 3A
			anywhereState | (printAction << 4),   // 3B
			anywhereState | (printAction << 4),   // 3C
			anywhereState | (printAction << 4),   // 3D
			anywhereState | (printAction << 4),   // 3E
			anywhereState | (printAction << 4),   // 3F
			anywhereState | (printAction << 4),   // 40
			anywhereState | (printAction << 4),   // 41
			anywhereState | (printAction << 4),   // 42
			anywhereState | (printAction << 4),   // 43
			anywhereState | (printAction << 4),   // 44
			anywhereState | (printAction << 4),   // 45
			anywhereState | (printAction << 4),   // 46
			anywhereState | (printAction << 4),   // 47
			anywhereState | (printAction << 4),   // 48
			anywhereState | (printAction << 4),   // 49
			anywhereState | (printAction << 4),   // 4A
			anywhereState | (printAction << 4),   // 4B
			anywhereState | (printAction << 4),   // 4C
			anywhereState | (printAction << 4),   // 4D
			anywhereState | (printAction << 4),   // 4E
			anywhereState | (printAction << 4),   // 4F
			anywhereState | (printAction << 4),   // 50
			anywhereState | (printAction << 4),   // 51
			anywhereState | (printAction << 4),   // 52
			anywhereState | (printAction << 4),   // 53
			anywhereState | (printAction << 4),   // 54
			anywhereState | (printAction << 4),   // 55
			anywhereState | (printAction << 4),   // 56
			anywhereState | (printAction << 4),   // 57
			anywhereState | (printAction << 4),   // 58
			anywhereState | (printAction << 4),   // 59
			anywhereState | (printAction << 4),   // 5A
			anywhereState | (printAction << 4),   // 5B
			anywhereState | (printAction << 4),   // 5C
			anywhereState | (printAction << 4),   // 5D
			anywhereState | (printAction << 4),   // 5E
			anywhereState | (printAction << 4),   // 5F
			anywhereState | (printAction << 4),   // 60
			anywhereState | (printAction << 4),   // 61
			anywhereState | (printAction << 4),   // 62
			anywhereState | (printAction << 4),   // 63
			anywhereState | (printAction << 4),   // 64
			anywhereState | (printAction << 4),   // 65
			anywhereState | (printAction << 4),   // 66
			anywhereState | (printAction << 4),   // 67
			anywhereState | (printAction << 4),   // 68
			anywhereState | (printAction << 4),   // 69
			anywhereState | (printAction << 4),   // 6A
			anywhereState | (printAction << 4),   // 6B
			anywhereState | (printAction << 4),   // 6C
			anywhereState | (printAction << 4),   // 6D
			anywhereState | (printAction << 4),   // 6E
			anywhereState | (printAction << 4),   // 6F
			anywhereState | (printAction << 4),   // 70
			anywhereState | (printAction << 4),   // 71
			anywhereState | (printAction << 4),   // 72
			anywhereState | (printAction << 4),   // 73
			anywhereState | (printAction << 4),   // 74
			anywhereState | (printAction << 4),   // 75
			anywhereState | (printAction << 4),   // 76
			anywhereState | (printAction << 4),   // 77
			anywhereState | (printAction << 4),   // 78
			anywhereState | (printAction << 4),   // 79
			anywhereState | (printAction << 4),   // 7A
			anywhereState | (printAction << 4),   // 7B
			anywhereState | (printAction << 4),   // 7C
			anywhereState | (printAction << 4),   // 7D
			anywhereState | (printAction << 4),   // 7E
			anywhereState | (printAction << 4),   // 7F
			anywhereState | (executeAction << 4), // 80
			anywhereState | (executeAction << 4), // 81
			anywhereState | (executeAction << 4), // 82
			anywhereState | (executeAction << 4), // 83
			anywhereState | (executeAction << 4), // 84
			anywhereState | (executeAction << 4), // 85
			anywhereState | (executeAction << 4), // 86
			anywhereState | (executeAction << 4), // 87
			anywhereState | (executeAction << 4), // 88
			anywhereState | (executeAction << 4), // 89
			anywhereState | (executeAction << 4), // 8A
			anywhereState | (executeAction << 4), // 8B
			anywhereState | (executeAction << 4), // 8C
			anywhereState | (executeAction << 4), // 8D
			anywhereState | (executeAction << 4), // 8E
			anywhereState | (executeAction << 4), // 8F
			0,                                    // 90
			anywhereState | (executeAction << 4), // 91
			anywhereState | (executeAction << 4), // 92
			anywhereState | (executeAction << 4), // 93
			anywhereState | (executeAction << 4), // 94
			anywhereState | (executeAction << 4), // 95
			anywhereState | (executeAction << 4), // 96
			anywhereState | (executeAction << 4), // 97
			anywhereState | (executeAction << 4), // 98
			anywhereState | (executeAction << 4), // 99
			anywhereState | (executeAction << 4), // 9A
			0,                                    // 9B
			anywhereState | (executeAction << 4), // 9C
			0,                                    // 9D
			0,                                    // 9E
			0,                                    // 9F
			0,                                    // A0
			0,                                    // A1
			0,                                    // A2
			0,                                    // A3
			0,                                    // A4
			0,                                    // A5
			0,                                    // A6
			0,                                    // A7
			0,                                    // A8
			0,                                    // A9
			0,                                    // AA
			0,                                    // AB
			0,                                    // AC
			0,                                    // AD
			0,                                    // AE
			0,                                    // AF
			0,                                    // B0
			0,                                    // B1
			0,                                    // B2
			0,                                    // B3
			0,                                    // B4
			0,                                    // B5
			0,                                    // B6
			0,                                    // B7
			0,                                    // B8
			0,                                    // B9
			0,                                    // BA
			0,                                    // BB
			0,                                    // BC
			0,                                    // BD
			0,                                    // BE
			0,                                    // BF
			0,                                    // C0
			0,                                    // C1
			utf8State | (beginUtf8Action << 4),   // C2
			utf8State | (beginUtf8Action << 4),   // C3
			utf8State | (beginUtf8Action << 4),   // C4
			utf8State | (beginUtf8Action << 4),   // C5
			utf8State | (beginUtf8Action << 4),   // C6
			utf8State | (beginUtf8Action << 4),   // C7
			utf8State | (beginUtf8Action << 4),   // C8
			utf8State | (beginUtf8Action << 4),   // C9
			utf8State | (beginUtf8Action << 4),   // CA
			utf8State | (beginUtf8Action << 4),   // CB
			utf8State | (beginUtf8Action << 4),   // CC
			utf8State | (beginUtf8Action << 4),   // CD
			utf8State | (beginUtf8Action << 4),   // CE
			utf8State | (beginUtf8Action << 4),   // CF
			utf8State | (beginUtf8Action << 4),   // D0
			utf8State | (beginUtf8Action << 4),   // D1
			utf8State | (beginUtf8Action << 4),   // D2
			utf8State | (beginUtf8Action << 4),   // D3
			utf8State | (beginUtf8Action << 4),   // D4
			utf8State | (beginUtf8Action << 4),   // D5
			utf8State | (beginUtf8Action << 4),   // D6
			utf8State | (beginUtf8Action << 4),   // D7
			utf8State | (beginUtf8Action << 4),   // D8
			utf8State | (beginUtf8Action << 4),   // D9
			utf8State | (beginUtf8Action << 4),   // DA
			utf8State | (beginUtf8Action << 4),   // DB
			utf8State | (beginUtf8Action << 4),   // DC
			utf8State | (beginUtf8Action << 4),   // DD
			utf8State | (beginUtf8Action << 4),   // DE
			utf8State | (beginUtf8Action << 4),   // DF
			utf8State | (beginUtf8Action << 4),   // E0
			utf8State | (beginUtf8Action << 4),   // E1
			utf8State | (beginUtf8Action << 4),   // E2
			utf8State | (beginUtf8Action << 4),   // E3
			utf8State | (beginUtf8Action << 4),   // E4
			utf8State | (beginUtf8Action << 4),   // E5
			utf8State | (beginUtf8Action << 4),   // E6
			utf8State | (beginUtf8Action << 4),   // E7
			utf8State | (beginUtf8Action << 4),   // E8
			utf8State | (beginUtf8Action << 4),   // E9
			utf8State | (beginUtf8Action << 4),   // EA
			utf8State | (beginUtf8Action << 4),   // EB
			utf8State | (beginUtf8Action << 4),   // EC
			utf8State | (beginUtf8Action << 4),   // ED
			utf8State | (beginUtf8Action << 4),   // EE
			utf8State | (beginUtf8Action << 4),   // EF
			utf8State | (beginUtf8Action << 4),   // F0
			utf8State | (beginUtf8Action << 4),   // F1
			utf8State | (beginUtf8Action << 4),   // F2
			utf8State | (beginUtf8Action << 4),   // F3
			utf8State | (beginUtf8Action << 4),   // F4
			0,                                    // F5
			0,                                    // F6
			0,                                    // F7
			0,                                    // F8
			0,                                    // F9
			0,                                    // FA
			0,                                    // FB
			0,                                    // FC
			0,                                    // FD
			0,                                    // FE
			0,                                    // FF
		},
		{ //State oscStringState = 13
			anywhereState | (ignoreAction << 4), // 00
			anywhereState | (ignoreAction << 4), // 01
			anywhereState | (ignoreAction << 4), // 02
			anywhereState | (ignoreAction << 4), // 03
			anywhereState | (ignoreAction << 4), // 04
			anywhereState | (ignoreAction << 4), // 05
			anywhereState | (ignoreAction << 4), // 06
			groundState | (noneAction << 4),     // 07
			anywhereState | (ignoreAction << 4), // 08
			anywhereState | (ignoreAction << 4), // 09
			anywhereState | (ignoreAction << 4), // 0A
			anywhereState | (ignoreAction << 4), // 0B
			anywhereState | (ignoreAction << 4), // 0C
			anywhereState | (ignoreAction << 4), // 0D
			anywhereState | (ignoreAction << 4), // 0E
			anywhereState | (ignoreAction << 4), // 0F
			anywhereState | (ignoreAction << 4), // 10
			anywhereState | (ignoreAction << 4), // 11
			anywhereState | (ignoreAction << 4), // 12
			anywhereState | (ignoreAction << 4), // 13
			anywhereState | (ignoreAction << 4), // 14
			anywhereState | (ignoreAction << 4), // 15
			anywhereState | (ignoreAction << 4), // 16
			anywhereState | (ignoreAction << 4), // 17
			0,                                   // 18
			anywhereState | (ignoreAction << 4), // 19
			0,                                   // 1A
			0,                                   // 1B
			anywhereState | (ignoreAction << 4), // 1C
			anywhereState | (ignoreAction << 4), // 1D
			anywhereState | (ignoreAction << 4), // 1E
			anywhereState | (ignoreAction << 4), // 1F
			anywhereState | (oscPutAction << 4), // 20
			anywhereState | (oscPutAction << 4), // 21
			anywhereState | (oscPutAction << 4), // 22
			anywhereState | (oscPutAction << 4), // 23
			anywhereState | (oscPutAction << 4), // 24
			anywhereState | (oscPutAction << 4), // 25
			anywhereState | (oscPutAction << 4), // 26
			anywhereState | (oscPutAction << 4), // 27
			anywhereState | (oscPutAction << 4), // 28
			anywhereState | (oscPutAction << 4), // 29
			anywhereState | (oscPutAction << 4), // 2A
			anywhereState | (oscPutAction << 4), // 2B
			anywhereState | (oscPutAction << 4), // 2C
			anywhereState | (oscPutAction << 4), // 2D
			anywhereState | (oscPutAction << 4), // 2E
			anywhereState | (oscPutAction << 4), // 2F
			anywhereState | (oscPutAction << 4), // 30
			anywhereState | (oscPutAction << 4), // 31
			anywhereState | (oscPutAction << 4), // 32
			anywhereState | (oscPutAction << 4), // 33
			anywhereState | (oscPutAction << 4), // 34
			anywhereState | (oscPutAction << 4), // 35
			anywhereState | (oscPutAction << 4), // 36
			anywhereState | (oscPutAction << 4), // 37
			anywhereState | (oscPutAction << 4), // 38
			anywhereState | (oscPutAction << 4), // 39
			anywhereState | (oscPutAction << 4), // 3A
			anywhereState | (oscPutAction << 4), // 3B
			anywhereState | (oscPutAction << 4), // 3C
			anywhereState | (oscPutAction << 4), // 3D
			anywhereState | (oscPutAction << 4), // 3E
			anywhereState | (oscPutAction << 4), // 3F
			anywhereState | (oscPutAction << 4), // 40
			anywhereState | (oscPutAction << 4), // 41
			anywhereState | (oscPutAction << 4), // 42
			anywhereState | (oscPutAction << 4), // 43
			anywhereState | (oscPutAction << 4), // 44
			anywhereState | (oscPutAction << 4), // 45
			anywhereState | (oscPutAction << 4), // 46
			anywhereState | (oscPutAction << 4), // 47
			anywhereState | (oscPutAction << 4), // 48
			anywhereState | (oscPutAction << 4), // 49
			anywhereState | (oscPutAction << 4), // 4A
			anywhereState | (oscPutAction << 4), // 4B
			anywhereState | (oscPutAction << 4), // 4C
			anywhereState | (oscPutAction << 4), // 4D
			anywhereState | (oscPutAction << 4), // 4E
			anywhereState | (oscPutAction << 4), // 4F
			anywhereState | (oscPutAction << 4), // 50
			anywhereState | (oscPutAction << 4), // 51
			anywhereState | (oscPutAction << 4), // 52
			anywhereState | (oscPutAction << 4), // 53
			anywhereState | (oscPutAction << 4), // 54
			anywhereState | (oscPutAction << 4), // 55
			anywhereState | (oscPutAction << 4), // 56
			anywhereState | (oscPutAction << 4), // 57
			anywhereState | (oscPutAction << 4), // 58
			anywhereState | (oscPutAction << 4), // 59
			anywhereState | (oscPutAction << 4), // 5A
			anywhereState | (oscPutAction << 4), // 5B
			anywhereState | (oscPutAction << 4), // 5C
			anywhereState | (oscPutAction << 4), // 5D
			anywhereState | (oscPutAction << 4), // 5E
			anywhereState | (oscPutAction << 4), // 5F
			anywhereState | (oscPutAction << 4), // 60
			anywhereState | (oscPutAction << 4), // 61
			anywhereState | (oscPutAction << 4), // 62
			anywhereState | (oscPutAction << 4), // 63
			anywhereState | (oscPutAction << 4), // 64
			anywhereState | (oscPutAction << 4), // 65
			anywhereState | (oscPutAction << 4), // 66
			anywhereState | (oscPutAction << 4), // 67
			anywhereState | (oscPutAction << 4), // 68
			anywhereState | (oscPutAction << 4), // 69
			anywhereState | (oscPutAction << 4), // 6A
			anywhereState | (oscPutAction << 4), // 6B
			anywhereState | (oscPutAction << 4), // 6C
			anywhereState | (oscPutAction << 4), // 6D
			anywhereState | (oscPutAction << 4), // 6E
			anywhereState | (oscPutAction << 4), // 6F
			anywhereState | (oscPutAction << 4), // 70
			anywhereState | (oscPutAction << 4), // 71
			anywhereState | (oscPutAction << 4), // 72
			anywhereState | (oscPutAction << 4), // 73
			anywhereState | (oscPutAction << 4), // 74
			anywhereState | (oscPutAction << 4), // 75
			anywhereState | (oscPutAction << 4), // 76
			anywhereState | (oscPutAction << 4), // 77
			anywhereState | (oscPutAction << 4), // 78
			anywhereState | (oscPutAction << 4), // 79
			anywhereState | (oscPutAction << 4), // 7A
			anywhereState | (oscPutAction << 4), // 7B
			anywhereState | (oscPutAction << 4), // 7C
			anywhereState | (oscPutAction << 4), // 7D
			anywhereState | (oscPutAction << 4), // 7E
			anywhereState | (oscPutAction << 4), // 7F
			anywhereState | (oscPutAction << 4), // 80
			anywhereState | (oscPutAction << 4), // 81
			anywhereState | (oscPutAction << 4), // 82
			anywhereState | (oscPutAction << 4), // 83
			anywhereState | (oscPutAction << 4), // 84
			anywhereState | (oscPutAction << 4), // 85
			anywhereState | (oscPutAction << 4), // 86
			anywhereState | (oscPutAction << 4), // 87
			anywhereState | (oscPutAction << 4), // 88
			anywhereState | (oscPutAction << 4), // 89
			anywhereState | (oscPutAction << 4), // 8A
			anywhereState | (oscPutAction << 4), // 8B
			anywhereState | (oscPutAction << 4), // 8C
			anywhereState | (oscPutAction << 4), // 8D
			anywhereState | (oscPutAction << 4), // 8E
			anywhereState | (oscPutAction << 4), // 8F
			anywhereState | (oscPutAction << 4), // 90
			anywhereState | (oscPutAction << 4), // 91
			anywhereState | (oscPutAction << 4), // 92
			anywhereState | (oscPutAction << 4), // 93
			anywhereState | (oscPutAction << 4), // 94
			anywhereState | (oscPutAction << 4), // 95
			anywhereState | (oscPutAction << 4), // 96
			anywhereState | (oscPutAction << 4), // 97
			anywhereState | (oscPutAction << 4), // 98
			anywhereState | (oscPutAction << 4), // 99
			anywhereState | (oscPutAction << 4), // 9A
			anywhereState | (oscPutAction << 4), // 9B
			anywhereState | (oscPutAction << 4), // 9C
			anywhereState | (oscPutAction << 4), // 9D
			anywhereState | (oscPutAction << 4), // 9E
			anywhereState | (oscPutAction << 4), // 9F
			anywhereState | (oscPutAction << 4), // A0
			anywhereState | (oscPutAction << 4), // A1
			anywhereState | (oscPutAction << 4), // A2
			anywhereState | (oscPutAction << 4), // A3
			anywhereState | (oscPutAction << 4), // A4
			anywhereState | (oscPutAction << 4), // A5
			anywhereState | (oscPutAction << 4), // A6
			anywhereState | (oscPutAction << 4), // A7
			anywhereState | (oscPutAction << 4), // A8
			anywhereState | (oscPutAction << 4), // A9
			anywhereState | (oscPutAction << 4), // AA
			anywhereState | (oscPutAction << 4), // AB
			anywhereState | (oscPutAction << 4), // AC
			anywhereState | (oscPutAction << 4), // AD
			anywhereState | (oscPutAction << 4), // AE
			anywhereState | (oscPutAction << 4), // AF
			anywhereState | (oscPutAction << 4), // B0
			anywhereState | (oscPutAction << 4), // B1
			anywhereState | (oscPutAction << 4), // B2
			anywhereState | (oscPutAction << 4), // B3
			anywhereState | (oscPutAction << 4), // B4
			anywhereState | (oscPutAction << 4), // B5
			anywhereState | (oscPutAction << 4), // B6
			anywhereState | (oscPutAction << 4), // B7
			anywhereState | (oscPutAction << 4), // B8
			anywhereState | (oscPutAction << 4), // B9
			anywhereState | (oscPutAction << 4), // BA
			anywhereState | (oscPutAction << 4), // BB
			anywhereState | (oscPutAction << 4), // BC
			anywhereState | (oscPutAction << 4), // BD
			anywhereState | (oscPutAction << 4), // BE
			anywhereState | (oscPutAction << 4), // BF
			anywhereState | (oscPutAction << 4), // C0
			anywhereState | (oscPutAction << 4), // C1
			anywhereState | (oscPutAction << 4), // C2
			anywhereState | (oscPutAction << 4), // C3
			anywhereState | (oscPutAction << 4), // C4
			anywhereState | (oscPutAction << 4), // C5
			anywhereState | (oscPutAction << 4), // C6
			anywhereState | (oscPutAction << 4), // C7
			anywhereState | (oscPutAction << 4), // C8
			anywhereState | (oscPutAction << 4), // C9
			anywhereState | (oscPutAction << 4), // CA
			anywhereState | (oscPutAction << 4), // CB
			anywhereState | (oscPutAction << 4), // CC
			anywhereState | (oscPutAction << 4), // CD
			anywhereState | (oscPutAction << 4), // CE
			anywhereState | (oscPutAction << 4), // CF
			anywhereState | (oscPutAction << 4), // D0
			anywhereState | (oscPutAction << 4), // D1
			anywhereState | (oscPutAction << 4), // D2
			anywhereState | (oscPutAction << 4), // D3
			anywhereState | (oscPutAction << 4), // D4
			anywhereState | (oscPutAction << 4), // D5
			anywhereState | (oscPutAction << 4), // D6
			anywhereState | (oscPutAction << 4), // D7
			anywhereState | (oscPutAction << 4), // D8
			anywhereState | (oscPutAction << 4), // D9
			anywhereState | (oscPutAction << 4), // DA
			anywhereState | (oscPutAction << 4), // DB
			anywhereState | (oscPutAction << 4), // DC
			anywhereState | (oscPutAction << 4), // DD
			anywhereState | (oscPutAction << 4), // DE
			anywhereState | (oscPutAction << 4), // DF
			anywhereState | (oscPutAction << 4), // E0
			anywhereState | (oscPutAction << 4), // E1
			anywhereState | (oscPutAction << 4), // E2
			anywhereState | (oscPutAction << 4), // E3
			anywhereState | (oscPutAction << 4), // E4
			anywhereState | (oscPutAction << 4), // E5
			anywhereState | (oscPutAction << 4), // E6
			anywhereState | (oscPutAction << 4), // E7
			anywhereState | (oscPutAction << 4), // E8
			anywhereState | (oscPutAction << 4), // E9
			anywhereState | (oscPutAction << 4), // EA
			anywhereState | (oscPutAction << 4), // EB
			anywhereState | (oscPutAction << 4), // EC
			anywhereState | (oscPutAction << 4), // ED
			anywhereState | (oscPutAction << 4), // EE
			anywhereState | (oscPutAction << 4), // EF
			anywhereState | (oscPutAction << 4), // F0
			anywhereState | (oscPutAction << 4), // F1
			anywhereState | (oscPutAction << 4), // F2
			anywhereState | (oscPutAction << 4), // F3
			anywhereState | (oscPutAction << 4), // F4
			anywhereState | (oscPutAction << 4), // F5
			anywhereState | (oscPutAction << 4), // F6
			anywhereState | (oscPutAction << 4), // F7
			anywhereState | (oscPutAction << 4), // F8
			anywhereState | (oscPutAction << 4), // F9
			anywhereState | (oscPutAction << 4), // FA
			anywhereState | (oscPutAction << 4), // FB
			anywhereState | (oscPutAction << 4), // FC
			anywhereState | (oscPutAction << 4), // FD
			anywhereState | (oscPutAction << 4), // FE
			anywhereState | (oscPutAction << 4), // FF
		},
		{ //State sosPmApcStringState = 14
			anywhereState | (ignoreAction << 4), // 00
			anywhereState | (ignoreAction << 4), // 01
			anywhereState | (ignoreAction << 4), // 02
			anywhereState | (ignoreAction << 4), // 03
			anywhereState | (ignoreAction << 4), // 04
			anywhereState | (ignoreAction << 4), // 05
			anywhereState | (ignoreAction << 4), // 06
			anywhereState | (ignoreAction << 4), // 07
			anywhereState | (ignoreAction << 4), // 08
			anywhereState | (ignoreAction << 4), // 09
			anywhereState | (ignoreAction << 4), // 0A
			anywhereState | (ignoreAction << 4), // 0B
			anywhereState | (ignoreAction << 4), // 0C
			anywhereState | (ignoreAction << 4), // 0D
			anywhereState | (ignoreAction << 4), // 0E
			anywhereState | (ignoreAction << 4), // 0F
			anywhereState | (ignoreAction << 4), // 10
			anywhereState | (ignoreAction << 4), // 11
			anywhereState | (ignoreAction << 4), // 12
			anywhereState | (ignoreAction << 4), // 13
			anywhereState | (ignoreAction << 4), // 14
			anywhereState | (ignoreAction << 4), // 15
			anywhereState | (ignoreAction << 4), // 16
			anywhereState | (ignoreAction << 4), // 17
			0,                                   // 18
			anywhereState | (ignoreAction << 4), // 19
			0,                                   // 1A
			0,                                   // 1B
			anywhereState | (ignoreAction << 4), // 1C
			anywhereState | (ignoreAction << 4), // 1D
			anywhereState | (ignoreAction << 4), // 1E
			anywhereState | (ignoreAction << 4), // 1F
			anywhereState | (ignoreAction << 4), // 20
			anywhereState | (ignoreAction << 4), // 21
			anywhereState | (ignoreAction << 4), // 22
			anywhereState | (ignoreAction << 4), // 23
			anywhereState | (ignoreAction << 4), // 24
			anywhereState | (ignoreAction << 4), // 25
			anywhereState | (ignoreAction << 4), // 26
			anywhereState | (ignoreAction << 4), // 27
			anywhereState | (ignoreAction << 4), // 28
			anywhereState | (ignoreAction << 4), // 29
			anywhereState | (ignoreAction << 4), // 2A
			anywhereState | (ignoreAction << 4), // 2B
			anywhereState | (ignoreAction << 4), // 2C
			anywhereState | (ignoreAction << 4), // 2D
			anywhereState | (ignoreAction << 4), // 2E
			anywhereState | (ignoreAction << 4), // 2F
			anywhereState | (ignoreAction << 4), // 30
			anywhereState | (ignoreAction << 4), // 31
			anywhereState | (ignoreAction << 4), // 32
			anywhereState | (ignoreAction << 4), // 33
			anywhereState | (ignoreAction << 4), // 34
			anywhereState | (ignoreAction << 4), // 35
			anywhereState | (ignoreAction << 4), // 36
			anywhereState | (ignoreAction << 4), // 37
			anywhereState | (ignoreAction << 4), // 38
			anywhereState | (ignoreAction << 4), // 39
			anywhereState | (ignoreAction << 4), // 3A
			anywhereState | (ignoreAction << 4), // 3B
			anywhereState | (ignoreAction << 4), // 3C
			anywhereState | (ignoreAction << 4), // 3D
			anywhereState | (ignoreAction << 4), // 3E
			anywhereState | (ignoreAction << 4), // 3F
			anywhereState | (ignoreAction << 4), // 40
			anywhereState | (ignoreAction << 4), // 41
			anywhereState | (ignoreAction << 4), // 42
			anywhereState | (ignoreAction << 4), // 43
			anywhereState | (ignoreAction << 4), // 44
			anywhereState | (ignoreAction << 4), // 45
			anywhereState | (ignoreAction << 4), // 46
			anywhereState | (ignoreAction << 4), // 47
			anywhereState | (ignoreAction << 4), // 48
			anywhereState | (ignoreAction << 4), // 49
			anywhereState | (ignoreAction << 4), // 4A
			anywhereState | (ignoreAction << 4), // 4B
			anywhereState | (ignoreAction << 4), // 4C
			anywhereState | (ignoreAction << 4), // 4D
			anywhereState | (ignoreAction << 4), // 4E
			anywhereState | (ignoreAction << 4), // 4F
			anywhereState | (ignoreAction << 4), // 50
			anywhereState | (ignoreAction << 4), // 51
			anywhereState | (ignoreAction << 4), // 52
			anywhereState | (ignoreAction << 4), // 53
			anywhereState | (ignoreAction << 4), // 54
			anywhereState | (ignoreAction << 4), // 55
			anywhereState | (ignoreAction << 4), // 56
			anywhereState | (ignoreAction << 4), // 57
			anywhereState | (ignoreAction << 4), // 58
			anywhereState | (ignoreAction << 4), // 59
			anywhereState | (ignoreAction << 4), // 5A
			anywhereState | (ignoreAction << 4), // 5B
			anywhereState | (ignoreAction << 4), // 5C
			anywhereState | (ignoreAction << 4), // 5D
			anywhereState | (ignoreAction << 4), // 5E
			anywhereState | (ignoreAction << 4), // 5F
			anywhereState | (ignoreAction << 4), // 60
			anywhereState | (ignoreAction << 4), // 61
			anywhereState | (ignoreAction << 4), // 62
			anywhereState | (ignoreAction << 4), // 63
			anywhereState | (ignoreAction << 4), // 64
			anywhereState | (ignoreAction << 4), // 65
			anywhereState | (ignoreAction << 4), // 66
			anywhereState | (ignoreAction << 4), // 67
			anywhereState | (ignoreAction << 4), // 68
			anywhereState | (ignoreAction << 4), // 69
			anywhereState | (ignoreAction << 4), // 6A
			anywhereState | (ignoreAction << 4), // 6B
			anywhereState | (ignoreAction << 4), // 6C
			anywhereState | (ignoreAction << 4), // 6D
			anywhereState | (ignoreAction << 4), // 6E
			anywhereState | (ignoreAction << 4), // 6F
			anywhereState | (ignoreAction << 4), // 70
			anywhereState | (ignoreAction << 4), // 71
			anywhereState | (ignoreAction << 4), // 72
			anywhereState | (ignoreAction << 4), // 73
			anywhereState | (ignoreAction << 4), // 74
			anywhereState | (ignoreAction << 4), // 75
			anywhereState | (ignoreAction << 4), // 76
			anywhereState | (ignoreAction << 4), // 77
			anywhereState | (ignoreAction << 4), // 78
			anywhereState | (ignoreAction << 4), // 79
			anywhereState | (ignoreAction << 4), // 7A
			anywhereState | (ignoreAction << 4), // 7B
			anywhereState | (ignoreAction << 4), // 7C
			anywhereState | (ignoreAction << 4), // 7D
			anywhereState | (ignoreAction << 4), // 7E
			anywhereState | (ignoreAction << 4), // 7F
			0,                                   // 80
			0,                                   // 81
			0,                                   // 82
			0,                                   // 83
			0,                                   // 84
			0,                                   // 85
			0,                                   // 86
			0,                                   // 87
			0,                                   // 88
			0,                                   // 89
			0,                                   // 8A
			0,                                   // 8B
			0,                                   // 8C
			0,                                   // 8D
			0,                                   // 8E
			0,                                   // 8F
			0,                                   // 90
			0,                                   // 91
			0,                                   // 92
			0,                                   // 93
			0,                                   // 94
			0,                                   // 95
			0,                                   // 96
			0,                                   // 97
			0,                                   // 98
			0,                                   // 99
			0,                                   // 9A
			0,                                   // 9B
			groundState | (noneAction << 4),     // 9C
			0,                                   // 9D
			0,                                   // 9E
			0,                                   // 9F
			0,                                   // A0
			0,                                   // A1
			0,                                   // A2
			0,                                   // A3
			0,                                   // A4
			0,                                   // A5
			0,                                   // A6
			0,                                   // A7
			0,                                   // A8
			0,                                   // A9
			0,                                   // AA
			0,                                   // AB
			0,                                   // AC
			0,                                   // AD
			0,                                   // AE
			0,                                   // AF
			0,                                   // B0
			0,                                   // B1
			0,                                   // B2
			0,                                   // B3
			0,                                   // B4
			0,                                   // B5
			0,                                   // B6
			0,                                   // B7
			0,                                   // B8
			0,                                   // B9
			0,                                   // BA
			0,                                   // BB
			0,                                   // BC
			0,                                   // BD
			0,                                   // BE
			0,                                   // BF
			0,                                   // C0
			0,                                   // C1
			0,                                   // C2
			0,                                   // C3
			0,                                   // C4
			0,                                   // C5
			0,                                   // C6
			0,                                   // C7
			0,                                   // C8
			0,                                   // C9
			0,                                   // CA
			0,                                   // CB
			0,                                   // CC
			0,                                   // CD
			0,                                   // CE
			0,                                   // CF
			0,                                   // D0
			0,                                   // D1
			0,                                   // D2
			0,                                   // D3
			0,                                   // D4
			0,                                   // D5
			0,                                   // D6
			0,                                   // D7
			0,                                   // D8
			0,                                   // D9
			0,                                   // DA
			0,                                   // DB
			0,                                   // DC
			0,                                   // DD
			0,                                   // DE
			0,                                   // DF
			0,                                   // E0
			0,                                   // E1
			0,                                   // E2
			0,                                   // E3
			0,                                   // E4
			0,                                   // E5
			0,                                   // E6
			0,                                   // E7
			0,                                   // E8
			0,                                   // E9
			0,                                   // EA
			0,                                   // EB
			0,                                   // EC
			0,                                   // ED
			0,                                   // EE
			0,                                   // EF
			0,                                   // F0
			0,                                   // F1
			0,                                   // F2
			0,                                   // F3
			0,                                   // F4
			0,                                   // F5
			0,                                   // F6
			0,                                   // F7
			0,                                   // F8
			0,                                   // F9
			0,                                   // FA
			0,                                   // FB
			0,                                   // FC
			0,                                   // FD
			0,                                   // FE
			0,                                   // FF
		},
	}

	entryActions = []byte{
		0,              //anywhereState
		clearAction,    //csiEntryState
		0,              //csiIgnoreState
		0,              //csiIntermediateState
		0,              //csiParamState
		clearAction,    //dcsEntryState
		0,              //dcsIgnoreState
		0,              //dcsIntermediateState
		0,              //dcsParamState
		hookAction,     //dcsPassthroughState
		clearAction,    //escapeState
		0,              //escapeIntermediateState
		0,              //groundState
		oscStartAction, //oscStringState
		0,              //sosPmApcStringState
		0,              //utf8State
	}

	exitActions = []byte{
		0,            //anywhereState
		0,            //csiEntryState
		0,            //csiIgnoreState
		0,            //csiIntermediateState
		0,            //csiParamState
		0,            //dcsEntryState
		0,            //dcsIgnoreState
		0,            //dcsIntermediateState
		0,            //dcsParamState
		unhookAction, //dcsPassthroughState
		0,            //escapeState
		0,            //escapeIntermediateState
		0,            //groundState
		oscEndAction, //oscStringState
		0,            //sosPmApcStringState
		0,            //utf8State
	}
)
