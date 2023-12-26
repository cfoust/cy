package fuzzy

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
)

var pokemon []Option = []Option{
	NewOption("Bulbasaur", nil),
	NewOption("Ivysaur", nil),
	NewOption("Venusaur", nil),
	NewOption("Charmander", nil),
	NewOption("Charmeleon", nil),
	NewOption("Charizard", nil),
	NewOption("Squirtle", nil),
	NewOption("Wartortle", nil),
	NewOption("Blastoise", nil),
	NewOption("Caterpie", nil),
	NewOption("Metapod", nil),
	NewOption("Butterfree", nil),
	NewOption("Weedle", nil),
	NewOption("Kakuna", nil),
	NewOption("Beedrill", nil),
	NewOption("Pidgey", nil),
	NewOption("Pidgeotto", nil),
	NewOption("Pidgeot", nil),
	NewOption("Rattata", nil),
	NewOption("Raticate", nil),
	NewOption("Spearow", nil),
	NewOption("Fearow", nil),
	NewOption("Ekans", nil),
	NewOption("Arbok", nil),
	NewOption("Pikachu", nil),
	NewOption("Raichu", nil),
	NewOption("Sandshrew", nil),
	NewOption("Sandslash", nil),
	NewOption("Nidoran♀", nil),
	NewOption("Nidorina", nil),
	NewOption("Nidoqueen", nil),
	NewOption("Nidoran♂", nil),
	NewOption("Nidorino", nil),
	NewOption("Nidoking", nil),
	NewOption("Clefairy", nil),
	NewOption("Clefable", nil),
	NewOption("Vulpix", nil),
	NewOption("Ninetales", nil),
	NewOption("Jigglypuff", nil),
	NewOption("Wigglytuff", nil),
	NewOption("Zubat", nil),
	NewOption("Golbat", nil),
	NewOption("Oddish", nil),
	NewOption("Gloom", nil),
	NewOption("Vileplume", nil),
	NewOption("Paras", nil),
	NewOption("Parasect", nil),
	NewOption("Venonat", nil),
	NewOption("Venomoth", nil),
	NewOption("Diglett", nil),
	NewOption("Dugtrio", nil),
	NewOption("Meowth", nil),
	NewOption("Persian", nil),
	NewOption("Psyduck", nil),
	NewOption("Golduck", nil),
	NewOption("Mankey", nil),
	NewOption("Primeape", nil),
	NewOption("Growlithe", nil),
	NewOption("Arcanine", nil),
	NewOption("Poliwag", nil),
	NewOption("Poliwhirl", nil),
	NewOption("Poliwrath", nil),
	NewOption("Abra", nil),
	NewOption("Kadabra", nil),
	NewOption("Alakazam", nil),
	NewOption("Machop", nil),
	NewOption("Machoke", nil),
	NewOption("Machamp", nil),
	NewOption("Bellsprout", nil),
	NewOption("Weepinbell", nil),
	NewOption("Victreebel", nil),
	NewOption("Tentacool", nil),
	NewOption("Tentacruel", nil),
	NewOption("Geodude", nil),
	NewOption("Graveler", nil),
	NewOption("Golem", nil),
	NewOption("Ponyta", nil),
	NewOption("Rapidash", nil),
	NewOption("Slowpoke", nil),
	NewOption("Slowbro", nil),
	NewOption("Magnemite", nil),
	NewOption("Magneton", nil),
	NewOption("Farfetch'd", nil),
	NewOption("Doduo", nil),
	NewOption("Dodrio", nil),
	NewOption("Seel", nil),
	NewOption("Dewgong", nil),
	NewOption("Grimer", nil),
	NewOption("Muk", nil),
	NewOption("Shellder", nil),
	NewOption("Cloyster", nil),
	NewOption("Gastly", nil),
	NewOption("Haunter", nil),
	NewOption("Gengar", nil),
	NewOption("Onix", nil),
	NewOption("Drowzee", nil),
	NewOption("Hypno", nil),
	NewOption("Krabby", nil),
	NewOption("Kingler", nil),
	NewOption("Voltorb", nil),
	NewOption("Electrode", nil),
	NewOption("Exeggcute", nil),
	NewOption("Exeggutor", nil),
	NewOption("Cubone", nil),
	NewOption("Marowak", nil),
	NewOption("Hitmonlee", nil),
	NewOption("Hitmonchan", nil),
	NewOption("Lickitung", nil),
	NewOption("Koffing", nil),
	NewOption("Weezing", nil),
	NewOption("Rhyhorn", nil),
	NewOption("Rhydon", nil),
	NewOption("Chansey", nil),
	NewOption("Tangela", nil),
	NewOption("Kangaskhan", nil),
	NewOption("Horsea", nil),
	NewOption("Seadra", nil),
	NewOption("Goldeen", nil),
	NewOption("Seaking", nil),
	NewOption("Staryu", nil),
	NewOption("Starmie", nil),
	NewOption("Mr. Mime", nil),
	NewOption("Scyther", nil),
	NewOption("Jynx", nil),
	NewOption("Electabuzz", nil),
	NewOption("Magmar", nil),
	NewOption("Pinsir", nil),
	NewOption("Tauros", nil),
	NewOption("Magikarp", nil),
	NewOption("Gyarados", nil),
	NewOption("Lapras", nil),
	NewOption("Ditto", nil),
	NewOption("Eevee", nil),
	NewOption("Vaporeon", nil),
	NewOption("Jolteon", nil),
	NewOption("Flareon", nil),
	NewOption("Porygon", nil),
	NewOption("Omanyte", nil),
	NewOption("Omastar", nil),
	NewOption("Kabuto", nil),
	NewOption("Kabutops", nil),
	NewOption("Aerodactyl", nil),
	NewOption("Snorlax", nil),
	NewOption("Articuno", nil),
	NewOption("Zapdos", nil),
	NewOption("Moltres", nil),
	NewOption("Dratini", nil),
	NewOption("Dragonair", nil),
	NewOption("Dragonite", nil),
	NewOption("Mewtwo", nil),
	NewOption("Mew", nil),
}

var BottomRight stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	location := geom.DEFAULT_SIZE
	location.C -= 1
	location.R -= 1
	return NewFuzzy(
		ctx,
		pokemon,
		WithInline(location, geom.DEFAULT_SIZE),
	), nil
}

var TopLeft stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	return NewFuzzy(
		ctx,
		pokemon,
		WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	), nil
}

var Search stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := NewFuzzy(
		ctx,
		pokemon,
		WithInline(geom.Size{}, geom.DEFAULT_SIZE),
	)

	stories.Send(f, "Pid")
	return f, nil
}

var FullTop stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := NewFuzzy(
		ctx,
		pokemon,
		WithReverse,
	)
	return f, nil
}

var FullBottom stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	f := NewFuzzy(
		ctx,
		pokemon,
	)
	return f, nil
}

func init() {
	config := stories.Config{
		Size: geom.DEFAULT_SIZE,
		Input: []interface{}{
			"B",
			stories.Wait(time.Second),
			"u",
			stories.Wait(time.Second),
			"t",
			stories.Wait(time.Second),
		},
	}
	stories.Register("input/find/bottom-right", BottomRight, config)
	stories.Register("input/find/top-left", TopLeft, config)
	stories.Register("input/find/search", Search, config)
	stories.Register("input/find/full-top", FullTop, config)
	stories.Register("input/find/full-bottom", FullBottom, config)
}
