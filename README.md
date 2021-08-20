# PlanetaryDefense2012
"Planetary Defense" game for the Atari 8-bit computers, revised for 2012

In Planetary Defense, you control an orbiting defense platform that is defending your planet against incoming Planet Bombs. If the bombs reach the surface of your planet, they will explode and destroy an area of the planet with an expanding plasma cloud.  If the bombs erode the planet to the point where the inner core is exposed, the planet is lost and the game is over.

You defend the planet by firing missiles to intercept the Planet Bombs and explode them in space, before they damage the planet. You cannot control the orbit of your platform, and if the planet is between your platform and an incoming bomb, firing a missile will hit the planet and you will damage it instead of protecting it -- so you must take care in targeting.

In addition to the bombs, occasionally an enemy flying saucer will attack, firing missiles at the planet.  These saucers must be stopped quickly, as they can inflict a lot of damage as they quickly fire missiles at the planet core.

There are 99 levels of difficulty.  Each level gets progressively more difficult; there are more incoming bombs and saucers, they move faster and the saucers fire more quickly.

The original Planetary Defense was written in 1982 for the Atari 400 and was constrained to run in 16K of memory.  As such, functionality was limited and certain features that had been considered could not be added.

In 2012, the game was revised to run under the "Colleen" emulator for Android devices. The port was a natural, since the touch screen interface allows direct "tap" input to target missiles.

Added features in the 2012 version include more levels and a periodic bonus: "Terraforming" that will restore damaged areas of the planet, prolonging the game action for skilled players.

The final added feature for the 2012 version is an Internet-based Leaderboard.  The Colleen emulator added a "B:" device, shorthand for "Browser", which allows software to query URLs.  Planetary Defense 2012 uses this functionality to send out queries to the analog.klanky.com website, which then adds the scores to a leaderboard database for display.

The assembly source code uses the Mac/65 macro assembler syntax; see the ATasm website for the cross-assembler: https://atari.miribilist.com/atasm/

